{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- Module      : Gen.Stage2
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.Stage2 where

import           Control.Applicative
import           Control.Error
import           Control.Lens             hiding ((.=), (<.>), op)
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.Aeson.Types         (Pair)
import           Data.Bifunctor
import qualified Data.ByteString.Lazy     as LBS
import           Data.CaseInsensitive     (CI)
import qualified Data.CaseInsensitive     as CI
import           Data.Char
import           Data.Function            (on)
import           Data.HashMap.Strict      (HashMap)
import qualified Data.HashMap.Strict      as Map
import           Data.List
import           Data.Monoid              hiding (Product)
import           Data.SemVer
import           Data.String
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           Data.Text.Manipulate
import           Gen.IO
import           Gen.JSON              ()
import           Gen.Names
import           Gen.TH
import           Gen.Types
import           System.FilePath

default (Text, FilePath)

newtype Exposed  a = Exposed  a
newtype Internal a = Internal a

newtype Doc = Doc Text
    deriving (Eq, Ord, Show, ToJSON, IsString)

documentation :: Maybe Text -> Doc
documentation = Doc . fromMaybe ""

rewrap :: Pair -> Value -> Value
rewrap (k, v) = Object . \case
    Object o -> Map.insert    k v o
    _        -> Map.singleton k v

data Derive
    = Eq'
    | Ord'
    | Show'
    | Generic'
    | Enum'
    | Num'
    | Monoid'
    | Semigroup'
      deriving (Eq, Ord, Show)

instance ToJSON Derive where
    toJSON = toJSON . takeWhile (/= '\'') . show

class DerivingOf a where
    derivingOf :: a -> [Derive]

instance DerivingOf a => DerivingOf [a] where
    derivingOf xs =
        case map derivingOf xs of
            []   -> []
            y:ys -> sort (foldl' intersect y ys)

data Derived a = Derived !a !Value

instance (DerivingOf a, ToJSON a) => ToJSON (Derived a) where
    toJSON (Derived x v) = rewrap ("deriving", toJSON (derivingOf x)) v

class HasName a where
    nameOf :: Lens' a Text

instance HasName Text where
    nameOf = id

nameCI :: HasName a => a -> CI Text
nameCI = CI.mk . view nameOf

constructor :: HasName a => a -> Text
constructor = reserved
    . uncurry (<>)
    . first Text.toLower
    . breakWord
    . view nameOf

data Prim
    = PBlob
    | PReq
    | PRes
    | PBool
    | PText
    | PInt
    | PInteger
    | PDouble
    | PTime !Timestamp
      deriving (Eq, Ord, Show)

-- | Primitives are rendered according to their unwrapped Iso' mappings.
primitive :: Bool -> Prim -> Text
primitive int = \case
    PBlob                -> "LazyByteString"
    PReq                 -> "RqBody"
    PRes                 -> "RsBody"
    -- PBool | int          -> "Boolean"
          -- | otherwise    -> "Bool"
    PBool                -> "Bool"
    PText                -> "Text"
    PInt                 -> "Int"
    PInteger             -> "Integer"
    PDouble              -> "Double"
    PTime ts | int       -> timestamp ts
             | otherwise -> "UTCTime"

instance DerivingOf Prim where
    derivingOf = (def ++) . \case
        PBool    -> [Eq', Ord', Enum']
        PText    -> [Eq', Ord', Monoid']
        PInt     -> [Eq', Ord', Num', Enum']
        PInteger -> [Eq', Ord', Num', Enum']
        PDouble  -> [Eq', Ord', Num', Enum']
        PTime _  -> [Eq', Ord']
        PBlob    -> [Eq']
        _        -> []
      where
        def = [Show', Generic']

data Type
    = TType      !Text
    | TPrim      !Prim
    | TList      !Type
    | TList1     !Type
    | TMap       !Type !Type
    | TMaybe     !Type
    | TSensitive !Type
      deriving (Eq, Ord, Show)

isRequired :: Type -> Bool
isRequired (TMaybe _) = False
isRequired _          = True

isMonoid :: Type -> Bool
isMonoid (TList  _) = True
isMonoid (TMap _ _) = True
isMonoid _          = False

instance Plated Type where
    plate f = \case
        TList      x   -> TList      <$> f x
        TList1     x   -> TList1     <$> f x
        TMap       k v -> TMap       <$> f k <*> f v
        TMaybe     x   -> TMaybe     <$> f x
        TSensitive x   -> TSensitive <$> f x
        x              -> pure x

instance ToJSON (Exposed Type) where
    toJSON (Exposed e) = toJSON (go e)
      where
        go = \case
            TType      t   -> t
            TPrim      p   -> primitive False p
            TList      x   -> "["          <> wrap (go x) <> "]"
            TList1     x   -> "NonEmpty "  <> wrap (go x)
            TMap       k v -> "HashMap "   <> wrap (go k) <> " " <> wrap (go v)
            TMaybe     x   -> "Maybe "     <> wrap (go x)
            TSensitive x   -> wrap (go x)

        wrap   t = maybe t (const (parens t)) (Text.findIndex isSpace t)
        parens t = "(" <> t <> ")"

instance ToJSON (Internal Type) where
    toJSON (Internal i) = toJSON (go i)
      where
        go = \case
            TType      t   -> t
            TPrim      p   -> primitive True p
            TList      x   -> "["          <> wrap (go x) <> "]"
            TList1     x   -> "List1 "     <> wrap (go x)
            TMap       k v -> "Map "       <> wrap (go k) <> " " <> wrap (go v)
            TMaybe     x   -> "Maybe "     <> wrap (go x)
            TSensitive x   -> "Sensitive " <> wrap (go x)

        wrap   t = maybe t (const (parens t)) (Text.findIndex isSpace t)
        parens t = "(" <> t <> ")"

typeMapping :: Type -> Maybe Text
typeMapping t
    | x:xs <- go t = Just (Text.intercalate " . " (x : xs))
    | otherwise    = Nothing
  where
    go y = case y of
        TType      _   -> []
        TPrim      p   -> maybeToList (primIso p)
        TList      _   -> []
        TList1     _   -> maybeToList (typeIso y)
        TMap       _ _ -> maybeToList (typeIso y)
        TMaybe     x   -> wrap (go x)
        TSensitive x   -> maybeToList (typeIso y) ++ go x

    wrap []     = []
    wrap (x:xs) = "mapping " <> x : xs

typeIso :: Type -> Maybe Text
typeIso = \case
    TPrim      p   -> primIso p
    TList1     _   -> Just "_List1"
    TMap       _ _ -> Just "_Map"
    TSensitive _   -> Just "_Sensitive"
    _              -> Nothing

primIso :: Prim -> Maybe Text
primIso = \case
    PTime _ -> Just "_Time"
--    PBool   -> Just "_Boolean"
    _       -> Nothing

typeDefault :: Type -> Text
typeDefault t
    | isMonoid   t = "mempty"
    | isRequired t = "<error>"
    | otherwise    = "Nothing"

class HasType a where
    typeOf :: Lens' a Type

typesOf :: HasType a => Traversal' a Type
typesOf = typeOf . go
  where
    go f = \case
        TList      x   -> TList      <$> f x
        TList1     x   -> TList1     <$> f x
        TMap       k v -> TMap       <$> f k <*> f v
        TMaybe     x   -> TMaybe     <$> f x
        TSensitive x   -> TSensitive <$> f x
        t              -> pure t

instance DerivingOf Type where
    derivingOf = \case
        TType      _   -> [Eq', Show', Generic']
        TPrim      p   -> derivingOf p
        TList      x   -> Monoid'    : derivingOf x
        TList1     x   -> Semigroup' : derivingOf x
        TMap       k v -> Monoid'    : delete Ord' (derivingOf k `intersect` derivingOf v)
        TMaybe     x   -> delete Enum' . delete Num' $ derivingOf x
        TSensitive x   -> derivingOf x

data Field = Field
    { _fName          :: !Text
    , _fShape         :: !Text
    , _fType          :: !Type
    , _fLocation      :: !Location
    , _fLocationName  :: !Text
    , _fDocumentation :: Maybe Doc
    } deriving (Eq, Show)

instance Ord Field where
    compare a b = on compare _fName a b <> on compare _fType a b

makeLenses ''Field

instance ToJSON Field where
    toJSON Field{..} = object
        [ "name"          .= fieldName _fName
        , "lens"          .= lensName  _fName
        , "lensMapping"   .= typeMapping _fType
        , "shape"         .= _fShape
        , "type"          .= Internal _fType
        , "typeExposed"   .= Exposed _fType
        , "location"      .= _fLocation
        , "locationName"  .= _fLocationName
        , "documentation" .= _fDocumentation
        , "required"      .= isRequired _fType
        , "default"       .= typeDefault _fType
        , "iso"           .= typeIso _fType
        ]

parameters :: [Field] -> ([Field], [Field])
parameters = partition (f . view typeOf)
  where
    f x = not (isMonoid x) && isRequired x

isHeader :: Field -> Bool
isHeader = f . view fLocation
  where
    f Header  = True
    f Headers = True
    f _       = False

isQuery :: Field -> Bool
isQuery = f . view fLocation
  where
    f Querystring = True
    f _           = False

isPayload :: Field -> Bool
isPayload f =
    case _fLocation f of
        BodyXml  -> True
        BodyJson -> True
        Body     -> True
        _        -> False

instance HasName Field where
    nameOf = fName

instance HasType Field where
    typeOf = fType

data Data
    = Nullary !Text (HashMap Text Text)
    | Newtype !Text !Field
    | Record  !Text [Field]
    | Product !Text [Type]
    | Empty   !Text
    | Void
      deriving (Eq, Show)

instance Ord Data where
    compare a b =
        case (a, b) of
            (Nullary x _, Nullary y _) -> x `compare` y
            (Newtype x _, Newtype y _) -> x `compare` y
            (Record  x _, Record  y _) -> x `compare` y
            (Nullary _ _, _)           -> LT
            (Newtype _ _, _)           -> LT
            (Record  _ _, _)           -> LT
            _                          -> GT

instance ToJSON Data where
    toJSON d = toJSON . Derived d $
        case d of
            Void         -> object
                [ "type"      .= "void"
                ]

            Empty   n    -> object
                [ "type"      .= "empty"
                , "name"      .= n
                , "ctor"      .= constructor n
                , "fieldPad"  .= (0 :: Int)
                , "fields"    .= ([] :: [Text])
                , "optional"  .= ([] :: [Text])
                , "required"  .= ([] :: [Text])
                ]

            Product n fs -> object
                [ "type"      .= "product"
                , "name"      .= n
                , "slots"     .= map Internal fs
                ]

            Nullary n fs -> object
                [ "type"      .= "nullary"
                , "name"      .= n
                , "branches"  .= fs
                , "branchPad" .= maximum (map Text.length $ Map.keys fs)
                , "valuePad"  .= (maximum (map Text.length $ Map.elems fs) + 1)
                ]

            Newtype n f  -> object
                [ "type"     .= "newtype"
                , "name"     .= n
                , "ctor"     .= constructor n
                , "field"    .= f
                , "fields"   .= [f]
                , "fieldPad" .= (0 :: Int)
                , "required" .= req
                , "optional" .= opt
                , "payload"  .= pay
                ]
              where
                (req, opt) = parameters [f]
                pay        = find isPayload [f]

            Record  n fs -> object
                [ "type"     .= "record"
                , "name"     .= n
                , "ctor"     .= constructor n
                , "fields"   .= sort fs
                , "fieldPad" .= (maximum (map (Text.length . view nameOf) fs) + 1)
                , "required" .= req
                , "optional" .= opt
                , "payload"  .= pay
                ]
              where
                (req, opt) = parameters fs
                pay        = find isPayload fs

-- instance HasName Data where
--     nameOf f = \case
--         Newtype n x  -> (`Newtype` x)  <$> f n
--         Record  n fs -> (`Record`  fs) <$> f n
--         Product n fs -> (`Product` fs) <$> f n
--         Nullary n m  -> (`Nullary` m)  <$> f n
--         Empty   n    -> Empty          <$> f n

instance DerivingOf Data where
    derivingOf d = f . derivingOf $ toListOf (dataFields . typeOf) d
      where
        f | Newtype{} <- d = id
          | Nullary{} <- d = const [Eq', Ord', Enum', Show', Generic']
          | otherwise      = delete Semigroup' . delete Monoid'

nestedTypes :: Data -> [Type]
nestedTypes = toListOf (dataFields . typesOf)

dataFields :: Traversal' Data Field
dataFields f = \case
    Newtype n x  -> Newtype n <$> f x
    Record  n xs -> Record  n <$> traverse f xs
    Product n xs -> pure (Product n xs)
    Nullary n m  -> pure (Nullary n m)
    Empty   n    -> pure (Empty n)
    Void         -> pure Void

fieldNames :: Data -> [Text]
fieldNames (Nullary _ m) = Map.keys m
fieldNames d             = toListOf (dataFields . nameOf) d

fieldPrefix :: Data -> Maybe Text
fieldPrefix = fmap (Text.takeWhile (not . isUpper)) . headMay . fieldNames

mapFieldNames :: (Text -> Text) -> Data -> Data
mapFieldNames f (Nullary n m) = Nullary n . Map.fromList . map (first f) $ Map.toList m
mapFieldNames f d             = d & dataFields %~ nameOf %~ f

setStreaming :: Bool -> Data -> Data
setStreaming rq = dataFields %~ go
  where
    go x = x & typeOf %~ transform (body (x ^. fLocation))

    body :: Location -> Type -> Type
    body Body (TMaybe x@(TPrim y))
        | PReq <- y = x
        | PRes <- y = x
        | otherwise = body Body x
    body Body (TPrim _)
        | rq         = TPrim PReq
        | otherwise  = TPrim PRes
    body _    x      = x

isVoid :: Data -> Bool
isVoid Void = True
isVoid _    = False

data Request = Request
    { _rqUri  :: !URI
    , _rqName :: !Text
    , _rqData :: !Data
    } deriving (Eq, Show)

instance ToJSON Request where
    toJSON (Request u n d) = Object (operationJSON n d <> x)
      where
        Object x = object
            [ "path"      .= _uriPath u
            , "headers"   .= map pair hdr
            , "headerPad" .= pad hdr
            , "query"     .= (map toJSON (_uriQuery u) ++ map pair qry)
            , "queryPad"  .= pad qry
            ]

        pad [] = 0
        pad xs = (+1) . maximum $ map (Text.length . _fLocationName) xs

        hdr = locations isHeader
        qry = locations isQuery

        locations p = filter p (toListOf dataFields d)

        pair f = object
            [ "type"         .= "field"
            , "field"        .= fieldName (_fName f)
            , "locationName" .= _fLocationName f
            ]

data Response = Response
    { _rsWrapper       :: !Bool
    , _rsResultWrapper :: Maybe Text
    , _rsName          :: !Text
    , _rsData          :: !Data
    } deriving (Eq, Show)

instance ToJSON Response where
    toJSON (Response w r n d) = Object (operationJSON n d <> x)
      where
        Object x = object
            [ "resultWrapper" .= r
            , "wrapper"       .= w
            ]

operationJSON :: Text -> Data -> Object
operationJSON n d = x <> y
  where
    Object x = toJSON d
    Object y = object
        [ "name"      .= n
        , "streaming" .= stream
        ]

    stream = any ((== Body) . view fLocation) $
        case d of
            Newtype _ f  -> [f]
            Record  _ fs -> fs
            _            -> []

-- FIXME: Errors? Pagination? Result/Request inline and not part of
-- the types module?
data Operation = Operation
    { _opName             :: !Text
    , _opService          :: !Abbrev
    , _opProtocol         :: !Protocol
    , _opNamespace        :: !NS
    , _opImports          :: [NS]
    , _opDocumentation    :: !Doc
    , _opDocumentationUrl :: Maybe Text
    , _opMethod           :: !Method
    , _opRequest          :: !Request
    , _opResponse         :: !Response
    } deriving (Eq, Show)

record stage2 ''Operation

instance Ord Operation where
    compare = on compare _opName

instance ToFilePath Operation where
    toFilePath = toFilePath . _opNamespace

data Endpoint
    = Global
    | Regional
      deriving (Eq, Show)

instance ToJSON Endpoint where
    toJSON = toJSON . show

data Service = Service
    { _svName           :: !Text
    , _svAbbrev         :: !Abbrev
    , _svNamespace      :: !NS
    , _svImports        :: [NS]
    , _svVersion        :: !Text
    , _svDocumentation  :: !Doc
    , _svProtocol       :: !Protocol
    , _svEndpoint       :: !Endpoint
    , _svEndpointPrefix :: !Text
    , _svSignature      :: !Signature
    , _svChecksum       :: !Checksum
    , _svXmlNamespace   :: !Text
    , _svTargetPrefix   :: Maybe Text
    , _svError          :: !Text
    } deriving (Eq, Show)

record stage2 ''Service

instance ToFilePath Service where
    toFilePath = toFilePath . _svNamespace

data Cabal = Cabal
    { _cLibrary      :: !Library
    , _cVersion      :: !Version
    , _cSynopsis     :: !Doc
    , _cDescription  :: !Doc
    , _cDependencies :: [Version]
    , _cExposed      :: [NS]
    , _cOther        :: [NS]
    } deriving (Eq, Show)

record stage2 ''Cabal

instance ToFilePath Cabal where
    toFilePath c = toFilePath (_cLibrary c) <.> "cabal"

data Types = Types
    { _tService   :: Service
    , _tNamespace :: !NS
    , _tImports   :: [NS]
    , _tTypes     :: [Data]
    } deriving (Eq, Show)

record stage2 ''Types

instance ToFilePath Types where
    toFilePath = toFilePath . _tNamespace

data Stage2 = Stage2
    { _s2Cabal      :: Cabal
    , _s2Service    :: Service
    , _s2Operations :: [Operation]
    , _s2Types      :: Types
    } deriving (Eq, Show)

record stage2 ''Stage2

store :: ToJSON a => FilePath -> Model -> a -> Script ()
store d m x =
    say "Store Stage2" f >> scriptIO (LBS.writeFile f (encodePretty x))
  where
    f = d </> _mName m <.> "json"

render :: FilePath -> Templates -> Stage2 -> Script FilePath
render d Templates{..} s2 = do
    createDir src

    renderFile "Render Cabal"   lib _tCabal   (s2 ^. s2Cabal)
    renderFile "Render Service" gen _tService (s2 ^. s2Service)
    renderFile "Render Types"   gen _tTypes   (s2 ^. s2Types)

    mapM_ (renderFile "Render Operation" gen _tOperation) (s2 ^. s2Operations)

    return lib
  where
    src :: FilePath
    src = rel "src"

    gen :: FilePath
    gen = rel "gen"

    lib :: FilePath
    lib = rel ""

    rel :: ToFilePath a => a -> FilePath
    rel = combine d . combine (toFilePath (s2 ^. s2Cabal.cLibrary)) . toFilePath
