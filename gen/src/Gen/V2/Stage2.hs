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

-- Module      : Gen.V2.Stage2
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.V2.Stage2 where

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
import           Data.Foldable            (foldl')
import           Data.Function            (on)
import           Data.HashMap.Strict      (HashMap)
import qualified Data.HashMap.Strict      as Map
import           Data.List                (intersect, nub, sort, delete, partition)
import           Data.Monoid
import           Data.SemVer
import           Data.String
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           Data.Text.Manipulate
import           Debug.Trace
import           Gen.V2.IO
import           Gen.V2.JSON              ()
import           Gen.V2.Names
import           Gen.V2.TH
import           Gen.V2.Types
import           System.FilePath

default (Text, FilePath)

newtype Doc = Doc Text
    deriving (Eq, Ord, Show, ToJSON, IsString)

documentation :: Maybe Text -> Doc
documentation = Doc . fromMaybe ""

newtype NS = NS [Text]
    deriving (Eq, Ord, Show)

instance ToJSON NS where
    toJSON (NS xs) = String (Text.intercalate "." xs)

instance ToFilePath NS where
    toFilePath (NS xs) = Text.unpack (Text.intercalate "/" xs) <.> "hs"

namespace :: [Text] -> NS
namespace = NS . ("Network":) . ("AWS":)

typesNS :: Abbrev -> NS
typesNS (Abbrev a) = namespace [a, "Types"]

operationNS :: Abbrev -> Text -> NS
operationNS (Abbrev a) op = namespace [a, op]

requestNS :: Protocol -> NS
requestNS p = namespace ["Request", Text.pack (show p)]

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
constructor = uncurry (<>) . first Text.toLower . breakWord . view nameOf

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

primitive :: Prim -> Text
primitive = \case
    PBlob     -> "Blob"
    PReq      -> "RqBody"
    PRes      -> "RsBody"
    PBool     -> "Bool"
    PText     -> "Text"
    PInt      -> "Int"
    PInteger  -> "Integer"
    PDouble   -> "Double"
    PTime ts  -> timestamp ts

instance DerivingOf Prim where
    derivingOf = (def ++) . \case
        PBool    -> [Ord', Enum']
        PText    -> [Ord', Monoid']
        PInt     -> [Ord', Num', Enum']
        PInteger -> [Ord', Num', Enum']
        PDouble  -> [Ord', Num', Enum']
        PTime _  -> [Ord']
        _        -> []
      where
        def = [Eq', Show', Generic']

data Type
    = TType      !Text
    | TPrim      !Prim
    | TList      !Type
    | TList1     !Type
    | TMap       !Type !Type
    | TMaybe     !Type
    | TSensitive !Type
      deriving (Eq, Ord, Show)

instance Plated Type where
    plate f = \case
        TList      x   -> TList      <$> f x
        TList1     x   -> TList1     <$> f x
        TMap       k v -> TMap       <$> f k <*> f v
        TMaybe     x   -> TMaybe     <$> f x
        TSensitive x   -> TSensitive <$> f x
        x              -> pure x

instance ToJSON Type where
    toJSON = toJSON . go
      where
        go = \case
            TType      t   -> t
            TPrim      p   -> primitive p
            TList      x   -> "["          <> wrap (go x) <> "]"
            TList1     x   -> "List1 "     <> wrap (go x)
            TMap       k v -> "Map "       <> wrap (go k) <> " " <> wrap (go v)
            TMaybe     x   -> "Maybe "     <> wrap (go x)
            TSensitive x   -> "Sensitive " <> wrap (go x)

        wrap   t = maybe t (const (parens t)) (Text.findIndex isSpace t)
        parens t = "(" <> t <> ")"

isRequired :: Type -> Bool
isRequired (TMaybe _) = False
isRequired _          = True

isMonoid :: Type -> Bool
isMonoid (TList  _) = True
isMonoid (TMap _ _) = True
isMonoid _          = False

class HasType a where
    typeOf :: Lens' a Type

class TypesOf a where
    typesOf :: a -> [Type]

instance TypesOf a => TypesOf [a] where
    typesOf = nub . concatMap typesOf

instance TypesOf Type where
    typesOf = (:[])

instance DerivingOf Type where
    derivingOf = \case
        TType      _   -> [Eq', Ord', Show', Generic']
        TPrim      p   -> derivingOf p
        TList      x   -> Monoid' : derivingOf x
        TList1     x   -> Semigroup' : derivingOf x
        TMap       k v -> derivingOf k `intersect` derivingOf v
        TMaybe     x   -> derivingOf x
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
        , "shape"         .= _fShape
        , "type"          .= _fType
        , "location"      .= _fLocation
        , "locationName"  .= _fLocationName
        , "documentation" .= _fDocumentation
        , "default"       .= def
        ]
      where
        def | isMonoid   _fType = "mempty"
            | isRequired _fType = "<error>"
            | otherwise         = "Nothing"

parameters :: [Field] -> ([Field], [Field])
parameters = partition (isRequired . view typeOf)

instance HasName Field where
    nameOf = fName

instance HasType Field where
    typeOf = fType

instance TypesOf Field where
    typesOf = (:[]) . _fType

data Data
    = Nullary !Text (HashMap Text Text)
    | Newtype !Text !Field
    | Record  !Text [Field]
    | Empty
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
            Empty        -> object
                [ "type"     .= "empty"
                ]

            Nullary n fs -> object
                [ "type"     .= "nullary"
                , "name"     .= n
                , "branches" .= fs
                ]

            Newtype n f  -> object
                [ "type"     .= "newtype"
                , "name"     .= n
                , "ctor"     .= ctor
                , "ctorPad"  .= pad
                , "field"    .= f
                , "fields"   .= [f]
                , "required" .= req
                , "optional" .= opt
                ]
              where
                (req, opt) = parameters [f]
                pad        = Text.replicate (Text.length ctor) " "
                ctor       = constructor n

            Record  n fs -> object
                [ "type"     .= "record"
                , "name"     .= n
                , "ctor"     .= ctor
                , "ctorPad"  .= pad
                , "fields"   .= sort fs
                , "required" .= req
                , "optional" .= opt
                ]
              where
                (req, opt) = parameters fs
                pad        = Text.replicate (Text.length ctor) " "
                ctor       = constructor n

instance DerivingOf Data where
    derivingOf d = f (derivingOf (typesOf d))
      where
        f | Newtype{} <- d = id
          | Nullary{} <- d = const [Eq', Ord', Enum', Show', Generic']
          | otherwise      = delete Monoid'

instance TypesOf Data where
    typesOf = \case
        Newtype _ f  -> typesOf f
        Record  _ fs -> typesOf fs
        Nullary _ _  -> []
        Empty        -> []

dataFields :: Traversal' Data Field
dataFields f = \case
    Newtype n x  -> Newtype n <$> f x
    Record  n xs -> Record  n <$> traverse f xs
    Nullary n m  -> pure (Nullary n m)
    Empty        -> pure Empty

fieldNames :: Data -> [Text]
fieldNames = toListOf (dataFields . nameOf)

fieldPrefix :: Data -> Maybe Text
fieldPrefix = fmap (Text.takeWhile (not . isUpper)) . headMay . fieldNames

mapFieldNames :: (Text -> Text) -> Data -> Data
mapFieldNames f = dataFields %~ nameOf %~ f

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

isEmpty :: Data -> Bool
isEmpty Empty = True
isEmpty _     = False

data Request = Request
    { _rqUri  :: !URI
    , _rqName :: !Text
    , _rqData :: !Data
    } deriving (Eq, Show)

instance ToJSON Request where
    toJSON (Request u n d) = Object (operationJSON n d <> x)
      where
        Object x = object
            [ "uri" .= u
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
    , _cModules      :: [NS]
    , _cDependencies :: [Version]
    } deriving (Eq, Show)

record stage2 ''Cabal

instance ToFilePath Cabal where
    toFilePath c = toFilePath (_cLibrary c) <.> "cabal"

data Types = Types
    { _tService   :: Service
    , _tNamespace :: !NS
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
    renderFile "Render Types"   gen t         (s2 ^. s2Types)

    mapM_ (renderFile "Render Operation" gen o) (s2 ^. s2Operations)

    return lib
  where
    (t, o) = _tProtocol (s2 ^. s2Service.svProtocol)

    src :: FilePath
    src = rel "src"

    gen :: FilePath
    gen = rel "gen"

    lib :: FilePath
    lib = rel ""

    rel :: ToFilePath a => a -> FilePath
    rel = combine d . combine (toFilePath (s2 ^. s2Cabal.cLibrary)) . toFilePath
