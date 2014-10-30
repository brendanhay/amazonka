{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFoldable             #-}
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
import qualified Data.ByteString.Lazy     as LBS
import           Data.Char
import           Data.Foldable            (Foldable, foldl')
import           Data.HashMap.Strict      (HashMap)
import qualified Data.HashMap.Strict      as Map
import           Data.List                (intersect, nub, sort, delete, partition)
import           Data.Monoid
import           Data.SemVer
import           Data.String
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           Gen.V2.IO
import           Gen.V2.JSON              ()
import           Gen.V2.TH
import           Gen.V2.Types
import           System.FilePath

default (Text, FilePath)

newtype Doc = Doc Text
    deriving (Eq, Show, ToJSON, IsString)

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

nullary stage2 ''Derive

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
      deriving (Eq, Show)

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
    PTime ts  -> "Time " <> timestamp ts

instance DerivingOf Prim where
    derivingOf = (def ++) . \case
        PBool    -> [Enum']
        PText    -> [Monoid']
        PInt     -> [Num', Enum']
        PInteger -> [Num', Enum']
        PDouble  -> [Num', Enum']
        _        -> []
      where
        def = [Eq', Ord', Show', Generic']

data Type
    = TType  !Text
    | TPrim  !Prim
    | TList  !Type
    | TList1 !Type
    | TMap   !Type !Type
    | TMaybe !Type
      deriving (Eq, Show)

instance Plated Type where
    plate f = \case
        TList   x   -> TList  <$> f x
        TList1  x   -> TList1 <$> f x
        TMap    k v -> TMap   <$> f k <*> f v
        TMaybe  x   -> TMaybe <$> f x
        x           -> pure x

instance ToJSON Type where
    toJSON = toJSON . go
      where
        go = \case
            TType   t   -> t
            TPrim   p   -> primitive p
            TList   x   -> "List "  <> wrap (go x)
            TList1  x   -> "List1 " <> wrap (go x)
            TMap    k v -> "Map "   <> wrap (go k) <> " " <> wrap (go v)
            TMaybe  x   -> "Maybe " <> wrap (go x)

        wrap   t = maybe t (const (parens t)) (Text.findIndex isSpace t)
        parens t = "(" <> t <> ")"

isRequired :: Type -> Bool
isRequired (TMaybe _) = False
isRequired _          = True

class TypesOf a where
    typesOf :: a -> [Type]

instance TypesOf a => TypesOf [a] where
    typesOf = nub . concatMap typesOf

instance TypesOf Type where
    typesOf = (:[])

instance DerivingOf Type where
    derivingOf = \case
        TType  _   -> [Eq', Ord', Show', Generic']
        TPrim  p   -> derivingOf p
        TList  x   -> Monoid' : derivingOf x
        TList1 x   -> Semigroup' : derivingOf x
        TMap   k v -> derivingOf k `intersect` derivingOf v
        TMaybe x   -> derivingOf x

data Typed a = Typed
    { _typeOf :: !Type
    , _typedV :: !a
    } deriving (Eq, Show, Functor, Foldable, Traversable)

makeClassy ''Typed

instance ToJSON a => ToJSON (Typed a) where
    toJSON (Typed k v) = rewrap ("type", toJSON k) (toJSON v)

instance TypesOf (Typed a) where
    typesOf = (:[]) . _typeOf

data Named a = Named
   { _nameOf :: !Text
   , _namedV :: !a
   } deriving (Eq, Show, Functor, Foldable, Traversable)

makeLenses ''Named

instance ToJSON a => ToJSON (Named a) where
    toJSON (Named k v) = rewrap ("name", String k) (toJSON v)

instance TypesOf a => TypesOf (Named a) where
    typesOf = typesOf . _namedV

instance HasTyped (Named (Typed a)) a where
    typed = namedV

type Ann a = Named (Typed a)

data Field = Field
    { _fLocation     :: Maybe Location
    , _fLocationName :: Maybe Text
    , _fPayload      :: !Bool
    , _fStreaming    :: !Bool
    } deriving (Eq, Show)

record stage2 ''Field

data Data
    = Newtype !(Ann Field)
    | Record  [Ann Field]
    | Nullary [Ann Field]
    | Empty
      deriving (Eq, Show)

instance ToJSON Data where
    toJSON d = toJSON . Derived d $
        case d of
            Nullary fs -> object ["type" .= "nullary", "enums" .= fs]
            Newtype f  -> object ["type" .= "newtype", "field" .= f]
            Empty      -> object ["type" .= "empty"]
            Record  fs -> object
                [ "type"     .= "record"
                , "required" .= req
                , "optional" .= opt
                , "payload"  .= pay
                ]
              where
                (req, opt) = partition (isRequired . view typeOf) fs

                pay = headMay (filter (view (namedV.typedV.fPayload)) fs)

instance DerivingOf Data where
    derivingOf d = f (derivingOf (typesOf d))
      where
        f | Newtype{} <- d = id
          | otherwise      = delete Monoid'

instance TypesOf Data where
    typesOf = \case
        Newtype f  -> typesOf f
        Record  fs -> typesOf fs
        Nullary fs -> typesOf fs
        Empty      -> []

setStreaming :: Bool -> Data -> Data
setStreaming rq = \case
    Newtype f  -> Newtype (go f)
    Record  fs -> Record  (map go fs)
    Nullary fs -> Nullary (map go fs)
    Empty      -> Empty
  where
    go x = x & typeOf %~ transform (body (x ^. namedV.typedV.fStreaming))

    body :: Bool -> Type -> Type
    body True (TMaybe x@(TPrim PBlob)) = body True x
    body True (TPrim PBlob)
        | rq        = TPrim PReq
        | otherwise = TPrim PRes
    body _    x     = x

data Body
    = XML
    | JSON
    | Raw
    | Stream
      deriving (Eq, Show)

nullary stage2 ''Body

newtype Request = Request Data
    deriving (Eq, Show)

instance ToJSON Request where
    toJSON (Request d) = toJSON d

data Response = Response
    { _rsWrapper       :: !Bool
    , _rsResultWrapper :: Maybe Text
    , _rsData          :: !Data
    } deriving (Eq, Show)

instance ToJSON Response where
    toJSON (Response w r d) = Object (x <> y)
      where
        Object x = toJSON (Request d)
        Object y = object
            [ "resultWrapper" .= r
            , "wrapper"       .= w
            ]

-- FIXME: Errors? Pagination? Result/Request inline and not part of
-- the types module?
data Operation = Operation
    { _opDocumentation    :: !Doc
    , _opDocumentationUrl :: Maybe Text
    , _opMethod           :: !Method
    , _opUri              :: !URI
    , _opRequest          :: !(Named Request)
    , _opResponse         :: !(Named Response)
    } deriving (Eq, Show)

record stage2 ''Operation

data Endpoint
    = Global
    | Regional
      deriving (Eq, Show)

nullary (stage2 & thCtor .~ id) ''Endpoint

data Service = Service
    { _svName           :: !Text
    , _svAbbrev         :: !Abbrev
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

data Cabal = Cabal
    { _cLibrary      :: !Library
    , _cVersion      :: !Version
    , _cSynopsis     :: !Doc
    , _cDescription  :: !Doc
    , _cModules      :: [NS]
    , _cDependencies :: [Named Version]
    } deriving (Eq, Show)

record stage2 ''Cabal

instance ToFilePath Cabal where
    toFilePath c = toFilePath (_cLibrary c) <.> "cabal"

data Mod a = Mod
    { _mModule    :: !a
    , _mNamespace :: !NS
    , _mImports   :: [NS]
    } deriving (Eq, Show, Functor)

makeLenses ''Mod

instance ToJSON a => ToJSON (Mod a) where
    toJSON Mod{..} = Object (x <> y)
      where
        Object x = toJSON _mModule
        Object y = object
            [ "namespace" .= _mNamespace
            , "imports"   .= _mImports
            ]

instance ToFilePath (Mod a) where
    toFilePath = toFilePath . _mNamespace

data Stage2 = Stage2
    { _s2Cabal      :: Cabal
    , _s2Service    :: Mod Service
    , _s2Operations :: [Mod (Named Operation)]
    , _s2Types      :: Mod (HashMap Text Data)
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
    (t, o) = _tProtocol (s2 ^. s2Service.mModule.svProtocol)

    src :: FilePath
    src = rel "src"

    gen :: FilePath
    gen = rel "gen"

    lib :: FilePath
    lib = rel ""

    rel :: ToFilePath a => a -> FilePath
    rel = combine d . combine (toFilePath (s2 ^. s2Cabal.cLibrary)) . toFilePath
