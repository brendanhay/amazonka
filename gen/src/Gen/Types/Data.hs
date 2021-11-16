{-# LANGUAGE TemplateHaskell #-}

module Gen.Types.Data where

import qualified Control.Lens as Lens
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Pair)
import qualified Data.Function as Function
import qualified Data.Set as Set
import qualified Data.Text as Text
import Gen.Prelude
import Gen.Types.Ann
import Gen.Types.Help
import Gen.Types.Id
import Gen.Types.TypeOf

type Rendered = TextLazy

data Fun = Fun'
  { _funName :: Text,
    _funDoc :: Help,
    _funSig :: Rendered,
    _funDecl :: Rendered,
    _funMeta :: Rendered
  }
  deriving (Eq, Show)

instance ToJSON Fun where
  toJSON Fun' {..} =
    Aeson.object
      [ "type" .= Text.pack "function",
        "name" .= _funName,
        "documentation" .= _funDoc,
        "signature" .= _funSig,
        "declaration" .= _funDecl,
        "meta" .= _funMeta
      ]

data Prod = Prod'
  { _prodName :: Text,
    _prodDoc :: Maybe Help,
    _prodDecl :: Rendered,
    _prodCtor :: Fun,
    _prodLenses :: [Fun],
    _prodDeps :: Set.Set Text
  }
  deriving (Eq, Show)

prodToJSON :: ToJSON a => Solved -> Prod -> HashMap Text a -> [Pair]
prodToJSON s Prod' {..} is =
  [ "type" .= Text.pack "product",
    "name" .= _prodName,
    "constructor" .= _prodCtor,
    "documentation" .= _prodDoc,
    "declaration" .= _prodDecl,
    "lenses" .= _prodLenses,
    "instances" .= is,
    "shared" .= isShared s,
    "eq" .= isEq s
  ]

data Sum = Sum'
  { _sumName :: Text,
    _sumDoc :: Maybe Help,
    _sumDecl :: Rendered,
    _sumCtor :: Text,
    _sumCtors :: HashMap Text Text
  }
  deriving (Eq, Show)

sumToJSON :: Solved -> Sum -> [Text] -> [Pair]
sumToJSON s Sum' {..} is =
  [ "type" .= Text.pack "sum",
    "name" .= _sumName,
    "constructor" .= _sumCtor,
    "constructors" .= _sumCtors,
    "documentation" .= _sumDoc,
    "declaration" .= _sumDecl,
    "instances" .= is,
    "shared" .= isShared s,
    "eq" .= True
  ]

data Gen = Gen'
  { _genName :: Text,
    _genDoc :: Maybe Help,
    _genDecl :: Rendered
  }
  deriving (Eq, Show)

instance ToJSON Gen where
  toJSON Gen' {..} =
    Aeson.object
      [ "type" .= Text.pack "error",
        "name" .= _genName,
        "documentation" .= _genDoc,
        "declaration" .= _genDecl
      ]

data SData
  = -- | A product type (record).
    Prod !Solved Prod (HashMap Text Rendered)
  | -- | A nullary sum type.
    Sum !Solved Sum [Text]
  | -- | A function declaration.
    Fun Fun
  deriving (Eq, Show)

instance Ord SData where
  compare a b =
    case (a, b) of
      (Prod _ x _, Prod _ y _) -> Function.on compare _prodName x y
      (Sum _ x _, Sum _ y _) -> Function.on compare _sumName x y
      (Fun _, Fun _) -> EQ
      (Prod {}, _) -> GT
      (_, Prod {}) -> LT
      (Sum {}, _) -> GT
      (_, Sum {}) -> LT

instance ToJSON SData where
  toJSON = \case
    Prod s x is -> Aeson.object (prodToJSON s x is)
    Sum s st is -> Aeson.object (sumToJSON s st is)
    Fun f -> Aeson.toJSON f

instance HasId SData where
  identifier = \case
    Prod _ p _ -> mkId (_prodName p)
    Sum _ s _ -> mkId (_sumName s)
    Fun (Fun' n _ _ _ _) -> mkId n

data WData = WData
  { _waitName :: Text,
    _waitOpName :: Id,
    _waitCtor :: Fun
  }
  deriving (Show)

$(Lens.makeLenses ''WData)

instance ToJSON WData where
  toJSON (WData n _ c) =
    Aeson.object
      [ "name" .= n,
        "constructor" .= c
      ]
