{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

-- Module      : Gen.AST
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.AST
     ( ToEnv (..)
     , Library
     , HasLibrary (..)
     , Cabal
     , cabal
     ) where

import           Control.Applicative          (Applicative, pure, (<$>))
import           Control.Arrow                ((***))
import           Control.Error
import           Control.Lens                 (makeClassy, makeLenses, view,
                                               (^.))
import           Control.Monad.Trans.Except
import Control.Monad.Error
import           Data.Aeson
import           Data.Aeson.Types             (Pair)
import           Data.Bifunctor
import qualified Data.Foldable                as Fold
import           Data.Hashable                (Hashable)
import qualified Data.HashMap.Strict          as Map
import qualified Data.HashSet                 as Set
import           Data.List                    (sort)
import           Data.Maybe
import           Data.Monoid                  hiding (Sum)
import qualified Data.SemVer                  as SemVer
import           Data.String
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Text.Lazy               as LText
import           Data.Text.Manipulate
import           Data.Traversable             (traverse)
import           Gen.Documentation            as Doc
import           Gen.Model                    hiding (Name)
import           Gen.OrdMap                   (OrdMap)
import qualified Gen.OrdMap                   as OrdMap
import           Gen.Text
import           Gen.Types
import           GHC.Generics                 (Generic)
import qualified Language.Haskell.Exts        as Exts
import           Language.Haskell.Exts.Build  (app, lamE, op, paren, sfun, sym)
import           Language.Haskell.Exts.Pretty
import           Language.Haskell.Exts.Syntax
import           Prelude                      hiding (Enum)

nfield :: Member -> Text
nfield = Text.cons '_' . nlens

nlens :: Member -> Text
nlens (Member p _ n) = prefixed (Text.toLower <$> p) n

nbranch :: Member -> Text
nbranch (Member p _ n) = prefixed (Text.toUpper <$> p) n

nctor :: Text -> Text
nctor = reserved . lowerHead

prefixed :: Maybe Text -> Text -> Text
prefixed (Just x) = mappend x . upperHead
prefixed Nothing  = upperHead

pretty :: (Monad m, MonadError String m, Pretty a) => a -> m LText.Text
pretty d =
    return (LText.pack x)
    --hoist $ HIndent.reformat HIndent.johanTibell Nothing (LText.pack x)
  where
    -- hoist (Left  e) = throwError (e ++ "\nDecl: " ++ x)
    -- hoist (Right o) = return (Build.toLazyText o)

    x = prettyPrintStyleMode style' mode' d

    style' = style
        { mode           = PageMode
        , lineLength     = 80
        , ribbonsPerLine = 1.5
        }

    mode' = defaultMode
        { spacing = False
        , layout  = PPNoLayout
        }

class ToEnv a where
    toEnv :: (Applicative m, MonadError String m) => a -> m Value

    default toEnv :: (Applicative m, MonadError String m, ToJSON a)
                  => a
                  -> m Value
    toEnv = pure . toJSON

(.-) :: (Applicative m, Monad m, MonadError String m, ToEnv a)
     => Text
     -> a
     -> m Pair
k .- v = (k,) <$> toEnv v

env :: (Monad m, MonadError String m) => [m Pair] -> m Value
env = liftM object . sequence

instance ToEnv Text
instance ToEnv LText.Text
instance ToEnv (Above Doc)
instance ToEnv (Below Doc)
instance ToEnv (Blank Doc)

instance ToEnv a => ToEnv [a] where
    toEnv = fmap toJSON . traverse toEnv

instance ToEnv v => ToEnv (TextMap v) where
    toEnv = fmap toJSON . traverse toEnv

-- instance ToEnv v => ToEnv (HashSet v) where
--     toEnv = fmap toJSON . traverse toEnv . Set.toList

instance ToEnv v => ToEnv (OrdMap Member v) where
    toEnv = toEnv . Map.fromList . map (first nbranch) . OrdMap.toList

instance ToEnv ModuleName   where toEnv = pure . toJSON . prettyPrint
instance ToEnv ModulePragma where toEnv = pure . toJSON . prettyPrint
instance ToEnv ImportDecl   where toEnv = pure . toJSON . prettyPrint
instance ToEnv Name         where toEnv = pure . toJSON . prettyPrint
instance ToEnv Decl         where toEnv = fmap toJSON . pretty

data Fun = Fun Name Doc Decl Decl

instance ToEnv Fun where
    toEnv (Fun n doc sig decl) = env
        [ "name"        .- n
        , "comment"     .- Above 0 doc
        , "signature"   .- sig
        , "declaration" .- decl
        ]

data Inst
    = IToQuery
    | IToJSON
    | IFromJSON
    | IToXML
    | IFromXML
      deriving (Eq, Ord, Show, Generic)

instance Hashable Inst

instance ToEnv Inst where
    toEnv = pure . \case
        IToQuery  -> "to-query"
        IToJSON   -> "to-json"
        IFromJSON -> "from-json"
        IToXML    -> "to-xml"
        IFromXML  -> "from-xml"

protoInsts :: Protocol -> [Inst]
protoInsts = sort . \case
    JSON     -> [IFromJSON, IToJSON]
    RestJSON -> [IFromJSON, IToJSON]
    XML      -> [IFromXML, IToXML]
    RestXML  -> [IFromXML, IToXML]
    Query    -> [IToQuery, IFromXML]
    EC2      -> [IToQuery, IFromXML]

data Data
    = Prod (Typed Struct) Doc Decl Fun (TextMap Fun) [Inst]
    | Sum Enum Doc Decl [Inst]

fieldPairs :: OrdMap Member (Typed Ref) -> TextMap Text
fieldPairs = Map.fromList . map f . OrdMap.toList
  where
    f (k, v) = (nfield k, fromMaybe (_memOriginal k) (v ^. refLocationName))

instance ToEnv Data where
    toEnv = env . \case
        Prod x doc decl ctor ls is ->
            [ "type"        .- Text.pack "product"
            , "constructor" .- ctor
            , "comment"     .- Above 0 doc
            , "declaration" .- decl
            , "fields"      .- fieldPairs (x ^. structMembers)
            , "lenses"      .- ls
            , "instances"   .- is
            ]
        Sum x doc decl is ->
            [ "type"         .- Text.pack "sum"
            , "comment"      .- Above 0 doc
            , "declaration"  .- decl
            , "constructors" .- view enumValues x
            , "instances"    .- is
            ]

data Mod = Mod ModuleName [ModulePragma] [ModulePragma] [ImportDecl] (TextMap Data)

instance ToEnv Mod where
    toEnv (Mod n es os is ds) = env
        [ "name"       .- n
        , "extensions" .- es
        , "options"    .- os
        , "imports"    .- is
        , "shapes"     .- ds
        ]

data Library = Library
    { _libService    :: Mod
    , _libTypes      :: Mod
    , _libOperations :: [Mod]
    , _libWaiters    :: Mod
    }

-- instance ToEnv Library where
--     toEnv Library{..} = env
--         [ -- "service"    .- _libService
-- --          "types"      .- _libTypes
--         -- , "operations" .- _libOperations
--         -- , "waiters"    .- _libWaiters
--         ]

makeClassy ''Library

data Cabal = Cabal
    { _cblVersion :: SemVer.Version
    , _cblService :: Service (Derived Shape) (Untyped Ref)
    , _cblLibrary :: Library
    }

makeLenses ''Cabal

instance HasMetadata Cabal where
    metadata = cblService . svcMetadata

instance HasService Cabal (Derived Shape) (Untyped Ref) where
    service = cblService

instance HasLibrary Cabal where
    library = cblLibrary

instance ToEnv Cabal where
    toEnv c = env
       [ "name"             .- view svcName c
       , "library"          .- view svcLibrary c
       , "version"          .- SemVer.toText (c ^. cblVersion)
       , "documentation"    .- Blank 4 (c ^. svcDocumentation)
       , "documentationUrl" .- view svcDocumentationUrl c
--       , "modules"          .- view cblModules c
       ]

cabal :: SemVer.Version -> Service (Derived Shape) (Untyped Ref) -> Cabal
cabal v s = Cabal v s (Library serviceMod (typesMod n s) [] waitersMod)
  where
    n = s ^. svcAbbrev

serviceMod :: Mod
serviceMod = error "serviceMod"

typesMod :: Text -> Service (Derived Shape) b -> Mod
typesMod n s = Mod (moduleName n) es os is ts
  where
    es = extensions
       [ "DataKinds"
       , "DeriveGeneric"
       , "FlexibleInstances"
       , "GeneralizedNewtypeDeriving"
       , "LambdaCase"
       , "NoImplicitPrelude"
       , "OverloadedStrings"
       , "RecordWildCards"
       , "TypeFamilies"
       , "ViewPatterns"
       ]

    os = options
       [ "-fno-warn-unused-imports"
       ]

    is = imports
       [ ("Network.AWS.Prelude", False)
       , ("Network.AWS.Signing", False)
       , ("GHC.Exts",            True)
       ]

    ts = Map.fromList . mapMaybe f $ Map.toList (s ^. svcShapes)
      where
        f (n, s) = (n,) <$> shapeData insts n s

        insts = protoInsts (s ^. metaProtocol)

operationMod :: Mod
operationMod = error "operationMod"

waitersMod :: Mod
waitersMod = error "waitersMod"

extensions :: [Text] -> [ModulePragma]
extensions = map (LanguagePragma loc . (:[]) . name) . sort

options :: [Text] -> [ModulePragma]
options = map (OptionsPragma loc (Just GHC) . Text.unpack . (`Text.snoc` ' '))

imports :: [(Text, Bool)] -> [ImportDecl]
imports = map f
  where
    f (n, q) = ImportDecl loc (moduleName n) q False False Nothing Nothing Nothing

shapeData :: [Inst] -> Text -> Derived Shape -> Maybe Data
shapeData is t Derived{..} = case _derTyped of
    SStruct x -> Just (prod x)
    SEnum   x -> Just (sum' x)
    _         -> Nothing
  where
    prod x =
        let ctor = structCtor t x
            decl = recDecl n (x ^. structMembers) d
         in Prod x (doc "Undocumented type.") decl ctor (lenses t x) is

    sum' x =
        let decl = sumDecl n (x ^. enumValues) d
         in Sum x (doc "Undocumented enumeration.") decl is

    doc = flip fromMaybe (_derTyped ^. documentation)

    n = name t
    d = map ((,[]) . unqual . constraint) . sort $ Set.toList _derConst

lenses :: Text -> Typed Struct -> TextMap Fun
lenses t = Map.fromList . map mk . OrdMap.toList . view structMembers
  where
    mk :: (Member, Typed Ref) -> (Text, Fun)
    mk (k, v) = (l, Fun n doc sig fun)
      where
        n = name l
        l = nlens k
        f = nfield k

        doc = fromMaybe "Undocumented lens." (v ^. refDocumentation)

        sig = typeSig n lens []
        fun = sfun loc n [] (UnGuardedRhs sett) (BDecls [])

        lens = TyApp (TyApp (tycon "Lens'") (tycon t)) (external (v ^. refAnn))

        sett = mapping (v ^. refAnn) $
            app (app (var "lens") (var f))
                (paren (lamE loc [pvar "s", pvar "a"]
                    (RecUpdate (var "s") [FieldUpdate (unqual f) (var "a")])))

--typeSig :: Name -> Type -> [Type] -> Decl

data Field = Field
    { _fldParam  :: Name
    , _fldName   :: Member
    , _fldType   :: TType
    , _fldUpdate :: FieldUpdate
    , _fldReq    :: Bool
    }

structCtor :: Text -> Typed Struct -> Fun
structCtor t (structFields -> fs) = Fun c d sig fun
  where
    d = fromString ("'" <> Text.unpack t <> "' smart constructor.")

    c = name (nctor t)
    n = name t

    sig = typeSig c (tycon t) (map external ts)
    fun = sfun loc c ps (UnGuardedRhs (RecConstr (UnQual n) us)) (BDecls [])

    ps = map _fldParam rs
    ts = map _fldType rs
    rs = filter _fldReq fs
    us = map _fldUpdate fs

structFields :: Typed Struct -> [Field]
structFields = zipWith mk [1..] . OrdMap.toList . view structMembers
  where
    mk :: Int -> (Member, Typed Ref) -> Field
    mk n (k, v) =
        let p = Ident ("p" ++ show n)
            t = v ^. refAnn
            d = defaulted t
            f = fromMaybe (Var (UnQual p)) d
         in Field
            { _fldParam  = p
            , _fldName   = k
            , _fldType   = t
            , _fldUpdate = FieldUpdate (UnQual (name $ nfield k)) f
            , _fldReq    = not (isJust d)
            }

typeSig :: Name -> Type -> [Type] -> Decl
typeSig n t = TypeSig loc [n] . Fold.foldr' (\x g -> TyFun x g) t

sumDecl :: Name -> OrdMap Member Text -> [Deriving] -> Decl
sumDecl n vs = dataDecl n (sumCtor `map` OrdMap.keys vs)

recDecl :: Name -> OrdMap Member (Typed Ref) -> [Deriving] -> Decl
recDecl n ms = dataDecl n [recCtor n (fields ms)]

dataDecl :: Name -> [QualConDecl] -> [Deriving] -> Decl
dataDecl n cs = DataDecl loc (dataOrNew cs) [] n [] cs

sumCtor :: Member -> QualConDecl
sumCtor m = QualConDecl loc [] [] (ConDecl (name $ nbranch m) [])

recCtor :: Name -> [([Name], Type)] -> QualConDecl
recCtor n = QualConDecl loc [] [] . RecDecl n

tycon :: Text -> Type
tycon = TyCon . UnQual . name

singleton :: Text -> Type
singleton = tycon . ("\"" <>) . (<> "\"")

dataOrNew :: [QualConDecl] -> DataOrNew
dataOrNew = \case
    [QualConDecl _ _ _ (RecDecl _ [_])] -> NewType
    _                                   -> DataType

fields :: OrdMap Member (Typed Ref) -> [([Name], Type)]
fields = map (((:[]) . name . nfield) *** (internal . view refAnn)) . OrdMap.toList

var :: Text -> Exp
var = Exts.var . name

pvar :: Text -> Pat
pvar = Exts.pvar . name

unqual :: Text -> QName
unqual = UnQual . name

name :: Text -> Name
name = Ident . Text.unpack

moduleName :: Text -> ModuleName
moduleName = ModuleName . Text.unpack

loc :: SrcLoc
loc = SrcLoc "" 0 0

defaulted :: TType -> Maybe Exp
defaulted = \case
    TMaybe {} -> Just (var "Nothing")
    TList  {} -> Just (var "mempty")
    TMap   {} -> Just (var "mempty")
    TEMap  {} -> Just (var "mempty")
    _         -> Nothing

internal :: TType -> Type
internal = \case
    TType  x         -> tycon x
    TPrim  x         -> primitive True x
    TMaybe x         -> TyApp (tycon "Maybe") (internal x)
    TSens  x         -> TyApp (tycon "Sensitive") (internal x)
    TFlat  x         -> TyApp (tycon "Flatten") (internal x)
    -- TCase  x         -> TyApp (tycon "CI") (internal x)
    TList  i x       -> TyApp (TyApp (tycon "List") (singleton i)) (internal x)
    TList1 i x       -> TyApp (TyApp (tycon "List1") (singleton i)) (internal x)
    TMap   k v       -> TyApp (TyApp (tycon "Map") (internal k)) (internal v)
    TEMap  e i j k v ->
        TyApp
          (TyApp
            (TyApp
               (TyApp
                  (TyApp (tycon "EMap") (singleton e))
                  (singleton i))
               (singleton j))
            (internal k))
          (internal v)

external :: TType -> Type
external = \case
    TType  x         -> tycon x
    TPrim  x         -> primitive False x
    TMaybe x         -> TyApp (tycon "Maybe") (external x)
    TSens  x         -> external x
    TFlat  x         -> external x
    -- TCase  x         -> external x
    TList  _ x       -> TyList (external x)
    TList1 _ x       -> TyApp (tycon "NonEmpty") (external x)
    TMap   k v       -> TyApp (TyApp (tycon "HashMap") (external k)) (external v)
    TEMap  _ _ _ k v -> TyApp (TyApp (tycon "HashMap") (external k)) (external v)

-- FIXME: This can potentially need chaining
mapping :: TType -> (Exp -> Exp)
mapping = compose . iso
  where
    compose xs e = Fold.foldl' (\y -> InfixApp y (op (sym "."))) e xs

    iso = \case
        TPrim (PTime {})    -> [var "_Time"]
        TPrim (PNatural {}) -> [var "_Nat"]
        TMaybe x            -> var "mapping" : iso x
        TSens  x            -> var "_Sensitive" : iso x
        TFlat  x            -> var "_Flatten" : iso x
        TList  {}           -> [var "_List"]  -- Coercible.
        TList1 {}           -> [var "_List1"] -- Coercible.
        TMap   {}           -> [var "_Map"]   -- Coercible.
        TEMap  {}           -> [var "_EMap"]  -- Coercible.
        _                   -> []

primitive :: Bool -> Prim -> Type
primitive i = tycon . \case
    PBlob           -> "Base64"
    -- PReq         -> "RqBody"
    -- PRes         -> "RsBody"
    PBool           -> "Bool"
    PText           -> "Text"
    PInt            -> "Int"
    PInteger        -> "Integer"
    PDouble         -> "Double"
    PNatural
        | i         -> "Nat"
        | otherwise -> "Natural"
    PTime ts
        | i         -> Text.pack (show ts)
        | otherwise -> "UTCTime"
