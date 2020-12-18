-- |
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Gen.Source.Exts
  ( -- * Syntax
    Name,
    QName,
    Type,
    Context,
    Decl,
    Exp,
    Pat,
    QOp,
    Binds,
    Deriving,
    InstDecl,
    QualConDecl,
    ConDecl,
    FieldDecl,
    FieldUpdate,
    Rhs,
    GuardedRhs,
    Match,

    -- * Types
    varT,
    conT,
    appT,
    funT,
    parenT,
    listT,
    forallT,
    assertT,

    -- * Names
    nameN,
    nameQ,
    opQ,

    -- * Patterns
    varP,

    -- * Declarations
        dataD,
    derivingD,
    qualConD,
    fieldD,
    typeSigD,
    funBindD,
    matchNullaryD,
    matchWildcardD,
    Exts.sfun,
    Exts.patBindWhere,
    Exts.noBinds,

    -- * Instances
    instanceD,
    associatedTypeD,

    -- * Records
    conR,
    fieldR,
    punR,

    -- * Expressions
    conE,
    varE,
    Exts.paren,
    Exts.app,
    Exts.appFun,
    Exts.infixApp,
    Exts.lamE,
    Exts.listE,
    Exts.tuple,

    -- ** Literals
    fracE,
    strE,
    Exts.intE,

    -- ** Functions
    applyE,
    composeE,
    constE,
    applicativeE,
    fmapE,
    fapplyE,
    pureE,

    -- ** Monoids
    mconcatE,
    memptyE,
    mappendE,

    -- ** Maybe
    justE,
    nothingE,

    -- ** Lenses
    fieldLensE,
    fieldGetterN,

    -- * Guards and matches
    guarded,
    unguarded,
    otherwiseE,
    guardE,
  )
where

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Either as Either
import qualified Data.Foldable as Foldable
import qualified Data.Text as Text
import Gen.Prelude
import qualified Language.Haskell.Exts as Exts

-- Aliases

type Name = Exts.Name ()

type QName = Exts.QName ()

type Type = Exts.Type ()

type Context = Exts.Context ()

type Decl = Exts.Decl ()

type Exp = Exts.Exp ()

type Pat = Exts.Pat ()

type QOp = Exts.QOp ()

type Binds = Exts.Binds ()

type Deriving = Exts.Deriving ()

type InstDecl = Exts.InstDecl ()

type QualConDecl = Exts.QualConDecl ()

type ConDecl = Exts.ConDecl ()

type FieldDecl = Exts.FieldDecl ()

type FieldUpdate = Exts.FieldUpdate ()

type Rhs = Exts.Rhs ()

type GuardedRhs = Exts.GuardedRhs ()

type Match = Exts.Match ()


-- Types

varT :: Text -> Type
varT = Exts.TyVar () . nameN

conT :: Text -> Type
conT = Exts.TyCon () . nameQ

appT :: Type -> Type -> Type
appT a b = Exts.TyApp () a (parenT b)

funT :: Type -> Type -> Type
funT = Exts.TyFun ()

parenT :: Type -> Type
parenT = \case
  x@Exts.TyApp {} -> Exts.TyParen () x
  x -> x

listT :: Type -> Type
listT = Exts.TyList ()

forallT :: Context -> Type -> Type
forallT ctx = Exts.TyForall () Nothing (Just ctx)

assertT :: Type -> Context
assertT = Exts.CxSingle () . Exts.TypeA () 

-- Names

nameN :: Text -> Name
nameN = Exts.name . Text.unpack

nameQ :: Text -> QName
nameQ = Exts.UnQual () . nameN

opQ :: Text -> QOp
opQ = Exts.op . Exts.sym . Text.unpack

-- Patterns

varP :: Text -> Pat
varP = Exts.pvar . nameN

-- Deriving

-- Declarations


dataD :: Text -> Bool -> [QualConDecl] -> [Deriving] -> Decl
dataD name isNewtype =
  Exts.DataDecl () arity Nothing (Exts.DHead () (nameN name))
  where
    arity
      | isNewtype = Exts.NewType ()
      | otherwise = Exts.DataType ()

derivingD :: Bool -> [Either Text Text] -> [Deriving]
derivingD isNewtype xs =
  stocks ++ newtypes
  where
    (stocks, newtypes) =
      Bifunctor.bimap
        (derive (Exts.DerivStock ()))
        (derive (if isNewtype then Exts.DerivNewtype () else Exts.DerivAnyclass ()))
        (Either.partitionEithers xs)

    derive strategy = \case
      [] -> []
      ns -> [Exts.Deriving () (Just strategy) (map rule ns)]

    rule name =
      Exts.IRule () Nothing Nothing $
        Exts.IHCon () (nameQ ("Core." <> name))

qualConD :: ConDecl -> QualConDecl
qualConD = Exts.QualConDecl () Nothing Nothing

fieldD :: Text -> Type -> FieldDecl
fieldD name = Exts.FieldDecl () [nameN name]

typeSigD :: Text -> Type -> Decl
typeSigD name = Exts.TypeSig () [nameN name]

funBindD :: [Match] -> InstDecl
funBindD = Exts.InsDecl () . Exts.FunBind ()

patBindD :: Pat -> Exp -> InstDecl
patBindD pat = Exts.InsDecl () . Exts.patBind pat

matchNullaryD :: Text -> Rhs -> Maybe Binds -> Match
matchNullaryD name =
  Exts.Match () (nameN name) []
  
matchWildcardD :: Text -> Maybe Text -> Bool -> Rhs -> Maybe Binds -> Match
matchWildcardD name mvar isEmpty =
  Exts.Match () (nameN name) [patAs patArg]
  where
    patAs =
      case mvar of
        Just var -> Exts.PAsPat () (nameN var)
        Nothing -> id

    patArg =
      if isEmpty
        then Exts.PWildCard ()
        else Exts.PRec () (nameQ name) [Exts.PFieldWildcard ()]

instanceD :: Text -> Text -> [InstDecl] -> Decl
instanceD class' type' =
  Exts.InstDecl () Nothing rule . \case
    [] -> Nothing
    xs -> Just xs
  where
    rule =
      Exts.IRule () Nothing Nothing $
        Exts.IHApp () (Exts.IHCon () (nameQ class')) (conT type')

associatedTypeD :: Text -> Text -> Text -> InstDecl
associatedTypeD name a b =
  Exts.InsType () (conT name `appT` conT a) (conT b)

-- Records

conR :: Text -> [FieldUpdate] -> Exp
conR name = Exts.RecConstr () (nameQ name)

fieldR :: Text -> Exp -> FieldUpdate
fieldR name = Exts.FieldUpdate () (nameQ name)

punR :: Text -> FieldUpdate
punR name = Exts.FieldPun () (nameQ name)

-- Expressions

conE :: Text -> Exp
conE = Exts.Con () . nameQ

varE :: Text -> Exp
varE = Exts.var . nameN

fracE :: Rational -> Exp
fracE n = Exts.Lit () (Exts.Frac () n (show n))

strE :: Text -> Exp
strE = Exts.strE . Text.unpack

applyE :: Exp -> Exp -> Exp
applyE a b = Exts.infixApp a (opQ "Core.$") b

composeE :: Exp -> Exp -> Exp
composeE a b = Exts.infixApp a (opQ "Core..") b

constE :: Exp -> Exp
constE = Exts.app (varE "Core.const")

mconcatE :: [Exp] -> Exp
mconcatE = Exts.app (varE "Core.mconcat") . Exts.listE

memptyE :: Exp
memptyE = varE "Core.mempty"

mappendE :: Exp -> Exp -> Exp
mappendE a b = Exts.infixApp a (opQ "Core.<>") b

applicativeE :: Exp -> [Exp] -> Exp
applicativeE a = \case
  [] -> pureE a
  b : bs -> fmapE a (Foldable.foldl' fapplyE b (map Exts.paren bs))

fmapE :: Exp -> Exp -> Exp
fmapE f a = Exts.infixApp f (opQ "Core.<$>") a

fapplyE :: Exp -> Exp -> Exp
fapplyE f a = Exts.infixApp f (opQ "Core.<*>") a

pureE :: Exp -> Exp
pureE = Exts.app (varE "Core.pure")

justE :: Exp -> Exp
justE = Exts.app (varE "Core.Just")

nothingE :: Exp
nothingE = varE "Core.Nothing"

fieldLensE :: Text -> Exp
fieldLensE (Text.unpack -> name) =
  Exts.app (varE "Lens.field")
    . Exts.TypeApp ()
    . Exts.TyPromoted ()
    $ Exts.PromotedString () name name

fieldGetterN :: Exp -> Text
fieldGetterN e = if go e then "Lens.^?" else "Lens.^."
  where
    go = \case
      Exts.App _ x y -> go x || go y
      Exts.InfixApp _ x _ y -> go x || go y
      Exts.Var _ (Exts.UnQual _ (Exts.Ident _ "Lens._last")) -> True
      Exts.Var _ (Exts.UnQual _ (Exts.Ident _ "Lens._Just")) -> True
      _ -> False

-- Guards

guarded :: [GuardedRhs]-> Rhs
guarded = Exts.GuardedRhss ()

unguarded :: Exp -> Rhs
unguarded = Exts.UnGuardedRhs ()

otherwiseE :: Exp -> GuardedRhs
otherwiseE = guardE (varE "Core.otherwise")

guardE :: Exp -> Exp -> GuardedRhs
guardE a b = Exts.GuardedRhs () [Exts.qualStmt a] b
