{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- Module      : Gen.AST.Data
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)

module Gen.AST.Data
  ( serviceData,
    operationData,
    shapeData,
    waiterData,
  )
where

import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Error
import Control.Lens hiding (List, enum, mapping, (:<), (??))
import qualified Control.Monad as Monad
import qualified Control.Monad.Except as Except
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State
import Data.Bifunctor
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Char as Char
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.Set as Set
import Data.String (IsString (fromString))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Builder as Text.Builder
import qualified Data.Text.Lazy.Builder.Int as Text.Builder.Int
import Gen.AST.Data.Field
import Gen.AST.Data.Instance
import Gen.AST.Data.Syntax
import Gen.Types
import qualified Language.Haskell.Exts as Exts
import Language.Haskell.Exts.Pretty (Pretty)

operationData ::
  HasMetadata a Identity =>
  Config ->
  a ->
  Operation Identity Ref (Pager Id) ->
  Either String (Operation Identity SData (Pager Id))
operationData cfg m o = do
  (xa, x) <- struct (xr ^. refAnn)
  (ya, y) <- struct (yr ^. refAnn)

  (xd, xs) <- prodData m xa x
  (yd, ys) <- prodData m ya y

  xis <- addInstances xa <$> requestInsts m (_opName o) h xr xs

  cls <- pp Print $ requestD cfg m h (xr, xis) (yr, ys)
  mpage <- pagerFields m o >>= traverse (pp Print . pagerD xn)

  yis' <- renderInsts p yn (responseInsts ys)
  xis' <-
    maybe id (HashMap.insert "AWSPager") mpage . HashMap.insert "AWSRequest" cls
      <$> renderInsts p xn xis

  pure
    $! o
      { _opInput = Identity $ Prod (xa & relShared .~ 0) xd xis',
        _opOutput = Identity $ Prod ya yd yis'
      }
  where
    struct (a :< Struct s) = Right (a, s)
    struct _ = Left ("Unexpected non-struct shape for operation " ++ show xn)

    p = m ^. protocol
    h = o ^. opHTTP

    xr = o ^. opInput . _Identity
    yr = o ^. opOutput . _Identity

    xn = identifier xr
    yn = identifier yr

shapeData ::
  HasMetadata a Identity =>
  a ->
  Shape Solved ->
  Either String (Maybe SData)
shapeData m (a :< s) = case s of
  _ | s ^. infoException -> Just <$> errorData m a (s ^. info)
  Enum i vs -> Just <$> sumData p a i vs
  Struct st -> do
    (d, fs) <- prodData m a st
    is <- renderInsts p (a ^. annId) (addInstances a (shapeInsts p r fs))
    pure $! Just $ Prod a d is
  _ -> pure Nothing
  where
    p = m ^. protocol
    r = a ^. relMode

addInstances :: TypeOf a => a -> [Inst] -> [Inst]
addInstances s = f isHashable IsHashable . f isNFData IsNFData
  where
    f g x
      | g s = (x :)
      | otherwise = id

errorData ::
  HasMetadata a Identity =>
  a ->
  Solved ->
  Info ->
  Either String SData
errorData m s i = Fun <$> mk
  where
    mk =
      Fun' p h
        <$> pp None (errorS p)
        <*> pp Print (errorD m p status code)

    h =
      flip fromMaybe (i ^. infoDocumentation) $
        fromString $
          "Prism for '" ++ Text.unpack (memberId n) ++ "' errors."

    status = i ^? infoError . _Just . errStatus
    code = fromMaybe (memberId n) (i ^. infoError . _Just . errCode)

    p = Text.cons '_' (typeId n)
    n = s ^. annId

sumData ::
  Protocol ->
  Solved ->
  Info ->
  Map Id Text ->
  Either String SData
sumData p s i vs = Sum s <$> mk <*> (HashMap.keys <$> insts)
  where
    mk =
      Sum' (typeId n) (i ^. infoDocumentation)
        <$> pp Print decl
        <*> pure ctor
        <*> pure bs

    decl = dataD n [newt] (derivingOf s)
      where
        newt = conD (Exts.ConDecl () (ident ctor) [tyapp (tycon "CI") (tycon "Text")])

    -- Sometimes the values share a name with a type, so we prime the data constructor to avoid clashes.
    ctor = ((<> "'") . typeId) n

    insts = renderInsts p n $ shapeInsts p (s ^. relMode) []

    n = s ^. annId
    bs = vs & kvTraversal %~ first (branchId (s ^. annPrefix))

prodData ::
  HasMetadata a Identity =>
  a ->
  Solved ->
  StructF (Shape Solved) ->
  Either String (Prod, [Field])
prodData m s st = (,fields) <$> mk
  where
    mk =
      Prod' (typeId n) (st ^. infoDocumentation)
        <$> pp Print decl
        <*> mkCtor
        <*> traverse mkLens fields
        <*> pure dependencies

    decl = dataD n [recordD m n fields] (derivingOf s)

    fields :: [Field]
    fields = mkFields m s st

    mkLens :: Field -> Either String Fun
    mkLens f =
      Fun' (fieldLens f) (fieldHelp f)
        <$> pp None (lensS m (s ^. annType) f)
        <*> pp None (lensD f)

    mkCtor :: Either String Fun
    mkCtor =
      Fun' (smartCtorId n) mkHelp
        <$> (pp None (ctorS m n fields) <&> addParamComments fields)
        <*> pp Print (ctorD n fields)

    mkHelp :: Help
    mkHelp =
      Help $
        "Creates a value of '"
          <> typeId n
          <> "' with the minimum fields required to make a request."

    -- FIXME: dirty hack to render smart ctor parameter haddock comments.
    addParamComments :: [Field] -> Rendered -> Rendered
    addParamComments fs =
      LText.replace " :: " "\n    :: "
        . LText.intercalate "\n    -> "
        . zipWith rel ps
        . map LText.strip
        . LText.splitOn "->"
      where
        rel Nothing t = t
        rel (Just p) t = t <> " -- ^ '" <> LText.fromStrict (fieldLens p) <> "'"

        ps = map Just (filter fieldIsParam fs) ++ repeat Nothing

    dependencies = foldMap go fields
      where
        tTypeDep :: Text -> Set.Set Text

        go :: TypeOf a => a -> Set.Set Text
        go f = case (typeOf f) of
          TType x _ -> tTypeDep x
          TLit _ -> Set.empty
          TNatural -> Set.empty
          TStream -> Set.empty
          TMaybe x -> go x
          TSensitive x -> go x
          TList x -> go x
          TList1 x -> go x
          TMap k v -> go k <> go v

        tTypeDep x =
          if (stripped /= typeId n)
            then Set.singleton stripped
            else Set.empty
          where
            stripped = fromMaybe x $ Text.stripPrefix "(Maybe " =<< Text.stripSuffix ")" x

    n = s ^. annId

renderInsts :: Protocol -> Id -> [Inst] -> Either String (Map Text LText.Text)
renderInsts p n = fmap HashMap.fromList . traverse go
  where
    go i = (instToText i,) <$> pp Print (instanceD p n i)

serviceData ::
  HasMetadata a Identity =>
  a ->
  Retry ->
  Either String Fun
serviceData m r =
  Fun' (m ^. serviceConfig) (Help help)
    <$> pp None (serviceS m)
    <*> pp Print (serviceD m r)
  where
    help =
      "API version @"
        <> (m ^. apiVersion)
        <> "@ of the "
        <> (m ^. serviceFullName)
        <> " configuration."

waiterData ::
  HasMetadata a Identity =>
  a ->
  Map Id (Operation Identity Ref b) ->
  Id ->
  Waiter Id ->
  Either String WData
waiterData m os n w = do
  o <- note missingErr (HashMap.lookup key os)
  wf <- waiterFields m o w
  c <-
    Fun' (smartCtorId n) (Help help)
      <$> pp None (waiterS n wf)
      <*> pp Print (waiterD n wf)
  pure $! WData (typeId n) (_opName o) c
  where
    missingErr =
      "Missing operation "
        ++ show key
        ++ " when rendering waiter, possible matches: "
        ++ show (partial key (HashMap.map _opName os))

    help =
      LText.toStrict $
        Text.Builder.toLazyText $
          "Polls 'Network.AWS."
            <> Text.Builder.fromText (m ^. serviceAbbrev)
            <> "."
            <> Text.Builder.fromText (typeId key)
            <> "' every "
            <> Text.Builder.Int.decimal (_waitDelay w)
            <> " seconds until a "
            <> "successful state is reached. An error is pureed after "
            <> Text.Builder.Int.decimal (_waitAttempts w)
            <> " failed checks."

    key = _waitOperation w

waiterFields ::
  HasMetadata a Identity =>
  a ->
  Operation Identity Ref b ->
  Waiter Id ->
  Either String (Waiter Field)
waiterFields m o = traverseOf (waitAcceptors . each) go
  where
    out = o ^. opOutput . _Identity . refAnn

    go :: Accept Id -> Either String (Accept Field)
    go x = do
      n <- traverse (notation m out) (x ^. acceptArgument)
      pure $! x & acceptArgument .~ n

pagerFields ::
  HasMetadata a Identity =>
  a ->
  Operation Identity Ref (Pager Id) ->
  Either String (Maybe (Pager Field))
pagerFields m o = traverse go (o ^. opPager)
  where
    inp = o ^. opInput . _Identity . refAnn
    out = o ^. opOutput . _Identity . refAnn

    go :: Pager Id -> Either String (Pager Field)
    go = \case
      Only t -> Only <$> token t
      Next ks t -> Next <$> traverse (notation m out) ks <*> token t
      Many k ts -> Many <$> notation m out k <*> traverse token ts

    token :: Token Id -> Either String (Token Field)
    token (Token x y) =
      Token
        <$> notation m inp x
        <*> notation m out y

notation ::
  HasMetadata a Identity =>
  a ->
  Shape Solved ->
  Notation Id ->
  Either String (Notation Field)
notation m nid = go nid
  where
    go :: Shape Solved -> Notation Id -> Either String (Notation Field)
    go s = \case
      Choice x y -> Choice <$> go s x <*> go s y
      Length flen e op size -> Length flen <$> go s e <*> pure op <*> pure size
      Deref ks ->
        fmap Deref . flip evalStateT s . Monad.forM ks $ \x -> do
          k <- get >>= lift . (`key` x)
          put (skip (shape k))
          pure k

    key :: Shape Solved -> Key Id -> Either String (Key Field)
    key s = \case
      Key n -> Key <$> field' n s
      Each n -> Each <$> field' n s
      Last n -> Last <$> field' n s

    field' :: Id -> Shape Solved -> Either String Field
    field' n = \case
      a :< Struct st ->
        let fields = mkFields m a st
         in note (missingErr n (identifier a) fields) $
              List.find ((n ==) . _fieldId) fields
      _ -> Except.throwError (descendErr n)

    shape :: Key Field -> Shape Solved
    shape =
      view (fieldRef . refAnn) . \case
        Key f -> f
        Each f -> f
        Last f -> f

    skip :: Shape a -> Shape a
    skip = \case
      _ :< List x -> x ^. listItem . refAnn
      _ :< Map x -> x ^. mapValue . refAnn
      x -> x

    missingErr n parent fields =
      "Unable to find "
        ++ show n
        ++ " in members of "
        ++ show parent
        ++ " "
        ++ show (map _fieldId fields)

    descendErr k =
      "Unable to descend into nested reference "
        ++ show k

data PP
  = Print
  | None
  deriving (Eq)

pp :: Pretty a => PP -> a -> Either String Rendered
pp i d
  | otherwise = pure (LText.fromStrict (Text.decodeUtf8 printed))
  where
    printed =
      BS8.dropWhile Char.isSpace . BS8.pack $
        Exts.prettyPrintWithMode mode d

    mode
      | i == Print = Exts.defaultMode
      | otherwise =
        Exts.defaultMode
          { Exts.layout = Exts.PPInLine,
            Exts.spacing = False
          }
