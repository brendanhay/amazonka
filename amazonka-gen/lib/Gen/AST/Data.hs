-- |
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
import qualified Control.Lens as Lens
import qualified Control.Monad as Monad
import qualified Control.Monad.Except as Except
import qualified Control.Monad.State.Strict as State
import qualified Data.Bifunctor as Bifunctor
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.Char as Char
import qualified Data.HashMap.Strict.InsOrd as HashMap
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Text.Builder
import qualified Data.Text.Lazy.Builder.Int as Text.Builder.Int
import Gen.AST.Data.Field
import Gen.AST.Data.Instance
import Gen.AST.Data.Syntax
import Gen.Prelude
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

  let (xd, xs) = prodData m xa x
      (yd, ys) = prodData m ya y

  xis <- requestInsts m (_opName o) h xr xs

  mpage <- fmap (prettyPrint Block . pagerD xn) <$> pagerFields m o

  let cls = prettyPrint Block (requestD cfg m h (xr, xis) (yr, ys))
      yis' = mempty
      xis' =
        maybe id (HashMap.insert "AWSPager") mpage
          . HashMap.insert "AWSRequest" cls
          $ ppInsts p xn xis

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
  Maybe SData
shapeData m (a :< s) =
  case s of
    _
      | s ^. infoException ->
        Just (errorData m a (s ^. info))
    Enum i vs ->
      Just (patternData p a i vs)
    Struct st ->
      let (decl, fields) = prodData m a st
          instances = ppInsts p (a ^. annId) (shapeInsts p r fields)
       in Just $ Prod a decl instances
    --
    _other -> Nothing
  where
    p = m ^. protocol
    r = a ^. relMode

errorData ::
  HasMetadata a Identity =>
  a ->
  Solved ->
  Info ->
  SData
errorData m s i = Fun mk
  where
    mk =
      Fun'
        { _funName = p,
          _funDoc = h,
          _funSig = prettyPrint Inline (errorS p),
          _funDecl = prettyPrint Block (errorD m p status code),
          _funPragmas = [Text.Lazy.fromStrict deprecated]
        }

    deprecated =
      "{-# DEPRECATED "
        <> p
        <> " \"Use generic-lens or generic-optics instead.\" #-}"

    h =
      flip fromMaybe (i ^. infoDocumentation) $
        fromString $
          "Prism for '" ++ Text.unpack (memberId n) ++ "' errors."

    status = i ^? infoError . Lens._Just . errStatus
    code = fromMaybe (memberId n) (i ^. infoError . Lens._Just . errCode)

    p = Text.cons '_' (typeId n)
    n = s ^. annId

patternData ::
  Protocol ->
  Solved ->
  Info ->
  InsOrdHashMap Id Text ->
  SData
patternData p s i vs = Sum s mk (HashMap.keys instances)
  where
    mk =
      Sum'
        { _sumName = typeId name,
          _sumDoc = i ^. infoDocumentation,
          _sumDecl = prettyPrint Block decl,
          _sumCtor = constructor,
          _sumPatterns = patterns
        }

    decl =
      dataD
        name
        True
        [ conD (Exts.ConDecl () (ident constructor) [(tycon "Lude.Text")])
        ]
        (derivingOf s)

    name = s ^. annId

    -- Disambiguate the constructor name to avoid clashes with type names.
    constructor = typeId name <> "'"

    patterns =
      flip map (HashMap.toList vs) $ \(k, v) ->
        Pattern'
          { _patName = branchId (s ^. annPrefix) k,
            _patText = v
          }

    instances = ppInsts p name (shapeInsts p (s ^. relMode) [])

prodData ::
  HasMetadata a Identity =>
  a ->
  Solved ->
  StructF (Shape Solved) ->
  (Prod, [Field])
prodData m s st = (mk, fields)
  where
    mk =
      Prod'
        { _prodName = typeId n,
          _prodDoc = st ^. infoDocumentation,
          _prodDecl = prettyPrint Inline decl,
          _prodDeriving = derivingClauses,
          _prodCtor = mkCtor,
          _prodLenses = map mkLens fields,
          _prodAccessors = map mkAccessor fieldDecls,
          _prodDeps = dependencies
        }

    decl =
      dataD
        n
        (length fields == 1)
        [conD (Exts.ConDecl () (ident constructor) [])]
        []

    constructor = typeId n <> "'"

    (name, fieldDecls) = recordD m n fields

    derivingClauses =
      map (prettyPrint Inline) $
        derivingD (length fields == 1) (derivingOf s)

    mkAccessor (label, decl, help) =
        Accessor'
          { _accessorName = label,
            _accessorDecl = prettyPrint Inline decl,
            _accessorDoc = help
          }

    fields :: [Field]
    fields = mkFields m s st

    mkLens :: Field -> Fun
    mkLens f =
      Fun'
        { _funName = fieldLens f,
          _funDoc =
            fieldHelp f
              <> "\n--\n-- /Note:/ Consider using '"
              <> Help (fieldAccessor f)
              <> "' with <https://hackage.haskell.org/package/generic-lens generic-lens> or "
              <> "<https://hackage.haskell.org/package/generic-optics generic-optics> instead.",
          _funSig = prettyPrint Inline (lensS m (s ^. annType) f),
          _funDecl = prettyPrint Inline (lensD m (s ^. annType) f),
          _funPragmas = [Text.Lazy.fromStrict (lensDeprecated f)]
        }

    mkCtor :: Fun
    mkCtor =
      Fun'
        { _funName = smartCtorId n,
          _funDoc = mkHelp,
          _funSig = prettyPrint Inline (ctorS m n fields) & addParamComments fields,
          _funDecl = prettyPrint Block (ctorD n fields),
          _funPragmas = []
        }

    mkHelp :: Help
    mkHelp =
      Help $
        "Creates a value of '"
          <> typeId n
          <> "' with the minimum fields required to make a request."

    -- FIXME: dirty hack to render smart ctor parameter haddock comments.
    addParamComments :: [Field] -> Rendered -> Rendered
    addParamComments fs =
      Text.Lazy.replace " :: " "\n    :: "
        . Text.Lazy.intercalate "\n    -> "
        . zipWith rel ps
        . map Text.Lazy.strip
        . Text.Lazy.splitOn "->"
      where
        rel Nothing t = t
        rel (Just p) t = t <> " -- ^ '" <> Text.Lazy.fromStrict (fieldAccessor p) <> "'"

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
            stripped =
              fromMaybe x $
                Text.stripPrefix "(Maybe " =<< Text.stripSuffix ")" x

    n = s ^. annId

ppInsts :: Protocol -> Id -> [Inst] -> InsOrdHashMap Text Text.Lazy.Text
ppInsts p n =
  HashMap.fromList
    . map (\i -> (instToText i, prettyPrint Block (instanceD p n i)))

serviceData ::
  HasMetadata a Identity =>
  a ->
  Retry ->
  Fun
serviceData m r =
  Fun'
    { _funName = m ^. serviceConfig,
      _funDoc = Help help,
      _funSig = prettyPrint Inline (serviceS m),
      _funDecl = prettyPrint Block (serviceD m r),
      _funPragmas = []
    }
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
  InsOrdHashMap Id (Operation Identity Ref b) ->
  Id ->
  Waiter Id ->
  Either String WData
waiterData m os n w = do
  o <- maybe (Left missingErr) Right (HashMap.lookup key os)
  wf <- waiterFields m o w

  let c =
        Fun'
          { _funName = smartCtorId n,
            _funDoc = Help help,
            _funSig = prettyPrint Inline (waiterS n wf),
            _funDecl = prettyPrint Block (waiterD n wf),
            _funPragmas = []
          }

  pure $! WData (typeId n) (_opName o) c
  where
    missingErr =
      "Missing operation "
        ++ show key
        ++ " when rendering waiter, possible matches: "
        ++ show (partial key (HashMap.map _opName os))

    help =
      Text.Lazy.toStrict $
        Text.Builder.toLazyText $
          "Polls 'Network.AWS."
            <> Text.Builder.fromText (m ^. serviceAbbrev)
            <> "."
            <> Text.Builder.fromText (typeId key)
            <> "' every "
            <> Text.Builder.Int.decimal (_waitDelay w)
            <> " seconds until a "
            <> "successful state is reached. An error is returned after "
            <> Text.Builder.Int.decimal (_waitAttempts w)
            <> " failed checks."

    key = _waitOperation w

waiterFields ::
  HasMetadata a Identity =>
  a ->
  Operation Identity Ref b ->
  Waiter Id ->
  Either String (Waiter Field)
waiterFields m o = Lens.traverseOf (waitAcceptors . Lens.each) go
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
      Infix lens e -> Infix lens <$> go s e
      Deref ks ->
        fmap Deref . flip State.evalStateT s . Monad.forM ks $ \x -> do
          k <- State.get >>= lift . (`key` x)
          State.put (skip (shape k))
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
         in maybe (Left (missingErr n (identifier a) fields)) Right $
              List.find ((n ==) . _fieldId) fields
      _ -> Except.throwError (descendErr n)

    shape :: Key Field -> Shape Solved
    shape =
      Lens.view (fieldRef . refAnn) . \case
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

data Layout
  = Block
  | Inline
  deriving stock (Eq)

prettyPrint :: Pretty a => Layout -> a -> Rendered
prettyPrint layout d =
  Text.Lazy.dropWhile Char.isSpace . Text.Lazy.pack $
    Exts.prettyPrintWithMode mode d
  where
    mode
      | layout == Block = Exts.defaultMode
      | otherwise =
        Exts.defaultMode
          { Exts.layout = Exts.PPInLine,
            Exts.spacing = False
          }
