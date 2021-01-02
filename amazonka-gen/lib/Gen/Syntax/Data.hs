-- |
-- Module      : Gen.Syntax.Data
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Gen.Syntax.Data
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
import qualified Control.Monad.Reader as Reader
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
import Debug.Trace
import Gen.Prelude
import qualified Gen.Syntax.Decl as Decl
import qualified Gen.Syntax.Exts as Exts
import qualified Gen.Text
import Gen.Types
import Gen.Types.Field
import Gen.Types.Instance
import qualified Language.Haskell.Exts as Haskell.Exts
import Language.Haskell.Exts.Pretty (Pretty)

operationData ::
  HasMetadata meta Identity =>
  Config ->
  meta ->
  Operation Identity Ref (Pager Id) ->
  Either String (Operation Identity SData (Pager Id))
operationData cfg m o = do
  (xa, x) <- struct (xr ^. refAnn)
  (ya, y) <- struct (yr ^. refAnn)

  let (xd, xs) = prodData m xa x
      (yd, ys) = prodData m ya y

  xis <- requestInsts m (_opName o) h xr xs

  mpage <- fmap (Exts.renderBlock . Decl.classAWSPagerD xn) <$> pagerFields m o

  let cls = Exts.renderBlock (Decl.classAWSRequestD cfg m h (xr, xis, xs) (yr, ys))
      yis' = mempty
      xis' = prettyInstances p xn xis ++ [cls] ++ maybeToList mpage

  pure
    $! o
      { _opInput = Identity (Prod (xa & relShared .~ 0) xd xis'),
        _opOutput = Identity (Prod ya yd yis')
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
  HasMetadata meta Identity =>
  meta ->
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
          instances = prettyInstances p (a ^. annId) (shapeInsts p r fields)
       in Just $ Prod a decl instances
    --
    _other -> Nothing
  where
    p = m ^. protocol
    r = a ^. relMode

errorData ::
  HasMetadata meta Identity =>
  meta ->
  Solved ->
  Info ->
  SData
errorData meta solved info = Fun mk
  where
    mk =
      Fun'
        { _funName = prefixed,
          _funDoc = doc,
          _funDecls =
            [ Exts.renderInline (Decl.sigErrorD prefixed),
              Exts.renderBlock (Decl.funErrorD meta prefixed status code),
              Exts.inlineableD prefixed,
              Exts.renderInline $
                Exts.deprecatedD [prefixed] "Use generic-lens or generic-optics instead"
            ]
        }

    doc =
      flip fromMaybe (info ^. infoDocumentation) $
        fromString $
          "Prism for '" ++ Text.unpack (memberId name) ++ "' errors."

    status = info ^? infoError . Lens._Just . errStatus
    code = fromMaybe (memberId name) (info ^. infoError . Lens._Just . errCode)

    prefixed = Text.cons '_' (typeId name)
    name = solved ^. annId

patternData ::
  Protocol ->
  Solved ->
  Info ->
  InsOrdHashMap Id Text ->
  SData
patternData protocol' solved info branches =
  Sum solved newSum instances
  where
    newSum =
      Sum'
        { _sumName =
            typeId name,
          _sumDoc =
            info ^. infoDocumentation,
          _sumCtor =
            constructor,
          _sumDecl =
            Exts.renderBlock
              . Decl.patternD name constructor
              . Decl.derivingD True
              $ derivingOf solved,
          _sumPatterns =
            flip map (HashMap.toList branches) $ \(k, v) ->
              Pattern'
                { _patName = branchId prefix k,
                  _patText = v
                }
        }

    instances =
      prettyInstances protocol' name $
        shapeInsts protocol' (solved ^. relMode) []

    constructor =
      "from" <> typeId name

    prefix = solved ^. annPrefix
    name = solved ^. annId

prodData ::
  HasMetadata meta Identity =>
  meta ->
  Solved ->
  StructF (Shape Solved) ->
  (Prod, [Field])
prodData meta solved struct =
  (newProd, fields)
  where
    newProd =
      Prod'
        { _prodName =
            typeId name,
          _prodDoc =
            struct ^. infoDocumentation,
          _prodDecl =
            Exts.renderInline (Decl.recordD name isNewtype),
          _prodDeriving =
            map Exts.renderInline $
              Decl.derivingD isNewtype (derivingOf solved),
          _prodCtor =
            Fun'
              { _funName =
                  smartCtorId name,
                _funDoc =
                  Help $
                    "Creates a '"
                      <> typeId name
                      <> "' value with any optional fields omitted.",
                _funDecls =
                  [ addParamComments fields $
                      Exts.renderInline (Decl.sigSmartCtorD meta name fields),
                    Exts.renderBlock (Decl.funSmartCtorD name fields)
                  ]
              },
          _prodAccessors =
            flip map (Decl.fieldsD meta name fields) $ \(label, decl, help) ->
              Accessor'
                { _accessorName = label,
                  _accessorDecl = Exts.renderInline decl,
                  _accessorDoc = help
                },
          _prodLenses =
            flip map fields $ \field ->
              Fun'
                { _funName = fieldLens field,
                  _funDoc =
                    fieldHelp field
                      <> "\n--\n-- /Note:/ Consider using '"
                      <> Help (fieldAccessor field)
                      <> "' with <https://hackage.haskell.org/package/generic-lens generic-lens> or "
                      <> "<https://hackage.haskell.org/package/generic-optics generic-optics> instead.",
                  _funDecls =
                    Exts.renderInline (Decl.sigLensD meta name field) :
                    Exts.renderInline (Decl.funLensD meta name field) :
                    lensPragmas field
                },
          _prodDeps =
            foldMap (typeNames stripOverrideType) fields
        }

    -- FIXME: this is quite probably unecessary? Seems like it exists as
    -- hack for overrides with verbatim type application.
    stripOverrideType x
      | stripped == typeId name = Nothing
      | otherwise = Just x
      where
        stripped =
          Gen.Text.stripPrefix "(Maybe " (Gen.Text.stripSuffix ")" x)

    isNewtype = length fields == 1

    fields = getStructFields meta solved struct

    name = solved ^. annId

-- FIXME: dirty hack to render interleaved smart ctor parameter comments.
addParamComments :: [Field] -> LazyText -> LazyText
addParamComments fields =
  Text.Lazy.replace " :: " "\n    :: "
    . Text.Lazy.intercalate "\n    -> "
    . zipWith link params
    . map Text.Lazy.strip
    . Text.Lazy.splitOn "->"
  where
    link Nothing text = text
    link (Just x) text = text <> " -- ^ '" <> escape (fieldAccessor x) <> "'"

    escape (Text.Lazy.fromStrict -> text) =
      flip Text.Lazy.concatMap text $ \case
        '\'' -> "\\'"
        char -> Text.Lazy.singleton char

    params =
      map Just (filter fieldIsParam fields)
        ++ repeat Nothing

prettyInstances :: Protocol -> Id -> [Inst] -> [Text.Lazy.Text]
prettyInstances protocol' name =
  map Exts.renderBlock
    . Decl.instancesD protocol' name

serviceData ::
  HasMetadata meta Identity =>
  meta ->
  Retry ->
  Fun
serviceData m r =
  Fun'
    { _funName = m ^. serviceConfig,
      _funDoc = Help help,
      _funDecls =
        [ Exts.renderInline (Decl.sigServiceD m),
          Exts.renderBlock (Decl.funServiceD m r)
        ]
    }
  where
    help =
      "API version @"
        <> (m ^. apiVersion)
        <> "@ of the "
        <> (m ^. serviceFullName)
        <> " configuration."

waiterData ::
  HasMetadata meta Identity =>
  meta ->
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
            _funDecls =
              [ Exts.renderInline (Decl.sigWaiterD n wf),
                Exts.renderBlock (Decl.funWaiterD n wf)
              ]
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
  HasMetadata meta Identity =>
  meta ->
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
  HasMetadata meta Identity =>
  meta ->
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
  HasMetadata meta Identity =>
  meta ->
  Shape Solved ->
  Notation Id ->
  Either String (Notation Field)
notation meta start nid =
  snd <$> go start nid
  where
    go ::
      Shape Solved ->
      Notation Id ->
      Either String (Shape Solved, Notation Field)
    go ctx = \case
      Key k -> do
        x <- field ctx k

        pure (x ^. fieldRef . refAnn, Key x)
      --
      App f a -> do
        (ctx1, x) <- go ctx a

        pure (ctx1, App f x)
      --
      Dot a b -> do
        (ctx1, x) <- go ctx a
        (ctx2, y) <- go ctx1 b

        pure (ctx2, Dot x y)
      --
      Alt a b -> do
        (_ctx, x) <- go ctx a
        (_ctx, y) <- go ctx b

        pure (ctx, Alt x y)

    -- "Certificate.DomainValidationOptions[].ValidationStatus.Foo"

    -- (Dot (Key Certificate<Certificate>)
    --   (Dot (App Each (Key DomainValidationOptions<DomainValidationOptions>))
    --     (Dot (Key ValidationStatus<ValidationStatus>) (Key Foo<Foo>))))

    -- "Certificate.DomainValidationOptions[].ValidationStatus",
    -- Struct/DescribeCertificateResponse has member Struct/Certificate
    -- Struct/CertificateDetail has member List/DomainValidationOptions
    -- List/DomainValidationOptions has entry Struct/DomainValidation
    -- Struct/DomainValidation has member ValidationStatus

    field :: Shape Solved -> Id -> Either String Field
    field ctx k =
      case ctx of
        ann :< Struct struct ->
          let fields = getStructFields meta ann struct
           in case List.find ((k ==) . _fieldId) fields of
                Just ok -> pure ok
                Nothing ->
                  Left $
                    "Unable to find field "
                      ++ show k
                      ++ " in members of "
                      ++ show (identifier ctx)
                      ++ " "
                      ++ show (map _fieldId fields)
        --
        ann :< List list ->
          field (list ^. listItem . refAnn) k
        --
        other ->
          Left $
            "Unable to find field "
              ++ show k
              ++ " in non-struct reference "
              ++ show (identifier ctx)
              ++ "\n"
              ++ show nid
              ++ "\n\n"
              ++ show start
              ++ "\n\n"
              ++ show other
