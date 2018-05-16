{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- Module      : Gen.AST.Data
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)

module Gen.AST.Data
    ( serviceData
    , operationData
    , shapeData
    , waiterData
    ) where

import Control.Comonad.Cofree
import Control.Error
import Control.Lens              hiding ((:<), List, enum, mapping, (??))
import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans.State

import Data.Bifunctor
import Data.Char      (isSpace)
import Data.List      (find, sort)
import Data.Monoid    ((<>))
import Data.String
import Data.Text      (Text)

import Gen.AST.Data.Field
import Gen.AST.Data.Instance
import Gen.AST.Data.Syntax
import Gen.Formatting
import Gen.Types

import Language.Haskell.Exts.Pretty (Pretty)

import qualified Data.ByteString.Builder      as Build
import qualified Data.ByteString.Char8        as BS8
import qualified Data.HashMap.Strict          as Map
import qualified Data.Text                    as Text
import qualified Data.Text.Encoding           as Text
import qualified Data.Text.Lazy               as LText
import qualified Data.Text.Lazy.Encoding      as LText
import qualified HIndent
import qualified HIndent.Types                as HIndent
import qualified Language.Haskell.Exts.Pretty as Exts
import qualified Language.Haskell.Exts.Syntax as Exts

operationData :: HasMetadata a Identity
              => Config
              -> a
              -> Operation Identity Ref (Pager Id)
              -> Either Error (Operation Identity SData (Pager Id))
operationData cfg m o = do
    (xa, x)  <- struct (xr ^. refAnn)
    (ya, y)  <- struct (yr ^. refAnn)

    (xd, xs) <- prodData m xa x
    (yd, ys) <- prodData m ya y

    xis      <- addInstances xa <$> requestInsts m (_opName o) h xr xs

    cls      <- pp Print $ requestD cfg m h (xr, xis) (yr, ys)
    mpage    <- pagerFields m o >>= traverse (pp Print . pagerD xn)

    yis'     <- renderInsts p yn (responseInsts ys)
    xis'     <-
           maybe id (Map.insert "AWSPager") mpage
         . Map.insert "AWSRequest" cls
        <$> renderInsts p xn xis

    return $! o
        { _opInput  = Identity $ Prod (xa & relShared .~ 0) xd xis'
        , _opOutput = Identity $ Prod ya                    yd yis'
        }
  where
    struct (a :< Struct s) = Right (a, s)
    struct _               = Left $
        format ("Unexpected non-struct shape for operation " % iprimary)
               xn

    p  = m ^. protocol
    h  = o ^. opHTTP

    xr = o ^. opInput  . _Identity
    yr = o ^. opOutput . _Identity

    xn = identifier xr
    yn = identifier yr

shapeData :: HasMetadata a Identity
          => a
          -> Shape Solved
          -> Either Error (Maybe SData)
shapeData m (a :< s) = case s of
    _ | s ^. infoException -> Just <$> errorData m a (s ^. info)
    Enum   i vs            -> Just <$> sumData p a i vs
    Struct st              -> do
        (d, fs) <- prodData m a st
        is      <- renderInsts p (a ^. annId) (addInstances a (shapeInsts p r fs))
        return $! Just $ Prod a d is
    _                -> return Nothing
  where
    p = m ^. protocol
    r = a ^. relMode

addInstances :: TypeOf a => a -> [Inst] -> [Inst]
addInstances s = f isHashable IsHashable . f isNFData IsNFData
  where
    f g x | g s       = (x :)
          | otherwise = id

errorData :: HasMetadata a Identity
          => a
          -> Solved
          -> Info
          -> Either Error SData
errorData m s i = Fun <$> mk
  where
    mk = Fun' p h
        <$> pp None   (errorS p)
        <*> pp Indent (errorD m p status code)

    h = flip fromMaybe (i ^. infoDocumentation)
        . fromString
        . LText.unpack
        $ format ("Prism for " % iprimary % "' errors.") n

    status = i ^? infoError . _Just . errStatus
    code   = fromMaybe (memberId n) (i ^. infoError . _Just . errCode)

    p = Text.cons '_' (typeId n)
    n = s ^. annId

sumData :: Protocol
        -> Solved
        -> Info
        -> Map Id Text
        -> Either Error SData
sumData p s i vs = Sum s <$> mk <*> (Map.keys <$> insts)
  where
    mk = Sum' (typeId n) (i ^. infoDocumentation)
        <$> pp Indent decl
        <*> pure bs

    decl = dataD n (map f . sort $ Map.keys bs) (derivingOf s)
      where
        f x = conD (Exts.ConDecl () (ident x) [])

    insts = renderInsts p n $ shapeInsts p (s ^. relMode) []

    n  = s ^. annId
    bs = vs & kvTraversal %~ first (branchId (s ^. annPrefix))

prodData :: HasMetadata a Identity
         => a
         -> Solved
         -> StructF (Shape Solved)
         -> Either Error (Prod, [Field])
prodData m s st = (,fields) <$> mk
  where
    mk = Prod' (typeId n) (st ^. infoDocumentation)
        <$> pp Indent decl
        <*> mkCtor
        <*> traverse mkLens fields

    decl = dataD n [recordD m n fields] (derivingOf s)

    fields :: [Field]
    fields = mkFields m s st

    mkLens :: Field -> Either Error Fun
    mkLens f = Fun' (fieldLens f) (fieldHelp f)
        <$> pp None (lensS m (s ^. annType) f)
        <*> pp None (lensD f)

    mkCtor :: Either Error Fun
    mkCtor = Fun' (smartCtorId n) mkHelp
        <$> (pp None   (ctorS m n fields) <&> addParamComments fields)
        <*>  pp Indent (ctorD n fields)

    mkHelp :: Help
    mkHelp = Help $
        sformat ("Creates a value of '" % itype %
                "' with the minimum fields required to make a request.")
                n

    -- FIXME: dirty hack to render smart ctor parameter haddock comments.
    addParamComments :: [Field] -> Rendered -> Rendered
    addParamComments fs =
          LText.replace " :: " "\n    :: "
        . LText.intercalate "\n    -> "
        . zipWith rel ps
        . map LText.strip . LText.splitOn "->"
      where
        rel Nothing  t = t
        rel (Just p) t = t <> " -- ^ '" <> LText.fromStrict (fieldLens p) <> "'"

        ps = map Just (filter fieldIsParam fs) ++ repeat Nothing

    n = s ^. annId

renderInsts :: Protocol -> Id -> [Inst] -> Either Error (Map Text LText.Text)
renderInsts p n = fmap Map.fromList . traverse go
  where
    go i = (instToText i,) <$> pp Print (instanceD p n i)

serviceData :: HasMetadata a Identity
            => a
            -> Retry
            -> Either Error Fun
serviceData m r = Fun' (m ^. serviceConfig) (Help h)
    <$> pp None   (serviceS m)
    <*> pp Indent (serviceD m r)
  where
    h = sformat ("API version @" % stext % "@ of the " % stext % " configuration.")
                (m ^. apiVersion) (m ^. serviceFullName)

waiterData :: HasMetadata a Identity
           => a
           -> Map Id (Operation Identity Ref b)
           -> Id
           -> Waiter Id
           -> Either Error WData
waiterData m os n w = do
    o  <- note (missingErr k (k, Map.map _opName os)) $ Map.lookup k os
    wf <- waiterFields m o w
    c  <- Fun' (smartCtorId n) (Help h)
        <$> pp None   (waiterS n wf)
        <*> pp Indent (waiterD n wf)
    return $! WData (typeId n) (_opName o) c
  where
    missingErr = format
        ("Missing operation "      % iprimary %
         " when rendering waiter " %
         ", possible matches: "    % partial)

    h = sformat
        ("Polls 'Network.AWS." % stext % "." % itype %
         "' every " % int % " seconds until a " %
         "successful state is reached. An error is returned after "
         % int % " failed checks.")
        (m ^. serviceAbbrev) k (_waitDelay w) (_waitAttempts w)

    k = _waitOperation w

waiterFields :: HasMetadata a Identity
             => a
             -> Operation Identity Ref b
             -> Waiter Id
             -> Either Error (Waiter Field)
waiterFields m o = traverseOf (waitAcceptors . each) go
  where
    out = o ^. opOutput . _Identity . refAnn

    go :: Accept Id -> Either Error (Accept Field)
    go x = do
        n <- traverse (notation m out) (x ^. acceptArgument)
        return $! x & acceptArgument .~ n

pagerFields :: HasMetadata a Identity
            => a
            -> Operation Identity Ref (Pager Id)
            -> Either Error (Maybe (Pager Field))
pagerFields m o = traverse go (o ^. opPager)
  where
    inp = o ^. opInput  . _Identity . refAnn
    out = o ^. opOutput . _Identity . refAnn

    go :: Pager Id -> Either Error (Pager Field)
    go = \case
        Only    t  -> Only <$> token t
        Next ks t  -> Next <$> traverse (notation m out) ks <*> token t
        Many k  ts -> Many <$> notation m out k <*> traverse token ts

    token :: Token Id -> Either Error (Token Field)
    token (Token x y) = Token
        <$> notation m inp x
        <*> notation m out y

notation :: HasMetadata a Identity
         => a
         -> Shape Solved
         -> Notation Id
         -> Either Error (Notation Field)
notation m = go
  where
    go :: Shape Solved -> Notation Id -> Either Error (Notation Field)
    go s = \case
        NonEmptyList k   -> NonEmptyList <$> key s k
        NonEmptyText k   -> NonEmptyText <$> key s k
        Choice       x y -> Choice       <$> go s x <*> go s y
        Access       ks  -> fmap Access
            . flip evalStateT s
            . forM ks
            $ \x -> do
                k <- get >>= lift . (`key` x)
                put (skip (shape k))
                return k

    key :: Shape Solved -> Key Id -> Either Error (Key Field)
    key s = \case
        Key  n -> Key  <$> field' n s
        Each n -> Each <$> field' n s
        Last n -> Last <$> field' n s

    field' :: Id -> Shape Solved -> Either Error Field
    field' n = \case
        a :< Struct st ->
            note (missingErr n (identifier a) (Map.keys (st ^. members)))
                . find ((n ==) . _fieldId)
                $ mkFields m a st
        _ -> throwError (descendErr n)

    shape :: Key Field -> Shape Solved
    shape = view (fieldRef . refAnn) . \case
        Key  f -> f
        Each f -> f
        Last f -> f

    skip :: Shape a -> Shape a
    skip = \case
        _ :< List x -> x ^. listItem . refAnn
        _ :< Map  x -> x ^. mapValue . refAnn
        x           -> x

    missingErr =
        format ("Unable to find " % iprimary % " in members of " % iprimary % " " % shown)

    descendErr =
        format ("Unable to descend into nested reference " % iprimary)

data PP
    = Indent
    | Print
    | None
      deriving (Eq)

pp :: Pretty a => PP -> a -> Either Error Rendered
pp i d
    | i == Indent = bimap errorMessage render (reformat printed)
    | otherwise   = pure (LText.fromStrict (Text.decodeUtf8 printed))
  where
    render =
        LText.decodeUtf8 . Build.toLazyByteString

    reformat =
        HIndent.reformat HIndent.defaultConfig Nothing Nothing

    errorMessage =
        LText.fromStrict
            . Text.decodeUtf8
            . flip mappend (", when formatting datatype:\n\n" <> printed <> "\n")
            . BS8.pack

    printed =
        BS8.dropWhile isSpace . BS8.pack $
            Exts.prettyPrintStyleMode style mode d

    style = Exts.style
        { Exts.mode           = Exts.PageMode
        , Exts.lineLength     = 80
        , Exts.ribbonsPerLine = 1.5
        }

    mode | i == Print  = Exts.defaultMode
         | i == Indent = Exts.defaultMode
         | otherwise   =
             Exts.defaultMode
                 { Exts.layout  = Exts.PPNoLayout
                 , Exts.spacing = False
                 }
