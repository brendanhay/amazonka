{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- Module      : Gen.AST.Data
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.AST.Data
    ( serviceData
    , operationData
    , shapeData
    , waiterData
    ) where

import           Control.Comonad.Cofree
import           Control.Error
import           Control.Lens                 hiding (enum, mapping, (??))
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Trans.State
import           Data.Bifunctor
import           Data.Char                    (isSpace)
import qualified Data.HashMap.Strict          as Map
import qualified Data.HashSet                 as Set
import           Data.List                    (find)
import           Data.List.NonEmpty           (NonEmpty (..))
import           Data.Monoid                  ((<>))
import           Data.String
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Text.Lazy               as LText
import qualified Data.Text.Lazy.Builder       as Build
import           Data.Traversable             (mapAccumL)
import           Gen.AST.Data.Field
import           Gen.AST.Data.Instance
import           Gen.AST.Data.Syntax
import           Gen.AST.TypeOf
import           Gen.Formatting
import           Gen.Protocol
import           Gen.Types
import           HIndent
import           Language.Haskell.Exts.Pretty

import qualified Language.Haskell.Exts        as Exts
import           Language.Haskell.Exts.Build  hiding (pvar, var)
import           Language.Haskell.Exts.SrcLoc (noLoc)
import           Language.Haskell.Exts.Syntax hiding (Int, List, Lit, Var)

operationData :: HasMetadata a Identity
              => a
              -> Operation Identity Ref (Pager Id)
              -> Either Error (Operation Identity SData (Pager Id))
operationData m o = do
    (xa, x)  <- struct (xr ^. refAnn)
    (ya, y)  <- struct (yr ^. refAnn)

    (xd, xs) <- prodData m xa x
    (yd, ys) <- prodData m ya y

    is       <- requestInsts m h xr xs

    cls      <- pp Print $ requestD m h (xr, is) (yr, ys)

    mpage    <- pagerFields m o >>= traverse (pp Print . pagerD xn)

    is' <- maybe id (Map.insert "AWSPager") mpage
         . Map.insert "AWSRequest" cls
        <$> renderInsts p xn is

    return $! o
        { _opInput  = Identity $ Prod False (isStreaming x) xd is'
        , _opOutput = Identity $ Prod (isShared ya) (isStreaming y) yd mempty
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

shapeData :: HasMetadata a Identity
          => a
          -> Shape Solved
          -> Either Error (Maybe SData)
shapeData m (a :< s) = case s of
    _ | s ^. infoException -> Just <$> errorData a (s ^. info)
    Enum   i vs            -> Just <$> sumData p a i vs
    Struct st              -> do
        (d, fs) <- prodData m a st
        is      <- renderInsts p (a ^. annId) (shapeInsts p (a ^. relMode) fs)
        return $! Just $ Prod (isShared a) (isStreaming st) d is
    _                -> return Nothing
  where
    p = m ^. protocol

errorData :: Solved -> Info -> Either Error SData
errorData s i = Fun <$> mk
  where
    mk = Fun' p h
        <$> pp None   (errorS p)
        <*> pp Indent (errorD p status code)

    h = flip fromMaybe (i ^. infoDocumentation)
        . fromString
        . LText.unpack
        $ format ("Prism for " % iprimary % "' errors.") n

    status = i ^? infoError . _Just . errStatus
    code   = fromMaybe (n ^. memberId) (i ^. infoError . _Just . errCode)

    p = n ^. typeId . to (Text.cons '_')
    n = s ^. annId

sumData :: Protocol
        -> Solved
        -> Info
        -> Map Id Text
        -> Either Error SData
sumData p s i vs = Sum (isShared s) <$> mk <*> (Map.keys <$> insts)
  where
    mk = Sum' (n ^. typeId) (i ^. infoDocumentation)
        <$> pp Indent decl
        <*> pure bs

    decl  = dataD n (map con (Map.keys bs)) (derivingOf s)
    con x = conD (ConDecl (ident x) [])
    insts = renderInsts p n $ shapeInsts p (s ^. relMode) []

    n  = s ^. annId
    bs = vs & kvTraversal %~ first (^. branchId (s ^. annPrefix))

prodData :: HasMetadata a Identity
         => a
         -> Solved
         -> StructF (Shape Solved)
         -> Either Error (Prod, [Field])
prodData m s st = (,fields) <$> mk
  where
    mk = Prod' (n ^. typeId) (st ^. infoDocumentation)
        <$> pp Indent decl
        <*> mkCtor
        <*> traverse mkLens fields

    decl = dataD n [recordD ts n fields] (derivingOf s)

    fields :: [Field]
    fields = mkFields m s st

    mkLens :: Field -> Either Error Fun
    mkLens f = Fun' (f ^. fieldLens) (f ^. fieldHelp)
        <$> pp None (lensS ts (s ^. annType) f)
        <*> pp None (lensD f)

    mkCtor :: Either Error Fun
    mkCtor = Fun' (n ^. smartCtorId) mkHelp
        <$> pp None   (ctorS ts n fields)
        <*> pp Indent (ctorD n fields)

    mkHelp :: Help
    mkHelp = fromString
        . LText.unpack
        $ format ("'" % itype % "' smart constructor.") n
    -- <> mkSee

    -- FIXME: Re-add /See:/ documentation for shared types.
    -- mkSee :: LText.Text
    -- mkSee = case r ^. relParents of
    --     [] -> mempty
    --     xs -> mappend "\n\n/See/: "
    --         . LText.intercalate ", "
    --         $ map (format ("'" % itype % "'")) xs

    ts = m ^. timestampFormat . _Identity
    n  = s ^. annId

renderInsts :: Protocol -> Id -> [Inst] -> Either Error (Map Text LText.Text)
renderInsts p n = fmap Map.fromList . traverse go
  where
    go i = (instToText i,) <$> pp Print (instanceD p n i)

serviceData :: HasMetadata a f
            => a
            -> Retry
            -> Either Error Rendered
serviceData m r = pp Indent (serviceD m r)

waiterData :: HasMetadata a Identity
           => a
           -> Map Id (Operation Identity Ref b)
           -> Id
           -> Waiter Id
           -> Either Error WData
waiterData m os n w = do
    o  <- note (missingErr k (k, Map.map _opName os)) $ Map.lookup k os
    wf <- waiterFields m o w
    WData (o ^. opName)
        <$> pp None   (waiterS n wf)
        <*> pp Indent (waiterD n wf)
  where
    missingErr =
        format ("Missing operation "      % iprimary %
                " when rendering waiter " %
                ", possible matches: "    % partial)

    k = w ^. waitOperation

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
        Next x t  -> Next <$> notation m out x <*> token t
--        One  x t  -> One  <$> notation m out x <*> token t
        Many x ts -> Many <$> notation m out x <*> traverse token ts

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
        NonEmpty k   -> NonEmpty <$> key s k
        Choice   x y -> Choice   <$> go s x <*> go s y
        Access   ks  -> fmap Access
            . flip evalStateT s
            . forM ks
            $ \x -> do
                k <- get >>= lift . (`key` x)
                put (skip (shape k))
                return k

    key :: Shape Solved -> Key Id -> Either Error (Key Field)
    key s = \case
        Key  n -> Key  <$> field n s
        Each n -> Each <$> field n s
        Last n -> Last <$> field n s

    field :: Id -> Shape Solved -> Either Error Field
    field n = \case
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

data Ident
    = Indent
    | Print
    | None
      deriving (Eq)

pp :: Pretty a => Ident -> a -> Either Error LText.Text
pp i d
    | i == Indent = bimap e Build.toLazyText (reformat johanTibell Nothing p)
    | otherwise   = pure p
  where
    e = flip mappend (", when formatting datatype: " <> p) . LText.pack

    p = LText.dropWhile isSpace . LText.pack $
        prettyPrintStyleMode s m d

    s = style
        { mode           = PageMode
        , lineLength     = 80
        , ribbonsPerLine = 1.5
        }

    m | i == Print  = defaultMode
      | i == Indent = defaultMode -- Temporary, while hindent speed issues are considered.
      | otherwise   = defaultMode
          { layout  = PPNoLayout
          , spacing = False
          }
