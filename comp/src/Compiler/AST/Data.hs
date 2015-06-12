{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : Compiler.AST.Data
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.AST.Data
    ( operationData
    , shapeData
    ) where

import           Compiler.AST.Data.Field
import           Compiler.AST.Data.Instance
import           Compiler.AST.Data.Syntax
import           Compiler.AST.TypeOf
import           Compiler.Formatting
import           Compiler.Protocol
import           Compiler.Types
import           Control.Comonad.Cofree
import           Control.Lens                 hiding (enum, mapping, (??))
import           Control.Monad
import           Data.Bifunctor
import           Data.Char                    (isSpace)
import qualified Data.HashMap.Strict          as Map
import qualified Data.HashSet                 as Set
import           Data.Monoid                  hiding (Product, Sum)
import           Data.String
import           Data.Text                    (Text)
import qualified Data.Text.Lazy               as LText
import qualified Data.Text.Lazy.Builder       as Build
import           HIndent
import           Language.Haskell.Exts.Pretty

operationData :: HasMetadata a Identity
              => a
              -> Operation Identity Ref
              -> Either Error (Operation Identity Data)
operationData m o = do
    (xa, x)  <- struct (xr ^. refAnn)
    (ya, y)  <- struct (yr ^. refAnn)

    (xd, xs) <- prodData m xa x
    (yd, ys) <- prodData m ya y

    is       <- requestInsts m h xn xs
    cls      <- pp Print $ requestD m h (xr, is) (yr, ys)
    is'      <- Map.insert "AWSRequest" cls <$> renderInsts p xn is

    return $! o
        { _opInput  = Identity $ Prod False xd is'
        , _opOutput = Identity $ Prod (isShared ya) yd mempty
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
    xn = o ^. inputName

shapeData :: HasMetadata a Identity
          => a
          -> Shape Solved
          -> Either Error (Maybe Data)
shapeData m (a :< s) = case s of
    Enum   i vs -> Just <$> sumData p a i vs
    Struct st   -> do
        (d, fs) <- prodData m a st
        is      <- renderInsts p (a ^. annId) (shapeInsts p (a ^. relMode) fs)
        return $! Just $ Prod (isShared a) d is
    _                -> return Nothing
  where
    p = m ^. protocol

sumData :: Protocol
        -> Solved
        -> Info
        -> Map Id Text
        -> Either Error Data
sumData p s i vs = Sum (isShared s) <$> mk <*> (Map.keys <$> insts)
  where
    mk = Sum' (n ^. typeId) (i ^. infoDocumentation)
        <$> pp Indent decl
        <*> pure bs

    decl  = dataDecl n (map conDecl (Map.keys bs)) (derivingOf s)
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

    decl = dataDecl n [recDecl ts n fields] (derivingOf s)

    fields :: [Field]
    fields = mkFields m s st

    mkLens :: Field -> Either Error Fun
    mkLens f = Fun (f ^. fieldLens) (f ^. fieldHelp)
        <$> pp None (lensSig ts (s ^. annType) f)
        <*> pp None (lensDecl f)

    mkCtor :: Either Error Fun
    mkCtor = Fun (n ^. smartCtorId) mkHelp
        <$> pp None   (ctorSig ts n fields)
        <*> pp Indent (ctorDecl n fields)

    mkHelp :: Help
    mkHelp = fromString
        . LText.unpack
        $ format ("'" % itype % "' smart constructor.") n
--       <> mkSee

    -- mkSee :: LText.Text
    -- mkSee = case r ^. relParents of
    --     [] -> mempty
    --     xs -> mappend "\n\n/See/: "
    --         . LText.intercalate ", "
    --         $ map (format ("'" % itype % "'")) xs

    ts = m ^. timestampFormat . _Identity
    n  = s ^. annId
    r  = s ^. annRelation

renderInsts :: Protocol -> Id -> [Inst] -> Either Error (Map Text LText.Text)
renderInsts p n = fmap Map.fromList . traverse go
  where
    go i = (instToText i,) <$> pp Print (instanceD p n i)

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

    m | i == Print = defaultMode
      | otherwise  = defaultMode
          { layout  = PPNoLayout
          , spacing = False
          }
