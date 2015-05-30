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
    ( dataType
    ) where

import           Compiler.AST.Data.Field
import           Compiler.AST.Data.Instance
import           Compiler.AST.Data.Syntax
import           Compiler.Formatting
import           Compiler.Protocol
import           Compiler.Types
import           Control.Comonad.Cofree
import           Control.Lens                 hiding (enum, mapping, (??))
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

dataType :: Protocol -> Shape Solved -> Either Error (Maybe Data)
dataType proto s = case unwrap s of
    Enum   i vs -> Just <$> enum i vs
    Struct st   -> Just <$> struct st
    _           -> return Nothing
  where
    n  = s ^. annId
    r  = s ^. annRelation
    p  = s ^. annPrefix
    t  = s ^. annType
    ds = s ^. annDerive

    enum :: Info -> Map Id Text -> Either Error Data
    enum i vs = Sum <$> sum <*> pure (sumInsts proto r)
      where
        sum = Sum' (n ^. typeId) (i ^. infoDocumentation)
            <$> render (dataDecl n (map conDecl (Map.keys branches)) ds)
            <*> pure branches

        branches :: Map Text Text
        branches = vs & kvTraversal %~ first (^. ctorId p)

    struct :: StructF (Shape Solved) -> Either Error Data
    struct st =
        case s ^. related of
            RReq _ _ h -> Req
                <$> prod
                <*> pure (requestFunction proto h fields)
                <*> mkInsts (requestInsts proto h fields)

            RRes {} -> Res
                <$> prod
                <*> pure (responseFunction proto fields)
                <*> traverse plain (responseExps proto fields)

            _ -> Prod <$> prod <*> mkInsts (prodInsts proto s fields)
      where
        prod = Prod' (n ^. typeId) (st ^. infoDocumentation)
            <$> render (dataDecl n [recDecl n fields] ds)
            <*> mkCtor
            <*> traverse mkLens fields

        mkCtor :: Either Error Fun
        mkCtor = Fun (n ^. smartCtorId) help
            <$> plain  (ctorSig  n fields)
            <*> render (ctorDecl n fields)
          where
            help = fromString
                . LText.unpack
                $ format ("'" % itype % "' smart constructor.") n
               <> see

            see = case r ^. relParents . to Set.toList of
                [] -> mempty
                xs -> mappend "\n\n/See/: "
                    . LText.intercalate ", "
                    $ map (format ("'" % itype % "'")) xs

        mkLens :: Field -> Either Error Fun
        mkLens f = Fun (f ^. fieldLens) (f ^. fieldHelp)
            <$> plain (lensSig t f)
            <*> plain (lensDecl  f)

        mkInsts :: Either Error [Inst] -> Either Error (Map Text [LText.Text])
        mkInsts f = Map.fromList <$> (f >>= traverse mkInst)

        mkInst :: Inst -> Either Error (Text, [LText.Text])
        mkInst i = (instToText i,) <$> traverse plain (instanceExps proto i)

        fields :: [Field]
        fields = mkFields p st

render, plain :: Pretty a => a -> Either Error LText.Text
render = pretty True
plain  = pretty False

pretty :: Pretty a => Bool -> a -> Either Error LText.Text
pretty fmt d
    | fmt       = bimap e Build.toLazyText (reformat johanTibell Nothing p)
    | otherwise = pure p
  where
    e = flip mappend (", when formatting datatype: " <> p) . LText.pack

    p = LText.dropWhile isSpace . LText.pack $
        prettyPrintStyleMode s m d

    s = style
        { mode           = PageMode
        , lineLength     = 80
        , ribbonsPerLine = 1.5
        }

    m = defaultMode
        { layout  = PPNoLayout
        , spacing = False
        }
