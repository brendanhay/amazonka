{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE ViewPatterns          #-}

-- Module      : Gen.Tree
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.Tree
    ( root
    , fold
    , tests
    , library
    ) where

import           Control.Error
import           Control.Lens              (each, (^.), (^..))
import           Control.Monad
import           Data.Aeson                hiding (json)
import           Data.Functor.Identity
import qualified Data.HashMap.Strict       as Map
import           Data.Monoid
import           Data.Text                 (Text)
import qualified Data.Text.Lazy            as LText
import           Filesystem.Path.CurrentOS hiding (root)
import           Gen.Import
import qualified Gen.JSON                  as JS
import           Gen.Types
import           Prelude                   hiding (mod)
import           System.Directory.Tree     hiding (file)
import           Text.EDE                  hiding (render)

root :: AnchoredDirTree a -> Path
root (p :/ d) = decodeString p </> decodeString (name d)

fold :: Monad m
     => (IOError -> m ())  -- ^ Failures
     -> (Path -> m ())     -- ^ Directories
     -> (Path -> a -> m b) -- ^ Files
     -> AnchoredDirTree a
     -> m (AnchoredDirTree b)
fold h g f (p :/ t) = (p :/) <$> go (decodeString p) t
  where
    go x = \case
        Failed n e  -> h e >> return (Failed n e)
        File   n a  -> File n <$> f (x </> decodeString n) a
        Dir    n cs -> g d >> Dir n <$> mapM (go d) cs
          where
            d = x </> decodeString n

tests :: Path
      -> Templates
      -> Library
      -> Either Error (AnchoredDirTree LText.Text)
tests d Templates{..} l = (encodeString d :/) . dir "test" <$> layout
  where
    layout :: Either Error [DirTree LText.Text]
    layout = traverse sequenceA
        [ dir "gen"
            [ dir "Test"
                [ mod (l ^. testsNS) (testImports l) testsTemplate
                ]
            ]
        ]

    mod :: NS -> [NS] -> Template -> DirTree (Either Error LText.Text)
    mod n is t = module' n is t . pure $ toJSON l

library :: Path
        -> Templates
        -> Library
        -> Either Error (AnchoredDirTree LText.Text)
library d Templates{..} l = (encodeString d :/) . dir lib <$> layout
  where
    layout :: Either Error [DirTree LText.Text]
    layout = traverse sequenceA
        [ dir "src" []
        , dir "examples"
            [ dir "src" []
            , file (lib <-> "examples.cabal") exampleCabalTemplate
            , file "Makefile" exampleMakefileTemplate
            , file "stack.yaml" exampleStackTemplate
            ]
        , dir "gen"
            [ dir "Network"
                [ dir "AWS"
                    [ dir svc $
                        [ mod (l ^. typesNS) (typeImports l) typesTemplate
                        , mod (l ^. waitersNS) (waiterImports l) waitersTemplate
                        ] ++ map op (l ^.. operations . each)
                    , mod (l ^. libraryNS) mempty tocTemplate
                    ]
                ]
            ]
        , file (lib <.> "cabal") cabalTemplate
        , file "README.md" readmeTemplate
        ]

    svc, lib :: Path
    svc = fromText (l ^. serviceAbbrev)
    lib = fromText (l ^. libraryName)

    op :: Operation Identity SData a -> DirTree (Either Error LText.Text)
    op = operation' l operationTemplate

    mod :: NS -> [NS] -> Template -> DirTree (Either Error LText.Text)
    mod n is t = module' n is t (pure env)

    file :: Path -> Template -> DirTree (Either Error LText.Text)
    file p t = file' p t (pure env)

    env :: Value
    env = toJSON l

operation' :: Library
           -> Template
           -> Operation Identity SData a
           -> DirTree (Either Error Rendered)
operation' l t o = module' n is t $ do
    x <- JS.objectErr (show n) o
    y <- JS.objectErr "metadata" (toJSON m)
    return $! Map.insert "operationUrl" (toJSON u) (y <> x)
  where
    n  = operationNS (l ^. libraryNS) (o ^. opName)
    m  = l ^. metadata
    u  = l ^. operationUrl

    is = operationImports l o

module' :: ToJSON a
        => NS
        -> [NS]
        -> Template
        -> Either Error a
        -> DirTree (Either Error LText.Text)
module' ns is t f = file' (filename $ nsToPath ns) t $ do
    x <- f >>= JS.objectErr (show ns)
    return $! x <> fromPairs
        [ "moduleName"    .= ns
        , "moduleImports" .= is
        ]

file' :: ToJSON a
      => Path
      -> Template
      -> Either Error a
      -> DirTree (Either Error LText.Text)
file' (encodeString -> p) t f = File p $
    f >>= JS.objectErr p
      >>= fmapL LText.pack . eitherRender t

dir :: Path -> [DirTree a] -> DirTree a
dir p = Dir (encodeString p)

(<->) :: Path -> Text -> Path
a <-> b = fromText (toTextIgnore a <> "-" <> b)
