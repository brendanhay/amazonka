{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE ViewPatterns          #-}

-- Module      : Compiler.Tree
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.Tree
    ( root
    , fold
    , populate
    ) where

import qualified Compiler.JSON             as JS
import           Compiler.Types
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
import           Prelude                   hiding (mod)
import           System.Directory.Tree     hiding (file)
import           System.IO.Error
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

populate :: Path
         -> Templates
         -> Library
         -> Either Error (AnchoredDirTree LText.Text)
populate d Templates{..} l = ((encodeString d :/) . dir lib) <$> layout
  where
    layout :: Either Error [DirTree LText.Text]
    layout = traverse sequenceA
        [ dir "src" []
        , dir "examples"
            [ dir "src" []
            , file (lib <-> "examples.cabal") exampleCabalTemplate
            , file "Makefile" exampleMakefileTemplate
            ]
        , dir "gen"
            [ dir "Network"
                [ dir "AWS"
                    [ dir svc $
                        [ mod "Types" typesTemplate
                        , mod "Waiters" waitersTemplate
                        ] ++ map op (l ^.. operations . each)
                    , mod mempty tocTemplate
                    ]
                ]
            ]
        , file (lib <.> "cabal") cabalTemplate
        , file "README.md" readmeTemplate
        ]

    svc, lib :: Path
    svc = fromText (l ^. serviceAbbrev)
    lib = fromText (l ^. libraryName)

    ns :: NS
    ns = l ^. namespace

    env, met :: Value
    env = toJSON l
    met = toJSON (l ^. metadata)

    file :: Path -> Template -> DirTree (Either Error LText.Text)
    file p t = file' p t (pure env)

    mod :: NS -> Template -> DirTree (Either Error LText.Text)
    mod n t = module' (ns <> n) t (pure env)

    op :: Operation Identity Data -> DirTree (Either Error LText.Text)
    op o = module' n operationTemplate $ do
        x <- JS.objectErr (show n) o
        m <- JS.objectErr "metadata" met
        return $! y <> x <> m
      where
        n = ns <> o ^. operationNS
        y = fromPairs
            [ "operationUrl"     .= (l ^. operationUrl)
            , "operationImports" .= (l ^. operationImports)
            ]

dir :: Path -> [DirTree a] -> DirTree a
dir p = Dir (encodeString p)

module' :: ToJSON a
        => NS
        -> Template
        -> Either Error a
        -> DirTree (Either Error LText.Text)
module' ns t f = file' (filename $ nsToPath ns) t $
    Map.insert "moduleName" (toJSON ns)
       <$> (f >>= JS.objectErr (show ns))

file' :: ToJSON a
      => Path
      -> Template
      -> Either Error a
      -> DirTree (Either Error LText.Text)
file' (encodeString -> p) t f = File p $
    f >>= JS.objectErr p
      >>= fmapL LText.pack . eitherRender t

(<->) :: Path -> Text -> Path
a <-> b = fromText (toTextIgnore a <> "-" <> b)
