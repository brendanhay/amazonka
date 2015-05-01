{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
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
    ( rootTree
    , foldTree
    , populateTree
    ) where

import           Compiler.AST
import           Compiler.Types
import           Control.Lens              ((^.))
import           Data.Aeson                hiding (json)
import qualified Data.HashMap.Strict       as Map
import           Data.Monoid
import           Data.Text                 (Text)
import           Filesystem.Path.CurrentOS
import           System.Directory.Tree     hiding (file)
import           System.IO.Error
import           Text.EDE                  hiding (render)

rootTree :: AnchoredDirTree a -> Path
rootTree (p :/ d) = decodeString p </> decodeString (name d)

foldTree :: Monad m
         => (IOError -> m ())  -- ^ Failures
         -> (Path -> m ())     -- ^ Directories
         -> (Path -> a -> m b) -- ^ Files
         -> AnchoredDirTree a
         -> m (AnchoredDirTree b)
foldTree h g f (p :/ t) = (p :/) <$> go (decodeString p) t
  where
    go x = \case
        Failed n e  -> h e >> return (Failed n e)
        File   n a  -> File n <$> f (x </> decodeString n) a
        Dir    n cs -> g d >> Dir n <$> mapM (go d) cs
          where
            d = x </> decodeString n

populateTree :: Path
             -> Templates
             -> Library
             -> AnchoredDirTree LazyText
populateTree d Templates{..} l = encodeString d :/ dir lib
    [ dir "src" []
    , dir "examples"
        [ dir "src" []
        , file (lib <-> "examples.cabal") exampleCabalTemplate
        , file "Makefile" exampleMakefileTemplate
        ]
    , dir "gen"
        [ dir "Network"
            [ dir "AWS"
                [ dir abbrev
                    [ mod (ns <> "Types") typesTemplate
--                    , file "Waiters.hs" waitersTemplate (Object mempty)
                    ] -- ++ map (file ) []
                , mod ns serviceTemplate
                ]
            ]
        ]
    , file (lib <.> "cabal") cabalTemplate
    , file "README.md" readmeTemplate
    ]
  where
    abbrev = fromText (l ^. serviceAbbrev)
    lib    = fromText (l ^. libraryName)
    ns     = l ^. namespace

    mod n  = render (Map.insert "moduleName" (toJSON n) env) (filename (nsToPath n))
    file   = render env

    Object env = toJSON l

dir :: Path -> [DirTree a] -> DirTree a
dir p = Dir (encodeString p)

render :: Object -> Path -> Template -> DirTree LazyText
render o (encodeString -> f) x =
    case eitherRender x o of
        Right t -> File   f t
        Left  e -> Failed f ex
          where
            ex = mkIOError userErrorType (e ++ "\nRender") Nothing (Just f)

(<->) :: Path -> Text -> Path
a <-> b = fromText (toTextIgnore a <> "-" <> b)
