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

module Compiler.Tree where

import           Compiler.AST
import           Compiler.Types
import           Control.Error
import           Control.Lens              ((^.))
import           Data.Aeson
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
             -> SemVer
             -> Templates
             -> API
             -> AnchoredDirTree LazyText
populateTree d v Templates{..} api =
    encodeString d :/ dir lib
        [ dir "src" []
        , dir "examples"
            [ dir "src" []
            -- , file (lib <//> "-examples.cabal") exampleCabalTemplate (Object mempty)
            -- , file "Makefile" exampleMakefileTemplate (Object mempty)
            ]
        -- , dir "gen"
        --     [ dir "Network"
        --         [ dir "AWS"
        --             [ dir abbrev
        --                 [ file "Types.hs" typesTemplate (Object mempty)
        --                 , file "Waiters.hs" waitersTemplate (Object mempty)
        --                 ] -- ++ map (file ) []
        --             , file (abbrev <.> "hs") serviceTemplate (Object mempty)
        --             ]
        --         ]
        --     ]
        , file (lib <.> "cabal") cabalTemplate
--        , file "README.md" readmeTemplate (Object mempty)
        ]
  where
    abbrev = fromText (api ^. serviceAbbreviation)
    lib    = fromText (api ^. libraryName)


    file   = render json
    json   = toJSON api

    -- Types:
    --   key        = name
    --   value      =
    --     type     = <shape_type>
    --     ctor     = Text
    --     pretty   = Text
    --     lenses[] = Text

    -- types = do
    --     let ss = Map.fromList . catMaybes . map (\(k, v) -> (k,) <$> AST.transform k v) $ Map.toList (s ^. svcShapes)
    --     ds <- traverse AST.json ss
    --     render (t ^. fileTypes $ proto) $ object
    --         [ "namespace" .= Text.pack "Network.AWS.<service>.Types"
    --         , "service"   .= object
    --             [ "abbrev" .= view svcAbbrev s
    --             , "error"  .= Text.pack "<service>Error"
    --             ]
    --         , "shapes"    .= ds
    --         ]

--    operations      = map f . Map.toList $ s ^. svcOperations
      -- where
      --   f (k, _) = (fromText k <.> "hs", EDE.eitherRender file mempty)

dir :: Path -> [DirTree a] -> DirTree a
dir p = Dir (encodeString p)

render :: Value -> Path -> Template -> DirTree LazyText
render v (encodeString -> f) x =
    case note ("Error serialising params: " ++ show v) (fromValue v)
        >>= eitherRender x of
        Right t -> File   f t
        Left  e -> Failed f ex
          where
            ex = mkIOError userErrorType (e ++ "\nRender") Nothing (Just f)

(<//>) :: Path -> Text -> Path
a <//> b = fromText (toTextIgnore a <> b)
