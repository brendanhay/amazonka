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
import           Filesystem.Path.CurrentOS
import           System.Directory.Tree
import           System.IO.Error
import           Text.EDE

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
            , tmpl (lib <> "-examples.cabal") exampleCabalTemplate (Object mempty)
            , tmpl "Makefile" exampleMakefileTemplate (Object mempty)
            ]
        , dir "gen"
            [ dir "Network"
                [ dir "AWS"
                    [ dir abbrev
                        [ tmpl "Types.hs" typesTemplate (Object mempty)
                        , tmpl "Waiters.hs" waitersTemplate (Object mempty)
                        ] -- ++ map (tmpl ) []
                    , tmpl (abbrev <.> "hs") serviceTemplate (Object mempty)
                    ]
                ]
            ]
        , tmpl (lib <.> "cabal") cabalTemplate (Object mempty)
        , tmpl "README.md" readmeTemplate (Object mempty)
        ]
  where
    abbrev = fromText (api ^. serviceAbbreviation)
    lib    = fromText (api ^. libraryName)

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
    --     render (t ^. tmplTypes $ proto) $ object
    --         [ "namespace" .= Text.pack "Network.AWS.<service>.Types"
    --         , "service"   .= object
    --             [ "abbrev" .= view svcAbbrev s
    --             , "error"  .= Text.pack "<service>Error"
    --             ]
    --         , "shapes"    .= ds
    --         ]

--    operations      = map f . Map.toList $ s ^. svcOperations
      -- where
      --   f (k, _) = (fromText k <.> "hs", EDE.eitherRender tmpl mempty)

dir :: Path -> [DirTree a] -> DirTree a
dir p = Dir (encodeString p)

tmpl :: Path -> Template -> Value -> DirTree LazyText
tmpl (encodeString -> f) x v =
    case note ("Error serialising params: " ++ show v) (fromValue v)
        >>= eitherRender x of
        Right t -> File   f t
        Left  e -> Failed f ex
          where
            ex = mkIOError userErrorType (e ++ "\nRender") Nothing (Just f)

