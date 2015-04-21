{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}

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
import           Compiler.EDE
import           Compiler.Types
import           Control.Error
import           Control.Lens              ((^.))
import           Filesystem.Path.CurrentOS
import           System.Directory.Tree     hiding (file)

rootDir :: AnchoredDirTree a -> Path
rootDir (p :/ d) = decodeString p </> decodeString (name d)

populateTree :: Monad m
             => Path
             -> SemVer
             -> Templates
             -> API
             -> Compiler m (AnchoredDirTree LazyText)
populateTree d v t api =
    return $! encodeString d :/ dir lib
        [ dir "src" []
        , dir "examples"
            [ dir "src" []
        --     , file (fromText $ s ^. svcLibrary <> "-examples.cabal") cabalExample
        --     , file "Makefile" makefileExample
            ]
        , dir "gen"
            [ dir "Network"
                [ dir "AWS"
                    [ dir abbrev
                        [ -- file "Types.hs" types
                        ]
        --                 , file "Waiters.hs" waiters
        --                 ] ++ map (uncurry file) operations
        --             , file (abbrev <.> "hs") service
                    ]
                ]
            ]
--        , file (lib <.> "cabal") cabal
--        , file "README.md" readme
        ]
  where
    -- proto  = s ^. metaProtocol

    abbrev = fromText (api ^. serviceAbbreviation)
    lib    = fromText (api ^. libraryName)

    -- cabalExample    = render (t ^. tmplCabalExample)    test
    -- makefileExample = render (t ^. tmplMakefileExample) test

    -- service         = render (t ^. tmplService)         test
    -- waiters         = render (t ^. tmplWaiters)         test

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

    -- cbl = toJSON (Cabal v s)

--    operations      = map f . Map.toList $ s ^. svcOperations
      -- where
      --   f (k, _) = (fromText k <.> "hs", EDE.eitherRender tmpl mempty)

    -- tmpl = t ^. tmplOperation $ proto

dir :: Path -> [DirTree a] -> DirTree a
dir p = Dir (encodeString p)

file :: Path -> a -> DirTree a
file p = File (encodeString p)

-- reformat :: (Monad m, MonadError String m) => LText.Text -> m LText.Text
-- reformat = either throwError (return . Build.toLazyText)
--     . HIndent.reformat HIndent.johanTibell Nothing

-- (<//>) :: FilePath -> DirTree a -> DirTree a
-- p <//> d = dir p [d]
