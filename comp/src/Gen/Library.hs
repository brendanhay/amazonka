{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}

-- Module      : Gen.Library
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.Library where

import           Control.Applicative
import           Control.Error
import           Control.Lens               ((^.))
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.Trans.Except
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.SemVer                as SemVer
import qualified Data.Text.Lazy             as LText
import           Filesystem.Path.CurrentOS  hiding (encode)
import qualified Gen.AST                    as AST
import           Gen.Model
import           Gen.Types
import           Prelude                    hiding (FilePath)
import           System.Directory.Tree      hiding (file)
import qualified Text.EDE                   as EDE

-- tree :: FilePath
--      -> Templates Protocol
--      -> SemVer.Version
--      -> Service (Typed Shape) b
--      -> AnchoredDirTree (Either String LText.Text)

--               (MonadError String f, MonadError String m)
-- => FilePath
-- -> Templates a0
-- -> t
-- -> Service (Typed Shape) b
-- -> f (AnchoredDirTree (m LText.Text))

tree :: (Applicative m, MonadError String m)
     => FilePath
     -> Templates Protocol
     -> SemVer.Version
     -> Service (Derived Shape) (Untyped Ref)
     -> m (AnchoredDirTree LText.Text)
tree d t v s = do
    let c = AST.cabal v s

    env    <- AST.toEnv c

    cabal  <- render (t ^. tmplCabal)  env
    readme <- render (t ^. tmplReadme) env

    types  <- AST.toEnv (c ^. AST.libTypes)
        >>= render (t ^. tmplTypes $ proto)

    return $ encodeString d :/ dir lib
        [ dir "src" []
        -- , dir "examples"
        --     [ dir "src" []
        --     , file (fromText $ s ^. svcLibrary <> "-examples.cabal") cabalExample
        --     , file "Makefile" makefileExample
        --     ]
        , dir "gen"
            [ dir "Network"
                [ dir "AWS"
                    [ dir abbrev $
                        [ file "Types.hs" types
                        ]
        --                 , file "Waiters.hs" waiters
        --                 ] ++ map (uncurry file) operations
        --             , file (abbrev <.> "hs") service
                    ]
                ]
            ]
        , file (lib <.> "cabal") cabal
        , file "README.md" readme
        ]
  where

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

    proto  = s ^. metaProtocol

    abbrev = fromText (s ^. svcAbbrev)
    lib    = fromText (s ^. svcLibrary)

-- reformat :: (Monad m, MonadError String m) => LText.Text -> m LText.Text
-- reformat = either throwError (return . Build.toLazyText)
--     . HIndent.reformat HIndent.johanTibell Nothing

render :: (Monad m, MonadError String m) => EDE.Template -> Value -> m LText.Text
render x v = either throwError return $
    note ("Error serialising params: " ++ show v) (EDE.fromValue v)
        >>= EDE.eitherRenderWith filters x
  where
    filters = mempty -- Map.fromList
--        [ "indent" @: flip indent
        -- , "highlight"    @: highlightType
        -- , "parens"       @: parens
        -- , "wrapped"      @: wrapped
        -- , "concat"       @: (mappend :: Text -> Text -> Text)
        -- , "joinedLength" @: joinedLength
        -- , "member"       @: (elem :: Text -> [Text] -> Bool)
        -- , "waiter"       @: waiter
--        ]

root :: AnchoredDirTree a -> FilePath
root (p :/ d) = decodeString p </> decodeString (name d)

(<//>) :: FilePath -> DirTree a -> DirTree a
p <//> d = dir p [d]

dir :: FilePath -> [DirTree a] -> DirTree a
dir p = Dir (encodeString p)

file :: FilePath -> a -> DirTree a
file p = File (encodeString p)

-- parens :: Text -> Text
-- parens t = "(" <> t <> ")"

-- wrapped :: Text -> Text
-- wrapped t
--     | Text.null t        = t
--     | Text.head t == '[' = t
--     | otherwise          = maybe t (const (parens t)) (Text.findIndex isSpace t)

-- joinedLength :: [Text] -> Text -> Int
-- joinedLength xs sep = sum (map ((+n) . Text.length) xs)
--   where
--     n = Text.length sep

-- waiter :: Text -> Text
-- waiter t
--     | p "DB"    = "db" <> Text.drop 2 t
--     | otherwise = toCamel t
--   where
--     p = flip Text.isPrefixOf t

