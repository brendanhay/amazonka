-- |
-- Module      : Gen.Tree
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Gen.Tree
  ( root,
    fold,
    populate,
  )
where

import qualified Control.Lens as Lens
import qualified Control.Monad.Fail as Fail
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Object, Value, (.=))
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import Gen.Import
import qualified Gen.JSON as JSON
import Gen.Prelude
import Gen.Types
import System.Directory.Tree (AnchoredDirTree ((:/)), DirTree)
import qualified System.Directory.Tree as DirTree
import System.FilePath ((<.>), (</>))
import qualified System.FilePath as FilePath
import Text.EDE (Template)
import qualified Text.EDE as EDE

root :: AnchoredDirTree a -> FilePath
root (p :/ d) = p </> DirTree.name d

fold ::
  Fail.MonadFail m =>
  -- | Directories
  (FilePath -> m ()) ->
  -- | Files
  (FilePath -> a -> m b) ->
  AnchoredDirTree a ->
  m (AnchoredDirTree b)
fold g f (p :/ t) = (p :/) <$> go (p) t
  where
    go x = \case
      DirTree.Failed n e -> fail (show e) >> pure (DirTree.Failed n e)
      DirTree.File n a -> DirTree.File n <$> f (x </> n) a
      DirTree.Dir n cs -> g d >> DirTree.Dir n <$> mapM (go d) cs
        where
          d = x </> n

type Touch = Either Rendered Rendered

populate ::
  FilePath ->
  Templates ->
  Library ->
  Either String (AnchoredDirTree Touch)
populate d Templates {..} l = (d :/) . dir lib <$> layout
  where
    layout :: Either String [DirTree Touch]
    layout =
      traverse
        sequenceA
        [ dir
            "src"
            -- Supress cabal warnings about directories listed that don't exist.
            [ touch ".gitkeep" blankTemplate mempty
            ],
          dir
            "gen"
            [ dir
                "Network"
                [ dir
                    "AWS"
                    [ dir svc $
                        [ dir "Types" (mapMaybe shape (l ^.. shapes . Lens.each)),
                          mod (l ^. typesNS) (typeImports l) typesTemplate,
                          mod (l ^. waitersNS) (waiterImports l) waitersTemplate
                        ]
                          ++ map op (l ^.. operations . Lens.each),
                      mod (l ^. libraryNS) mempty tocTemplate
                    ]
                ]
            ],
          dir
            "test"
            [ mod "Main" (testImports l) testMainTemplate,
              dir
                "Test"
                [ dir
                    "AWS"
                    [ touch (l ^. serviceAbbrev <> ".hs") testNamespaceTemplate $
                        EDE.fromPairs
                          [ "moduleName"
                              .= ("Test.AWS." <> l ^. serviceAbbrev)
                          ],
                      dir
                        svc
                        [ touch "Internal.hs" testInternalTemplate $
                            EDE.fromPairs
                              [ "moduleName"
                                  .= ("Test.AWS." <> l ^. serviceAbbrev <> ".Internal")
                              ]
                        ],
                      dir
                        "Gen"
                        [ mod (l ^. fixturesNS) (fixtureImports l) fixturesTemplate
                        ]
                    ]
                ]
            ],
          dir "fixture" $
            concatMap fixture (l ^.. operations . Lens.each),
          file (lib <.> "cabal") cabalTemplate,
          file "README.md" readmeTemplate
        ]

    svc, lib :: FilePath
    svc = Text.unpack (l ^. serviceAbbrev)
    lib = Text.unpack (l ^. libraryName)

    op :: Operation Identity SData a -> DirTree (Either String Touch)
    op = write . operation' l operationTemplate

    shape :: SData -> Maybe (DirTree (Either String Touch))
    shape s = (\t -> (write . shape' l t) s) <$> template s
      where
        template (Prod _ _ _) = Just productTemplate
        template (Sum _ _ _) = Just sumTemplate
        template (Fun _) = Nothing

    fixture :: Operation Identity SData a -> [DirTree (Either String Touch)]
    fixture o =
      [ touch (n <> "Response.proto") blankTemplate mempty,
        touch (n <> ".yaml") fixtureRequestTemplate $
          EDE.fromPairs
            [ "method" .= (o ^. opHTTP . method),
              "endpointPrefix" .= (l ^. endpointPrefix)
            ]
      ]
      where
        n = typeId (_opName o)

    mod :: NS -> [NS] -> Template -> DirTree (Either String Touch)
    mod n is t = write $ module' n is t (pure env)

    file :: FilePath -> Template -> DirTree (Either String Touch)
    file p t = write $ file' p t (pure env)

    env :: Value
    env = Aeson.toJSON l

operation' ::
  Library ->
  Template ->
  Operation Identity SData a ->
  DirTree (Either String Rendered)
operation' l t o = module' n is t $ do
  x <- JSON.objectErr (show n) o
  y <- JSON.objectErr "metadata" (Aeson.toJSON m)
  pure $! y <> x
  where
    n = operationNS (l ^. libraryNS) (o ^. opName)
    m = l ^. metadata

    is = operationImports l o

shape' ::
  Library ->
  Template ->
  SData ->
  DirTree (Either String Rendered)
shape' l t s = module' n (is s) t $ pure env
  where
    n = (l ^. typesNS) <> ((mkNS . typeId) $ identifier s)

    is (Prod _ prod _) = productImports l prod
    is (Sum _ _ _) = sumImports l
    is _ = []

    env = Aeson.object ["shape" .= s]

module' ::
  ToJSON a =>
  NS ->
  [NS] ->
  Template ->
  Either String a ->
  DirTree (Either String Rendered)
module' ns is tmpl f =
  file' (FilePath.takeFileName (nsToPath ns)) tmpl $ do
    x <- f >>= JSON.objectErr (show ns)
    pure $! x
      <> EDE.fromPairs
        [ "moduleName" .= ns,
          "moduleImports" .= Set.fromList is,
          "templateName" .= (templateName ns)
        ]
  where
    templateName (NS xs) = last xs

file' ::
  ToJSON a =>
  FilePath ->
  Template ->
  Either String a ->
  DirTree (Either String Rendered)
file' (p) tmpl f =
  DirTree.File p $
    f >>= JSON.objectErr p
      >>= EDE.eitherRender tmpl

dir :: FilePath -> [DirTree a] -> DirTree a
dir p = DirTree.Dir p

write :: DirTree (Either e b) -> DirTree (Either e (Either a b))
write = fmap (second Right)

touch :: Text -> Template -> Object -> DirTree (Either String Touch)
touch f tmpl env =
  DirTree.File (Text.unpack f) (Left <$> EDE.eitherRender tmpl env)
