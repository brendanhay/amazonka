-- Module      : Gen.Tree
-- Copyright   : (c) 2013-2021 Brendan Hay
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

import Control.Lens (each, (^.), (^..))
import Control.Monad
import Control.Monad.Except
import Data.Aeson hiding (json)
import Data.Bifunctor
import Data.Functor.Identity
import Data.Maybe (mapMaybe)
import Data.Monoid hiding (Sum)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LText
import Filesystem.Path.CurrentOS hiding (FilePath, root)
import Gen.Formatting (failure, shown)
import Gen.Import
import Gen.JSON qualified as JS
import Gen.Types
import System.Directory.Tree hiding (file)
import Text.EDE hiding (failure, render)
import Prelude hiding (mod)

root :: AnchoredDirTree a -> Path
root (p :/ d) = decodeString p </> decodeString (name d)

fold ::
  MonadError Error m =>
  -- | Directories
  (Path -> m ()) ->
  -- | Files
  (Path -> a -> m b) ->
  AnchoredDirTree a ->
  m (AnchoredDirTree b)
fold g f (p :/ t) = (p :/) <$> go (decodeString p) t
  where
    go x = \case
      Failed n e -> failure shown e >> return (Failed n e)
      File n a -> File n <$> f (x </> decodeString n) a
      Dir n cs -> g d >> Dir n <$> mapM (go d) cs
        where
          d = x </> decodeString n

type Touch = Either Rendered Rendered

populate ::
  Path ->
  Templates ->
  Library ->
  Either Error (AnchoredDirTree Touch)
populate d Templates {..} l = (encodeString d :/) . dir lib <$> layout
  where
    layout :: Either Error [DirTree Touch]
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
                        [ dir "Types" $
                            mapMaybe shape (l ^.. shapes . each),
                          mod (l ^. typesNS) (typeImports l) typesTemplate,
                          mod (l ^. waitersNS) (waiterImports l) waitersTemplate
                        ]
                          ++ map op (l ^.. operations . each),
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
                        fromPairs
                          [ "moduleName"
                              .= ("Test.AWS." <> l ^. serviceAbbrev)
                          ],
                      dir
                        svc
                        [ touch "Internal.hs" testInternalTemplate $
                            fromPairs
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
            concatMap fixture (l ^.. operations . each),
          file (lib <.> "cabal") cabalTemplate,
          file "README.md" readmeTemplate
        ]

    svc, lib :: Path
    svc = fromText (l ^. serviceAbbrev)
    lib = fromText (l ^. libraryName)

    op :: Operation Identity SData a -> DirTree (Either Error Touch)
    op = write . operation' l operationTemplate

    shape :: SData -> Maybe (DirTree (Either Error Touch))
    shape s = (\t -> (write . shape' l t) s) <$> template s
      where
        template (Prod _ _ _) = Just productTemplate
        template (Sum _ _ _) = Just sumTemplate
        template (Fun _) = Nothing

    fixture :: Operation Identity SData a -> [DirTree (Either Error Touch)]
    fixture o =
      [ touch (n <> "Response.proto") blankTemplate mempty,
        touch (n <> ".yaml") fixtureRequestTemplate $
          fromPairs
            [ "method" .= (o ^. opHTTP . method),
              "endpointPrefix" .= (l ^. endpointPrefix)
            ]
      ]
      where
        n = typeId (_opName o)

    mod :: NS -> [NS] -> Template -> DirTree (Either Error Touch)
    mod n is t = write $ module' n is t (pure env)

    file :: Path -> Template -> DirTree (Either Error Touch)
    file p t = write $ file' p t (pure env)

    env :: Value
    env = toJSON l

operation' ::
  Library ->
  Template ->
  Operation Identity SData a ->
  DirTree (Either Error Rendered)
operation' l t o = module' n is t $ do
  x <- JS.objectErr (show n) o
  y <- JS.objectErr "metadata" (toJSON m)
  return $! y <> x
  where
    n = operationNS (l ^. libraryNS) (o ^. opName)
    m = l ^. metadata

    is = operationImports l o

shape' ::
  Library ->
  Template ->
  SData ->
  DirTree (Either Error Rendered)
shape' l t s = module' n (is s) t $ pure env
  where
    n = (l ^. typesNS) <> ((mkNS . typeId) $ identifier s)

    is (Prod _ prod _) = productImports l prod
    is (Sum _ _ _) = sumImports l
    is _ = []

    env = object ["shape" .= s]

module' ::
  ToJSON a =>
  NS ->
  [NS] ->
  Template ->
  Either Error a ->
  DirTree (Either Error Rendered)
module' ns is tmpl f =
  file' (filename $ nsToPath ns) tmpl $ do
    x <- f >>= JS.objectErr (show ns)
    return $! x
      <> fromPairs
        [ "moduleName" .= ns,
          "moduleImports" .= is,
          "templateName" .= (templateName ns)
        ]
  where
    templateName (NS xs) = last xs

file' ::
  ToJSON a =>
  Path ->
  Template ->
  Either Error a ->
  DirTree (Either Error Rendered)
file' (encodeString -> p) tmpl f =
  File p $
    f >>= JS.objectErr p
      >>= first LText.pack . eitherRender tmpl

dir :: Path -> [DirTree a] -> DirTree a
dir p = Dir (encodeString p)

write :: DirTree (Either e b) -> DirTree (Either e (Either a b))
write = fmap (second Right)

touch :: Text -> Template -> Object -> DirTree (Either Error Touch)
touch f tmpl env =
  File (Text.unpack f) (bimap LText.pack Left (eitherRender tmpl env))
