module Gen.Tree
  ( fold,
    populate,
  )
where

import qualified Control.Lens as Lens
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.List as List
import qualified Data.Text as Text
import Gen.Import
import qualified Gen.JSON as JSON
import Gen.Prelude hiding (mod)
import Gen.Types
import System.Directory.Tree (AnchoredDirTree ((:/)), DirTree (..))
import qualified System.FilePath as FilePath
import Text.EDE (Template)
import qualified Text.EDE as EDE

fold ::
  MonadFail m =>
  -- | Directories
  (FilePath -> m ()) ->
  -- | Files
  (FilePath -> a -> m ()) ->
  AnchoredDirTree a ->
  m FilePath
fold handleDir handleFile (p :/ tree) = go p tree >> pure p
  where
    go path = \case
      Failed _n err -> fail (show err)
      File name x -> handleFile (path </> name) x
      Dir name xs -> handleDir dir >> traverse_ (go dir) xs
        where
          dir = path </> name

type Touch = Either Rendered Rendered

populate ::
  FilePath ->
  Templates ->
  Library ->
  Either String (AnchoredDirTree Touch)
populate d Templates {..} l = (d :/) . Dir lib <$> layout
  where
    layout :: Either String [DirTree Touch]
    layout =
      traverse sequenceA $
        [ Dir "src" $
            -- Supress cabal warnings about directories listed that don't exist.
            [ touch ".gitkeep" blankTemplate mempty
            ],
          Dir "gen" $
            [ Dir "Amazonka" $
                [ Dir svc $
                    [ Dir "Types" (mapMaybe shape (l ^.. shapes . Lens.each)),
                      mod (l ^. typesNS) (typeImports l) typesTemplate,
                      mod (l ^. waitersNS) (waiterImports l) waitersTemplate,
                      mod (l ^. lensNS) (lensImports l) lensTemplate
                    ]
                      ++ map op (l ^.. operations . Lens.each),
                  mod (l ^. libraryNS) mempty tocTemplate
                ]
            ],
          Dir "test" $
            [ mod "Main" (testImports l) testMainTemplate,
              Dir "Test" $
                [ Dir "Amazonka" $
                    [ touch (l ^. serviceAbbrev <> ".hs") testNamespaceTemplate $
                        EDE.fromPairs
                          [ "moduleName"
                              .= ("Test.Amazonka." <> l ^. serviceAbbrev)
                          ],
                      Dir svc $
                        [ touch "Internal.hs" testInternalTemplate $
                            EDE.fromPairs
                              [ "moduleName"
                                  .= ("Test.Amazonka." <> l ^. serviceAbbrev <> ".Internal")
                              ]
                        ],
                      Dir "Gen" $
                        [ mod (l ^. fixturesNS) (fixtureImports l) fixturesTemplate
                        ]
                    ]
                ]
            ],
          Dir "fixture" (concatMap fixture (l ^.. operations . Lens.each)),
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
            [ "method" .= (o ^. opHttp . method),
              "endpointPrefix" .= (l ^. endpointPrefix)
            ]
      ]
      where
        n = typeId (_opName o)

    mod :: NS -> [NS] -> Template -> DirTree (Either String Touch)
    mod n is t = write $ module' n is t (pure env)

    file :: FilePath -> Template -> DirTree (Either String Touch)
    file p t = write $ render p t (pure env)

    env :: Aeson.Value
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
  render (FilePath.takeFileName (nsToPath ns)) tmpl $ do
    x <- f >>= JSON.objectErr (show ns)
    pure $! x
      <> EDE.fromPairs
        [ "moduleName" .= ns,
          "moduleImports" .= is,
          "templateName" .= (templateName ns)
        ]
  where
    templateName (NS xs) = List.last xs

render ::
  ToJSON a =>
  FilePath ->
  Template ->
  Either String a ->
  DirTree (Either String Rendered)
render p tmpl f =
  File p (f >>= JSON.objectErr p >>= EDE.eitherRender tmpl)

touch :: Text -> Template -> Aeson.Object -> DirTree (Either String Touch)
touch f tmpl env =
  File (Text.unpack f) $
    bimap id Left (EDE.eitherRender tmpl env)

write :: DirTree (Either e b) -> DirTree (Either e (Either a b))
write = fmap (second Right)
