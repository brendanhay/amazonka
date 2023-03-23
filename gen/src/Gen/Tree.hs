module Gen.Tree
  ( fold,
    populate,
  )
where

import qualified Control.Lens as Lens
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import qualified Data.Aeson.Types as Aeson.Types
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as Text
import Gen.Import
import qualified Gen.JSON as JSON
import Gen.Prelude hiding (mod)
import Gen.Types
import System.Directory.Tree
  ( AnchoredDirTree ((:/)),
    DirTree (Dir, Failed, File),
  )
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
            [touch ".gitkeep" blankTemplate mempty],
          Dir "gen" $
            [ Dir "Amazonka" $
                [ Dir svc $
                    [ Dir
                        "Types"
                        ( concat
                            [ mapMaybe shape $ l ^.. shapes . traverse,
                              mapMaybe bootShape $ l ^.. shapes . traverse
                            ]
                        ),
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
                        fromPairs
                          [ "moduleName"
                              .= ("Test.Amazonka." <> l ^. serviceAbbrev)
                          ],
                      Dir svc $
                        [ touch "Internal.hs" testInternalTemplate $
                            fromPairs
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
          file "LICENSE" licenseTemplate,
          file "README.md" readmeTemplate
        ]

    svc, lib :: FilePath
    svc = Text.unpack (l ^. serviceAbbrev)
    lib = Text.unpack (l ^. libraryName)

    op :: Operation Identity SData a -> DirTree (Either String Touch)
    op = write . operation' l operationTemplate

    shape :: SData -> Maybe (DirTree (Either String Touch))
    shape s = (\t -> write $ shape' l t s) <$> template
      where
        template = case s of
          Prod _ _ _ -> Just productTemplate
          Sum _ _ _ -> Just sumTemplate
          Fun _ -> Nothing

    bootShape :: SData -> Maybe (DirTree (Either String Touch))
    bootShape s = (\t -> write $ bootShape' l t s) <$> template
      where
        template = case s of
          Prod _ p _
            | _prodName p `elem` Set.map snd (l ^. cuts') ->
              Just bootProductTemplate
          _ -> Nothing

    fixture :: Operation Identity SData a -> [DirTree (Either String Touch)]
    fixture o =
      [ touch (n <> "Response.proto") blankTemplate mempty,
        touch (n <> ".yaml") fixtureRequestTemplate $
            fromPairs
              [ "method" .= (o ^. opHttp . method),
                "endpointPrefix" .= (l ^. endpointPrefix)
              ]
      ]
      where
        n = typeId (_opName o)

    mod :: NS -> [NS] -> Template -> DirTree (Either String Touch)
    mod name imports template = write $ module' Module {..}

    file :: FilePath -> Template -> DirTree (Either String Touch)
    file p t = write $ render p t env

    env :: Either String Aeson.Value
    env = pure $ Aeson.toJSON l

operation' ::
  Library ->
  Template ->
  Operation Identity SData a ->
  DirTree (Either String Rendered)
operation' l template o =
  module'
    Module
      { name,
        imports = operationImports l o,
        template,
        env = do
          x <- JSON.objectErr (show name) o
          y <- JSON.objectErr "metadata" (Aeson.toJSON $ l ^. metadata)
          pure $! y <> x
      }
  where
    name = operationNS (l ^. libraryNS) (o ^. opName)

shape' ::
  Library ->
  Template ->
  SData ->
  DirTree (Either String Rendered)
shape' l template s =
  module'
    Module
      { name = (l ^. typesNS) <> ((mkNS . typeId) $ identifier s),
        imports = imports s,
        template,
        env
      }
  where
    imports (Prod _ prod _) = productImports l prod
    imports (Sum _ _ _) = sumImports l
    imports _ = []

    env = pure $! Aeson.object ["shape" .= s]

bootShape' ::
  Library ->
  Template ->
  SData ->
  DirTree (Either String Rendered)
bootShape' l template s =
  bootModule'
    Module
      { name = (l ^. typesNS) <> ((mkNS . typeId) $ identifier s),
        imports =
          [ "qualified Amazonka.Data as Data",
            "qualified Amazonka.Prelude as Prelude"
          ],
        template,
        env
      }
  where
    env = pure $! Aeson.object ["shape" .= s]

-- | Substitutions for a module.
data Module a = Module
  { name :: NS,
    imports :: [NS],
    template :: Template,
    env :: Either String a
  }

module' :: ToJSON a => Module a -> DirTree (Either String Rendered)
module' Module {..} =
  render (FilePath.takeFileName (nsToPath name)) template $ do
    x <- env >>= JSON.objectErr (show name)
    pure $! x <> fromPairs pairs
  where
    templateName (NS xs) = List.last xs

    pairs =
      [ "moduleName" .= name,
        "moduleImports" .= imports,
        "templateName" .= templateName name
      ]

bootModule' :: ToJSON a => Module a -> DirTree (Either String Rendered)
bootModule' Module {..} =
  render (FilePath.takeFileName (nsToPath name) <> "-boot") template $ do
    x <- env >>= JSON.objectErr (show name)
    pure $! x <> fromPairs pairs
  where
    templateName (NS xs) = List.last xs

    pairs =
      [ "moduleName" .= name,
        "moduleImports" .= imports,
        "templateName" .= templateName name
      ]

render ::
  ToJSON a =>
  FilePath ->
  Template ->
  Either String a ->
  DirTree (Either String Rendered)
render p tmpl a =
  File p (a >>= JSON.objectErr p >>= EDE.eitherRender tmpl . Aeson.KeyMap.toHashMapText)

touch :: Text -> Template -> Aeson.Object -> DirTree (Either String Touch)
touch f tmpl env =
  File (Text.unpack f) $
     bimap id Left (EDE.eitherRender tmpl (Aeson.KeyMap.toHashMapText env))

write :: DirTree (Either e b) -> DirTree (Either e (Either a b))
write = fmap (second Right)

fromPairs :: [Aeson.Types.Pair]  -> Aeson.Types.Object
fromPairs = Aeson.KeyMap.fromHashMapText . EDE.fromPairs
