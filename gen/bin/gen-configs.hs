module Main (main) where

import qualified Control.Monad as Monad
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import Gen.Prelude hiding (words)
import Options.Applicative ((<**>))
import qualified Options.Applicative as Options
import qualified System.FilePath as FilePath
import qualified System.IO as IO
import qualified UnliftIO
import qualified UnliftIO.Directory as UnliftIO
import qualified Gen.WordFrequency

data Options = Options
  { botocoreDir :: FilePath,
    configDir :: FilePath,
    wordFrequencies :: FilePath
  }

parser :: Options.Parser Options
parser =
  Options
    <$> Options.strArgument
      ( Options.metavar "BOTOCORE-PATH"
          <> Options.help "The directory containing botocore service sub-directories."
      )
    <*> Options.strOption
      ( Options.long "configs"
          <> Options.metavar "PATH"
          <> Options.help "The parent config directory."
          <> Options.value "config"
          <> Options.showDefaultWith id
      )
    <*> Options.strOption
      ( Options.long "word-frequencies"
          <> Options.metavar "PATH"
          <> Options.help "An ordered list of words according to frequency."
          <> Options.value "config/word-frequencies"
          <> Options.showDefaultWith id
      )

main :: IO ()
main = do
  Options {botocoreDir, configDir, wordFrequencies} <-
    Options.execParser (Options.info (parser <**> Options.helper) Options.idm)

  let serviceDir = configDir </> "services"
      annexDir = configDir </> "annexes"

  frequencies <-
    fmap Gen.WordFrequency.newTable (ByteString.readFile wordFrequencies) >>= \case
      Left err -> UnliftIO.throwString err
      Right ok -> pure ok

  available <- Set.fromList <$> getAvailable botocoreDir
  configured <- Set.fromList <$> getConfigured serviceDir

  let missing = Set.difference available configured

  Monad.forM_ missing $ \name -> do
    let serviceFile = serviceDir </> name <.> "json"
        annexFile = annexDir </> name <.> "json"

    serviceExists <- UnliftIO.doesFileExist serviceFile

    Monad.unless serviceExists $ do
      ByteString.Lazy.writeFile serviceFile
        . Aeson.encode
        $ Aeson.object
          [ "libraryName" .= mappend "amazonka-" name
          ]

      say $ "Wrote " ++ serviceFile

    annexExists <- UnliftIO.doesFileExist annexFile

    Monad.unless annexExists $ do
      -- FIXME: improve this by sharing amazonka-gen code to select the latest
      -- version of a service definition and then check for the presence of a
      -- serviceAbbreviation key, otherwise falling back to word frequencies.
      let ascii = ByteString.Char8.split '-' (ByteString.Char8.pack name)
          words = concatMap (Gen.WordFrequency.inferWords frequencies) ascii
          abbrev = foldMap (upperHead . Text.Encoding.decodeUtf8) words

      ByteString.Lazy.writeFile annexFile
        . Aeson.encode
        $ Aeson.object
          [ "metadata" .= Aeson.object ["serviceAbbreviation" .= abbrev]
          ]

      say $ "Wrote " ++ annexFile

  say $
    "Found "
      ++ show (Set.size available)
      ++ " service definitions in "
      ++ botocoreDir

  say $
    "Found "
      ++ show (Set.size configured)
      ++ " service configurations in "
      ++ serviceDir

  say $
    "Wrote "
      ++ show (Set.size missing)
      ++ " service and annex configurations to "
      ++ configDir

  say "Done."

say :: String -> IO ()
say = IO.hPutStrLn IO.stderr

getAvailable :: FilePath -> IO [String]
getAvailable path = do
  entries <- UnliftIO.listDirectory path
  Monad.filterM (\name -> UnliftIO.doesDirectoryExist (path </> name)) entries

getConfigured :: FilePath -> IO [String]
getConfigured path = do
  entries <- UnliftIO.listDirectory path
  files <- Monad.filterM (\name -> UnliftIO.doesFileExist (path </> name)) entries
  pure (map FilePath.takeBaseName files)

upperHead :: Text -> Text
upperHead = mapHead Char.toUpper

mapHead :: (Char -> Char) -> Text -> Text
mapHead f text =
  Maybe.fromMaybe text $ do
    (c, cs) <- Text.uncons text
    pure (Text.cons (f c) cs)
