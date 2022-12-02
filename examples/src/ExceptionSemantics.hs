{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module ExceptionSemantics where

import Amazonka
import Amazonka.DynamoDB
import Control.Exception.Lens
import Control.Lens
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Generics.Product
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.IO

exceptions ::
  -- | Region to operate in.
  Region ->
  -- | DynamoDB table name, shouldn't exist.
  Text ->
  IO ()
exceptions reg n = do
  lgr <- newLogger Info stdout
  env <-
    newEnv discover <&> set (field @"logger") lgr . set (field @"region") reg

  let scan = newScan n & field @"attributesToGet" ?~ "foo" :| []

  runResourceT $ do
    sayLn $ "Listing all tables in region " <> fromRegion reg
    runConduit $
      paginate env newListTables
        .| CL.concatMap (view (field @"tableNames" . _Just))
        .| CL.mapM_ (sayLn . mappend "Table: ")

    say "Throwing deliberate IOError ... "
    Left _ <-
      trying _IOException $
        throwM (userError "deliberate!")
    sayLn "OK!"

    say $ "Performing table scan of " <> n <> " using 'send' ... "
    s <- try $ send env scan
    sayLn "OK!"

    say "Displaying error while scanning using 'paginate': "
    display s

    say $ "Performing table scan of " <> n <> " using 'paginate': "
    p <- try . runConduit $ paginate env scan .| CL.consume
    sayLn "OK!"

    say "Displaying error while scanning using 'paginate': "
    display p

  sayLn "Exited ResourceT scope."

display :: (MonadIO m, Show a) => Either SomeException a -> m ()
display = sayLn . either str (mappend "No error! " . str)

str :: Show a => a -> Text
str = Text.pack . show

sayLn :: MonadIO m => Text -> m ()
sayLn = liftIO . Text.putStrLn

say :: MonadIO m => Text -> m ()
say = liftIO . Text.putStr
