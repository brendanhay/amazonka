{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Example.ExceptionSemantics
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Example.ExceptionSemantics where

import           Control.Exception.Lens
import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Trans.AWS
import           Data.Conduit
import qualified Data.Conduit.List       as CL
import           Data.List.NonEmpty      (NonEmpty (..))
import           Data.Monoid
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.IO            as Text
import           Network.AWS.Data
import           Network.AWS.DynamoDB
import           System.IO

exceptions :: Region -- ^ Region to operate in.
           -> Text   -- ^ DynamoDB table name, shouldn't exist.
           -> IO ()
exceptions r n = do
    lgr <- newLogger Info stdout
    env <- newEnv Discover <&> set envLogger lgr

    let scan' = scan n & sAttributesToGet ?~ "foo" :| []

    runResourceT $ do
        runAWST env . within r $ do
            sayLn $ "Listing all tables in region " <> toText r
            paginate listTables
                =$= CL.concatMap (view ltrsTableNames)
                 $$ CL.mapM_ (sayLn . mappend "Table: ")

            say "Throwing deliberate IOError ... "
            Left _ <- trying _IOException $
                throwM (userError "deliberate!")
            sayLn "OK!"

            say $ "Performing table scan of " <> n <> " using 'send' ... "
            s <- try $ send scan'
            sayLn "OK!"

            say "Displaying error while scanning using 'paginate': "
            display s

            say $ "Performing table scan of " <> n <> " using 'paginate': "
            p <- try $ paginate scan' $$ CL.consume
            sayLn "OK!"

            say "Displaying error while scanning using 'paginate': "
            display p

        sayLn "Exited AWST scope."
    sayLn "Exited ResourceT scope."

display :: (MonadIO m, Show a) => Either SomeException a -> m ()
display = sayLn . either str (mappend "No error! " . str)

str :: Show a => a -> Text
str = Text.pack . show

sayLn :: MonadIO m => Text -> m ()
sayLn = liftIO . Text.putStrLn

say :: MonadIO m => Text -> m ()
say = liftIO . Text.putStr
