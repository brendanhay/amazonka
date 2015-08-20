{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Example.ExceptionSemantics
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
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
    env <- newEnv r Discover <&> envLogger .~ lgr

    let str :: Show a => a -> Text
        str = Text.pack . show

    let say :: MonadIO m => Text -> m ()
        say = liftIO . Text.putStrLn

    runResourceT $ do
        runAWST env $ do
            say $ "Listing all tables in region " <> toText r
            paginate listTables
                =$= CL.concatMap (view ltrsTableNames)
                 $$ CL.mapM_ (say . mappend "Table: ")

            say "Throwing deliberate IOError"
            Left u <- trying _IOException $
                throwM (userError "deliberate!")

            say $ "Performing table scan of " <> n
            p <- try $
                paginate (scan n & sAttributesToGet ?~ "foo" :| [])
                    $$ CL.consume

            say "Displaying error caught while scanning:"
            case p of
                Left  e -> say $ str (e :: SomeException)
                Right x -> say $ "No error! " <> str x

        say "Exited AWST scope."
    say "Exited ResourceT scope."

