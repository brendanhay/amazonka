{-# LANGUAGE OverloadedStrings #-}

-- Module      : Examples.EC2
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Examples.EC2 where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.AWS
import           Data.Monoid
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.IO            as Text
import           Data.Time.Clock.POSIX
import           Network.AWS.EC2
import           System.IO

launch :: IO ()
launch = do
    hSetBuffering stdout LineBuffering

    ts  <- Text.pack . show <$> getTimestamp
    env <- getEnv NorthVirginia Discover
    r   <- runAWST env $ do
        say "Create KeyPair " ts
        k <- send (createKeyPair ts)

        let key    = Text.unpack ts ++ ".pem"
            trusty = "ami-5895242f"

        say "Writing KeyPair material to " key
        liftIO (Text.writeFile key (k ^. ckprKeyMaterial))

        say "Create SecurityGroup " ts
        g <- view csgrGroupId <$>
            send (createSecurityGroup ts "amazonka-examples")

        say "Authorizing SSH on SecurityGroup " g
        void . send $ authorizeSecurityGroupIngress
            & asgiGroupId    ?~ g
            & asgiIpProtocol ?~ "tcp"
            & asgiFromPort   ?~ 22
            & asgiToPort     ?~ 22
            & asgiCidrIp     ?~ "0.0.0.0/22"

        say "Launching Instance with ImageId " trusty
        i <- sendCatch $ runInstances trusty 1 1
            & riKeyName          ?~ ts
            & riInstanceType     ?~ T2_Micro
            & riSecurityGroupIds .~ [g]

        either (\e -> do
                   say "Failed to Launch Instance " e
                   say "Deleting SecurityGroup " g
                   void . send $ deleteSecurityGroup & dsgGroupId ?~ g
                   say "Deleting KeyPair " ts
                   void . send $ deleteKeyPair ts
                   throwAWSError e)
               return
               i

    print r
  where
    getTimestamp :: IO Integer
    getTimestamp = truncate <$> getPOSIXTime

    say :: Show a => Text -> a -> AWS ()
    say msg = liftIO . Text.putStrLn . mappend msg . Text.pack . show
