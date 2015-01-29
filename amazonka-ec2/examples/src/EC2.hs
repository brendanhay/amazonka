{-# LANGUAGE OverloadedStrings #-}

-- Module      : EC2
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module EC2 where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.AWS
import qualified Data.ByteString.Builder as Build
import           Data.Monoid
import qualified Data.Text               as Text
import qualified Data.Text.IO            as Text
import           Data.Time.Clock.POSIX
import           Network.AWS.EC2
import           System.IO

example :: IO (Either Error RunInstancesResponse)
example = do
    lgr <- newLogger Debug stdout
    env <- getEnv Ireland Discover <&> envLogger .~ lgr
    ts  <- Text.pack . show <$> getTimestamp
    runAWST env $ do
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

        case i of
            Right x -> return x
            Left  e -> do
                say "Failed to Launch Instance " e
                say "Deleting SecurityGroup " g
                void . send $ deleteSecurityGroup & dsgGroupId ?~ g

                say "Deleting KeyPair " ts
                void . send $ deleteKeyPair ts

                throwAWSError e

say :: Show a => Build.Builder -> a -> AWST IO ()
say msg = info . mappend msg . Build.stringUtf8 . show

getTimestamp :: IO Integer
getTimestamp = truncate <$> getPOSIXTime
