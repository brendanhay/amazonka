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
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import qualified Data.Text.IO                    as Text
import           Data.Time.Clock.POSIX
import           Examples.Internal               (discoverEnv)
import           Network.AWS.EC2
import           System.IO

launch :: IO ()
launch = do
    hSetBuffering stdout LineBuffering

    ts  <- Text.pack . show <$> getTimestamp
    env <- discoverEnv
    r   <- runAWST env $ do
        say "Create KeyPair " ts
        kr <- send (createKeyPair ts)

        let kp = Text.unpack ts ++ ".pem"

        say "Writing KeyPair material to " kp
        liftIO (Text.writeFile kp (kr ^. ckprKeyMaterial))

        say "Create SecurityGroup " ts
        gid <- view csgrGroupId <$>
            send (createSecurityGroup "give-temp-name" "amazonka-examples")

        say "Authorizing SSH on SecurityGroup " gid
        void . send $ authorizeSecurityGroupIngress
            & asgiGroupId    ?~ gid
            & asgiIpProtocol ?~ "tcp"
            & asgiFromPort   ?~ 22
            & asgiToPort     ?~ 22
            & asgiCidrIp     ?~ "0.0.0.0/22"

--        kp  <- wait k
        say "KeyPair successfully created: " kp

  --      gid <- wait g
        say "GroupId successfully created: " gid

    print r

  --        createSecurityGroup

  --       send $ runInstances "ami-5895242f" 1 1
  --            & riKeyName =
  --            & riInstanceType = T2Micro
  --            & riSecurityGroups = 

  --        wait

  --        ssh, 

  --        terminateInstances

  --        wait

  --        deleteSecurityGroup

  --        deleteKeyPair

  --   print r

getTimestamp :: IO Integer
getTimestamp = truncate <$> getPOSIXTime

say :: Show a => Text -> a -> AWS ()
say msg = liftIO . Text.putStrLn . mappend msg . Text.pack . show
