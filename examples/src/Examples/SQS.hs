{-# LANGUAGE OverloadedStrings #-}

-- Module      : Examples.SQS
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Examples.SQS where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.AWS
import Examples.Internal
import Network.AWS.SQS

integration :: Bool -> IO (Either Error [Message])
integration dbg = do
    env  <- discoverEnv dbg
    name <- getTimestamp

    -- Wait 5 seconds, finally, catch, or MonadRetry et al.
    -- should be used instead in non-example code.
    let feebleWait = say "Waiting 5 seconds..." >> liftIO (threadDelay 5000000)

    runAWST env $ do
        -- Create the queue, ignoring the response so
        -- getQueueUrl can be utilised.
        say "Creating " name
        void $ send (createQueue name)

        feebleWait

        -- Get the SQS URL for the queue name.
        say "Retreving URL for " name
        url <- view gqurQueueUrl <$> send (getQueueUrl name)

        say "Found URL " url

        feebleWait

        -- Send a message to the newly created queue.
        say "Sending 'hello' to " url
        void $ send (sendMessage url "hello")

        feebleWait

        -- Receive the sent message, waiting a maximum of 2 minutes
        -- until it becomes available.
        say "Receiving messages from " url
        view rmrMessages <$> send (receiveMessage url & rmWaitTimeSeconds ?~ 20)
