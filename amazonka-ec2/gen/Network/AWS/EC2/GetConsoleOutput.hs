{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.EC2.GetConsoleOutput
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gets the console output for the specified instance. Instances do not have a
-- physical monitor through which you can view their console output. They also
-- lack physical controls that allow you to power up, reboot, or shut them
-- down. To allow these actions, we provide them through the Amazon EC2 API
-- and command line interface. Instance console output is buffered and posted
-- shortly after instance boot, reboot, and termination. Amazon EC2 preserves
-- the most recent 64 KB output which is available for at least one hour after
-- the most recent post. For Linux/Unix instances, the instance console output
-- displays the exact console output that would normally be displayed on a
-- physical monitor attached to a machine. This output is buffered because the
-- instance produces it and then posts it to a store where the instance's
-- owner can retrieve it. For Windows instances, the instance console output
-- displays the last three system event log errors.
module Network.AWS.EC2.GetConsoleOutput
    (
    -- * Request
      GetConsoleOutput
    -- ** Request constructor
    , getConsoleOutput
    -- ** Request lenses
    , gcoDryRun
    , gcoInstanceId

    -- * Response
    , GetConsoleOutputResponse
    -- ** Response constructor
    , getConsoleOutputResponse
    -- ** Response lenses
    , gcorInstanceId
    , gcorOutput
    , gcorTimestamp
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data GetConsoleOutput = GetConsoleOutput
    { _gcoDryRun     :: Maybe Bool
    , _gcoInstanceId :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'GetConsoleOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gcoDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'gcoInstanceId' @::@ 'Text'
--
getConsoleOutput :: Text -- ^ 'gcoInstanceId'
                 -> GetConsoleOutput
getConsoleOutput p1 = GetConsoleOutput
    { _gcoInstanceId = p1
    , _gcoDryRun     = Nothing
    }

gcoDryRun :: Lens' GetConsoleOutput (Maybe Bool)
gcoDryRun = lens _gcoDryRun (\s a -> s { _gcoDryRun = a })

-- | The ID of the instance.
gcoInstanceId :: Lens' GetConsoleOutput Text
gcoInstanceId = lens _gcoInstanceId (\s a -> s { _gcoInstanceId = a })

instance ToQuery GetConsoleOutput

instance ToPath GetConsoleOutput where
    toPath = const "/"

data GetConsoleOutputResponse = GetConsoleOutputResponse
    { _gcorInstanceId :: Maybe Text
    , _gcorOutput     :: Maybe Text
    , _gcorTimestamp  :: Maybe RFC822
    } deriving (Eq, Ord, Show, Generic)

-- | 'GetConsoleOutputResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gcorInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'gcorOutput' @::@ 'Maybe' 'Text'
--
-- * 'gcorTimestamp' @::@ 'Maybe' 'UTCTime'
--
getConsoleOutputResponse :: GetConsoleOutputResponse
getConsoleOutputResponse = GetConsoleOutputResponse
    { _gcorInstanceId = Nothing
    , _gcorTimestamp  = Nothing
    , _gcorOutput     = Nothing
    }

-- | The ID of the instance.
gcorInstanceId :: Lens' GetConsoleOutputResponse (Maybe Text)
gcorInstanceId = lens _gcorInstanceId (\s a -> s { _gcorInstanceId = a })

-- | The console output, Base64 encoded.
gcorOutput :: Lens' GetConsoleOutputResponse (Maybe Text)
gcorOutput = lens _gcorOutput (\s a -> s { _gcorOutput = a })

-- | The time the output was last updated.
gcorTimestamp :: Lens' GetConsoleOutputResponse (Maybe UTCTime)
gcorTimestamp = lens _gcorTimestamp (\s a -> s { _gcorTimestamp = a })
    . mapping _Time

instance AWSRequest GetConsoleOutput where
    type Sv GetConsoleOutput = EC2
    type Rs GetConsoleOutput = GetConsoleOutputResponse

    request  = post "GetConsoleOutput"
    response = xmlResponse $ \h x -> GetConsoleOutputResponse
        <$> x %| "instanceId"
        <*> x %| "output"
        <*> x %| "timestamp"
