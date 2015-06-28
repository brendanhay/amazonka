{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.GetConsoleOutput
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Gets the console output for the specified instance.
--
-- Instances do not have a physical monitor through which you can view
-- their console output. They also lack physical controls that allow you to
-- power up, reboot, or shut them down. To allow these actions, we provide
-- them through the Amazon EC2 API and command line interface.
--
-- Instance console output is buffered and posted shortly after instance
-- boot, reboot, and termination. Amazon EC2 preserves the most recent 64
-- KB output which is available for at least one hour after the most recent
-- post.
--
-- For Linux instances, the instance console output displays the exact
-- console output that would normally be displayed on a physical monitor
-- attached to a computer. This output is buffered because the instance
-- produces it and then posts it to a store where the instance\'s owner can
-- retrieve it.
--
-- For Windows instances, the instance console output includes output from
-- the EC2Config service.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-GetConsoleOutput.html>
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
    , gcorStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getConsoleOutput' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gcoDryRun'
--
-- * 'gcoInstanceId'
data GetConsoleOutput = GetConsoleOutput'
    { _gcoDryRun     :: !(Maybe Bool)
    , _gcoInstanceId :: !Text
    } deriving (Eq,Read,Show)

-- | 'GetConsoleOutput' smart constructor.
getConsoleOutput :: Text -> GetConsoleOutput
getConsoleOutput pInstanceId =
    GetConsoleOutput'
    { _gcoDryRun = Nothing
    , _gcoInstanceId = pInstanceId
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
gcoDryRun :: Lens' GetConsoleOutput (Maybe Bool)
gcoDryRun = lens _gcoDryRun (\ s a -> s{_gcoDryRun = a});

-- | The ID of the instance.
gcoInstanceId :: Lens' GetConsoleOutput Text
gcoInstanceId = lens _gcoInstanceId (\ s a -> s{_gcoInstanceId = a});

instance AWSRequest GetConsoleOutput where
        type Sv GetConsoleOutput = EC2
        type Rs GetConsoleOutput = GetConsoleOutputResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 GetConsoleOutputResponse' <$>
                   (x .@? "instanceId") <*> (x .@? "output") <*>
                     (x .@? "timestamp")
                     <*> (pure s))

instance ToHeaders GetConsoleOutput where
        toHeaders = const mempty

instance ToPath GetConsoleOutput where
        toPath = const "/"

instance ToQuery GetConsoleOutput where
        toQuery GetConsoleOutput'{..}
          = mconcat
              ["Action" =: ("GetConsoleOutput" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _gcoDryRun,
               "InstanceId" =: _gcoInstanceId]

-- | /See:/ 'getConsoleOutputResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gcorInstanceId'
--
-- * 'gcorOutput'
--
-- * 'gcorTimestamp'
--
-- * 'gcorStatus'
data GetConsoleOutputResponse = GetConsoleOutputResponse'
    { _gcorInstanceId :: !(Maybe Text)
    , _gcorOutput     :: !(Maybe Text)
    , _gcorTimestamp  :: !(Maybe ISO8601)
    , _gcorStatus     :: !Status
    } deriving (Eq,Read,Show)

-- | 'GetConsoleOutputResponse' smart constructor.
getConsoleOutputResponse :: Status -> GetConsoleOutputResponse
getConsoleOutputResponse pStatus =
    GetConsoleOutputResponse'
    { _gcorInstanceId = Nothing
    , _gcorOutput = Nothing
    , _gcorTimestamp = Nothing
    , _gcorStatus = pStatus
    }

-- | The ID of the instance.
gcorInstanceId :: Lens' GetConsoleOutputResponse (Maybe Text)
gcorInstanceId = lens _gcorInstanceId (\ s a -> s{_gcorInstanceId = a});

-- | The console output, Base64 encoded.
gcorOutput :: Lens' GetConsoleOutputResponse (Maybe Text)
gcorOutput = lens _gcorOutput (\ s a -> s{_gcorOutput = a});

-- | The time the output was last updated.
gcorTimestamp :: Lens' GetConsoleOutputResponse (Maybe UTCTime)
gcorTimestamp = lens _gcorTimestamp (\ s a -> s{_gcorTimestamp = a}) . mapping _Time;

-- | FIXME: Undocumented member.
gcorStatus :: Lens' GetConsoleOutputResponse Status
gcorStatus = lens _gcorStatus (\ s a -> s{_gcorStatus = a});
