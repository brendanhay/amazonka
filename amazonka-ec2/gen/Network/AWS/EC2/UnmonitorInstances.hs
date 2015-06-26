{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.EC2.UnmonitorInstances
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

-- | Disables monitoring for a running instance. For more information about
-- monitoring instances, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-cloudwatch.html Monitoring Your Instances and Volumes>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-UnmonitorInstances.html>
module Network.AWS.EC2.UnmonitorInstances
    (
    -- * Request
      UnmonitorInstances
    -- ** Request constructor
    , unmonitorInstances
    -- ** Request lenses
    , uiDryRun
    , uiInstanceIds

    -- * Response
    , UnmonitorInstancesResponse
    -- ** Response constructor
    , unmonitorInstancesResponse
    -- ** Response lenses
    , uirInstanceMonitorings
    , uirStatusCode
    ) where

import Network.AWS.EC2.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'unmonitorInstances' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uiDryRun'
--
-- * 'uiInstanceIds'
data UnmonitorInstances = UnmonitorInstances'{_uiDryRun :: Maybe Bool, _uiInstanceIds :: [Text]} deriving (Eq, Read, Show)

-- | 'UnmonitorInstances' smart constructor.
unmonitorInstances :: UnmonitorInstances
unmonitorInstances = UnmonitorInstances'{_uiDryRun = Nothing, _uiInstanceIds = mempty};

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
uiDryRun :: Lens' UnmonitorInstances (Maybe Bool)
uiDryRun = lens _uiDryRun (\ s a -> s{_uiDryRun = a});

-- | One or more instance IDs.
uiInstanceIds :: Lens' UnmonitorInstances [Text]
uiInstanceIds = lens _uiInstanceIds (\ s a -> s{_uiInstanceIds = a});

instance AWSRequest UnmonitorInstances where
        type Sv UnmonitorInstances = EC2
        type Rs UnmonitorInstances =
             UnmonitorInstancesResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 UnmonitorInstancesResponse' <$>
                   (may (parseXMLList "item") x) <*>
                     (pure (fromEnum s)))

instance ToHeaders UnmonitorInstances where
        toHeaders = const mempty

instance ToPath UnmonitorInstances where
        toPath = const "/"

instance ToQuery UnmonitorInstances where
        toQuery UnmonitorInstances'{..}
          = mconcat
              ["Action" =: ("UnmonitorInstances" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _uiDryRun,
               toQueryList "InstanceId" _uiInstanceIds]

-- | /See:/ 'unmonitorInstancesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uirInstanceMonitorings'
--
-- * 'uirStatusCode'
data UnmonitorInstancesResponse = UnmonitorInstancesResponse'{_uirInstanceMonitorings :: Maybe [InstanceMonitoring], _uirStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'UnmonitorInstancesResponse' smart constructor.
unmonitorInstancesResponse :: Int -> UnmonitorInstancesResponse
unmonitorInstancesResponse pStatusCode = UnmonitorInstancesResponse'{_uirInstanceMonitorings = Nothing, _uirStatusCode = pStatusCode};

-- | Monitoring information for one or more instances.
uirInstanceMonitorings :: Lens' UnmonitorInstancesResponse [InstanceMonitoring]
uirInstanceMonitorings = lens _uirInstanceMonitorings (\ s a -> s{_uirInstanceMonitorings = a}) . _Default;

-- | FIXME: Undocumented member.
uirStatusCode :: Lens' UnmonitorInstancesResponse Int
uirStatusCode = lens _uirStatusCode (\ s a -> s{_uirStatusCode = a});
