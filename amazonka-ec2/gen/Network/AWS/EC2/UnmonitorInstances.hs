{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.UnmonitorInstances
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables monitoring for a running instance. For more information about
-- monitoring instances, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-cloudwatch.html Monitoring Your Instances and Volumes>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-UnmonitorInstances.html AWS API Reference> for UnmonitorInstances.
module Network.AWS.EC2.UnmonitorInstances
    (
    -- * Creating a Request
      unmonitorInstances
    , UnmonitorInstances
    -- * Request Lenses
    , uiDryRun
    , uiInstanceIds

    -- * Destructuring the Response
    , unmonitorInstancesResponse
    , UnmonitorInstancesResponse
    -- * Response Lenses
    , uirsInstanceMonitorings
    , uirsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'unmonitorInstances' smart constructor.
data UnmonitorInstances = UnmonitorInstances'
    { _uiDryRun      :: !(Maybe Bool)
    , _uiInstanceIds :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UnmonitorInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uiDryRun'
--
-- * 'uiInstanceIds'
unmonitorInstances
    :: UnmonitorInstances
unmonitorInstances =
    UnmonitorInstances'
    { _uiDryRun = Nothing
    , _uiInstanceIds = mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is 'DryRunOperation'.
-- Otherwise, it is 'UnauthorizedOperation'.
uiDryRun :: Lens' UnmonitorInstances (Maybe Bool)
uiDryRun = lens _uiDryRun (\ s a -> s{_uiDryRun = a});

-- | One or more instance IDs.
uiInstanceIds :: Lens' UnmonitorInstances [Text]
uiInstanceIds = lens _uiInstanceIds (\ s a -> s{_uiInstanceIds = a}) . _Coerce;

instance AWSRequest UnmonitorInstances where
        type Sv UnmonitorInstances = EC2
        type Rs UnmonitorInstances =
             UnmonitorInstancesResponse
        request = postQuery
        response
          = receiveXML
              (\ s h x ->
                 UnmonitorInstancesResponse' <$>
                   (x .@? "instancesSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

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
data UnmonitorInstancesResponse = UnmonitorInstancesResponse'
    { _uirsInstanceMonitorings :: !(Maybe [InstanceMonitoring])
    , _uirsStatus              :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UnmonitorInstancesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uirsInstanceMonitorings'
--
-- * 'uirsStatus'
unmonitorInstancesResponse
    :: Int -- ^ 'uirsStatus'
    -> UnmonitorInstancesResponse
unmonitorInstancesResponse pStatus_ =
    UnmonitorInstancesResponse'
    { _uirsInstanceMonitorings = Nothing
    , _uirsStatus = pStatus_
    }

-- | Monitoring information for one or more instances.
uirsInstanceMonitorings :: Lens' UnmonitorInstancesResponse [InstanceMonitoring]
uirsInstanceMonitorings = lens _uirsInstanceMonitorings (\ s a -> s{_uirsInstanceMonitorings = a}) . _Default . _Coerce;

-- | The response status code.
uirsStatus :: Lens' UnmonitorInstancesResponse Int
uirsStatus = lens _uirsStatus (\ s a -> s{_uirsStatus = a});
