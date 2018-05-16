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
-- Module      : Network.AWS.EC2.RunScheduledInstances
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Launches the specified Scheduled Instances.
--
--
-- Before you can launch a Scheduled Instance, you must purchase it and obtain an identifier using 'PurchaseScheduledInstances' .
--
-- You must launch a Scheduled Instance during its scheduled time period. You can't stop or reboot a Scheduled Instance, but you can terminate it as needed. If you terminate a Scheduled Instance before the current scheduled time period ends, you can launch it again after a few minutes. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-scheduled-instances.html Scheduled Instances> in the /Amazon Elastic Compute Cloud User Guide/ .
--
module Network.AWS.EC2.RunScheduledInstances
    (
    -- * Creating a Request
      runScheduledInstances
    , RunScheduledInstances
    -- * Request Lenses
    , rsiClientToken
    , rsiInstanceCount
    , rsiDryRun
    , rsiLaunchSpecification
    , rsiScheduledInstanceId

    -- * Destructuring the Response
    , runScheduledInstancesResponse
    , RunScheduledInstancesResponse
    -- * Response Lenses
    , rrsInstanceIdSet
    , rrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for RunScheduledInstances.
--
--
--
-- /See:/ 'runScheduledInstances' smart constructor.
data RunScheduledInstances = RunScheduledInstances'
  { _rsiClientToken         :: !(Maybe Text)
  , _rsiInstanceCount       :: !(Maybe Int)
  , _rsiDryRun              :: !(Maybe Bool)
  , _rsiLaunchSpecification :: !ScheduledInstancesLaunchSpecification
  , _rsiScheduledInstanceId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RunScheduledInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsiClientToken' - Unique, case-sensitive identifier that ensures the idempotency of the request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- * 'rsiInstanceCount' - The number of instances. Default: 1
--
-- * 'rsiDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'rsiLaunchSpecification' - The launch specification. You must match the instance type, Availability Zone, network, and platform of the schedule that you purchased.
--
-- * 'rsiScheduledInstanceId' - The Scheduled Instance ID.
runScheduledInstances
    :: ScheduledInstancesLaunchSpecification -- ^ 'rsiLaunchSpecification'
    -> Text -- ^ 'rsiScheduledInstanceId'
    -> RunScheduledInstances
runScheduledInstances pLaunchSpecification_ pScheduledInstanceId_ =
  RunScheduledInstances'
    { _rsiClientToken = Nothing
    , _rsiInstanceCount = Nothing
    , _rsiDryRun = Nothing
    , _rsiLaunchSpecification = pLaunchSpecification_
    , _rsiScheduledInstanceId = pScheduledInstanceId_
    }


-- | Unique, case-sensitive identifier that ensures the idempotency of the request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
rsiClientToken :: Lens' RunScheduledInstances (Maybe Text)
rsiClientToken = lens _rsiClientToken (\ s a -> s{_rsiClientToken = a})

-- | The number of instances. Default: 1
rsiInstanceCount :: Lens' RunScheduledInstances (Maybe Int)
rsiInstanceCount = lens _rsiInstanceCount (\ s a -> s{_rsiInstanceCount = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
rsiDryRun :: Lens' RunScheduledInstances (Maybe Bool)
rsiDryRun = lens _rsiDryRun (\ s a -> s{_rsiDryRun = a})

-- | The launch specification. You must match the instance type, Availability Zone, network, and platform of the schedule that you purchased.
rsiLaunchSpecification :: Lens' RunScheduledInstances ScheduledInstancesLaunchSpecification
rsiLaunchSpecification = lens _rsiLaunchSpecification (\ s a -> s{_rsiLaunchSpecification = a})

-- | The Scheduled Instance ID.
rsiScheduledInstanceId :: Lens' RunScheduledInstances Text
rsiScheduledInstanceId = lens _rsiScheduledInstanceId (\ s a -> s{_rsiScheduledInstanceId = a})

instance AWSRequest RunScheduledInstances where
        type Rs RunScheduledInstances =
             RunScheduledInstancesResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 RunScheduledInstancesResponse' <$>
                   (x .@? "instanceIdSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable RunScheduledInstances where

instance NFData RunScheduledInstances where

instance ToHeaders RunScheduledInstances where
        toHeaders = const mempty

instance ToPath RunScheduledInstances where
        toPath = const "/"

instance ToQuery RunScheduledInstances where
        toQuery RunScheduledInstances'{..}
          = mconcat
              ["Action" =: ("RunScheduledInstances" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "ClientToken" =: _rsiClientToken,
               "InstanceCount" =: _rsiInstanceCount,
               "DryRun" =: _rsiDryRun,
               "LaunchSpecification" =: _rsiLaunchSpecification,
               "ScheduledInstanceId" =: _rsiScheduledInstanceId]

-- | Contains the output of RunScheduledInstances.
--
--
--
-- /See:/ 'runScheduledInstancesResponse' smart constructor.
data RunScheduledInstancesResponse = RunScheduledInstancesResponse'
  { _rrsInstanceIdSet  :: !(Maybe [Text])
  , _rrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RunScheduledInstancesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrsInstanceIdSet' - The IDs of the newly launched instances.
--
-- * 'rrsResponseStatus' - -- | The response status code.
runScheduledInstancesResponse
    :: Int -- ^ 'rrsResponseStatus'
    -> RunScheduledInstancesResponse
runScheduledInstancesResponse pResponseStatus_ =
  RunScheduledInstancesResponse'
    {_rrsInstanceIdSet = Nothing, _rrsResponseStatus = pResponseStatus_}


-- | The IDs of the newly launched instances.
rrsInstanceIdSet :: Lens' RunScheduledInstancesResponse [Text]
rrsInstanceIdSet = lens _rrsInstanceIdSet (\ s a -> s{_rrsInstanceIdSet = a}) . _Default . _Coerce

-- | -- | The response status code.
rrsResponseStatus :: Lens' RunScheduledInstancesResponse Int
rrsResponseStatus = lens _rrsResponseStatus (\ s a -> s{_rrsResponseStatus = a})

instance NFData RunScheduledInstancesResponse where
