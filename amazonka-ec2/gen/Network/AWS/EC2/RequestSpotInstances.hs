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
-- Module      : Network.AWS.EC2.RequestSpotInstances
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Spot instance request. Spot instances are instances that
-- Amazon EC2 launches when the bid price that you specify exceeds the
-- current Spot price. Amazon EC2 periodically sets the Spot price based on
-- available Spot Instance capacity and current Spot instance requests. For
-- more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-requests.html Spot Instance Requests>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-RequestSpotInstances.html AWS API Reference> for RequestSpotInstances.
module Network.AWS.EC2.RequestSpotInstances
    (
    -- * Creating a Request
      requestSpotInstances
    , RequestSpotInstances
    -- * Request Lenses
    , rsiBlockDurationMinutes
    , rsiClientToken
    , rsiInstanceCount
    , rsiLaunchSpecification
    , rsiAvailabilityZoneGroup
    , rsiValidUntil
    , rsiLaunchGroup
    , rsiType
    , rsiValidFrom
    , rsiDryRun
    , rsiSpotPrice

    -- * Destructuring the Response
    , requestSpotInstancesResponse
    , RequestSpotInstancesResponse
    -- * Response Lenses
    , rsirsSpotInstanceRequests
    , rsirsResponseStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for RequestSpotInstances.
--
-- /See:/ 'requestSpotInstances' smart constructor.
data RequestSpotInstances = RequestSpotInstances'
    { _rsiBlockDurationMinutes  :: !(Maybe Int)
    , _rsiClientToken           :: !(Maybe Text)
    , _rsiInstanceCount         :: !(Maybe Int)
    , _rsiLaunchSpecification   :: !(Maybe RequestSpotLaunchSpecification)
    , _rsiAvailabilityZoneGroup :: !(Maybe Text)
    , _rsiValidUntil            :: !(Maybe ISO8601)
    , _rsiLaunchGroup           :: !(Maybe Text)
    , _rsiType                  :: !(Maybe SpotInstanceType)
    , _rsiValidFrom             :: !(Maybe ISO8601)
    , _rsiDryRun                :: !(Maybe Bool)
    , _rsiSpotPrice             :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RequestSpotInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsiBlockDurationMinutes'
--
-- * 'rsiClientToken'
--
-- * 'rsiInstanceCount'
--
-- * 'rsiLaunchSpecification'
--
-- * 'rsiAvailabilityZoneGroup'
--
-- * 'rsiValidUntil'
--
-- * 'rsiLaunchGroup'
--
-- * 'rsiType'
--
-- * 'rsiValidFrom'
--
-- * 'rsiDryRun'
--
-- * 'rsiSpotPrice'
requestSpotInstances
    :: Text -- ^ 'rsiSpotPrice'
    -> RequestSpotInstances
requestSpotInstances pSpotPrice_ =
    RequestSpotInstances'
    { _rsiBlockDurationMinutes = Nothing
    , _rsiClientToken = Nothing
    , _rsiInstanceCount = Nothing
    , _rsiLaunchSpecification = Nothing
    , _rsiAvailabilityZoneGroup = Nothing
    , _rsiValidUntil = Nothing
    , _rsiLaunchGroup = Nothing
    , _rsiType = Nothing
    , _rsiValidFrom = Nothing
    , _rsiDryRun = Nothing
    , _rsiSpotPrice = pSpotPrice_
    }

-- | The required duration for the Spot instances, in minutes. This value
-- must be a multiple of 60 (60, 120, 180, 240, 300, or 360).
--
-- The duration period starts as soon as your Spot instance receives its
-- instance ID. At the end of the duration period, Amazon EC2 marks the
-- Spot instance for termination and provides a Spot instance termination
-- notice, which gives the instance a two-minute warning before it
-- terminates.
--
-- Note that you can\'t specify an Availability Zone group or a launch
-- group if you specify a duration.
rsiBlockDurationMinutes :: Lens' RequestSpotInstances (Maybe Int)
rsiBlockDurationMinutes = lens _rsiBlockDurationMinutes (\ s a -> s{_rsiBlockDurationMinutes = a});

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency>
-- in the /Amazon Elastic Compute Cloud User Guide/.
rsiClientToken :: Lens' RequestSpotInstances (Maybe Text)
rsiClientToken = lens _rsiClientToken (\ s a -> s{_rsiClientToken = a});

-- | The maximum number of Spot instances to launch.
--
-- Default: 1
rsiInstanceCount :: Lens' RequestSpotInstances (Maybe Int)
rsiInstanceCount = lens _rsiInstanceCount (\ s a -> s{_rsiInstanceCount = a});

-- | Undocumented member.
rsiLaunchSpecification :: Lens' RequestSpotInstances (Maybe RequestSpotLaunchSpecification)
rsiLaunchSpecification = lens _rsiLaunchSpecification (\ s a -> s{_rsiLaunchSpecification = a});

-- | The user-specified name for a logical grouping of bids.
--
-- When you specify an Availability Zone group in a Spot Instance request,
-- all Spot instances in the request are launched in the same Availability
-- Zone. Instance proximity is maintained with this parameter, but the
-- choice of Availability Zone is not. The group applies only to bids for
-- Spot Instances of the same instance type. Any additional Spot instance
-- requests that are specified with the same Availability Zone group name
-- are launched in that same Availability Zone, as long as at least one
-- instance from the group is still active.
--
-- If there is no active instance running in the Availability Zone group
-- that you specify for a new Spot instance request (all instances are
-- terminated, the bid is expired, or the bid falls below current market),
-- then Amazon EC2 launches the instance in any Availability Zone where the
-- constraint can be met. Consequently, the subsequent set of Spot
-- instances could be placed in a different zone from the original request,
-- even if you specified the same Availability Zone group.
--
-- Default: Instances are launched in any available Availability Zone.
rsiAvailabilityZoneGroup :: Lens' RequestSpotInstances (Maybe Text)
rsiAvailabilityZoneGroup = lens _rsiAvailabilityZoneGroup (\ s a -> s{_rsiAvailabilityZoneGroup = a});

-- | The end date of the request. If this is a one-time request, the request
-- remains active until all instances launch, the request is canceled, or
-- this date is reached. If the request is persistent, it remains active
-- until it is canceled or this date and time is reached.
--
-- Default: The request is effective indefinitely.
rsiValidUntil :: Lens' RequestSpotInstances (Maybe UTCTime)
rsiValidUntil = lens _rsiValidUntil (\ s a -> s{_rsiValidUntil = a}) . mapping _Time;

-- | The instance launch group. Launch groups are Spot instances that launch
-- together and terminate together.
--
-- Default: Instances are launched and terminated individually
rsiLaunchGroup :: Lens' RequestSpotInstances (Maybe Text)
rsiLaunchGroup = lens _rsiLaunchGroup (\ s a -> s{_rsiLaunchGroup = a});

-- | The Spot instance request type.
--
-- Default: 'one-time'
rsiType :: Lens' RequestSpotInstances (Maybe SpotInstanceType)
rsiType = lens _rsiType (\ s a -> s{_rsiType = a});

-- | The start date of the request. If this is a one-time request, the
-- request becomes active at this date and time and remains active until
-- all instances launch, the request expires, or the request is canceled.
-- If the request is persistent, the request becomes active at this date
-- and time and remains active until it expires or is canceled.
--
-- Default: The request is effective indefinitely.
rsiValidFrom :: Lens' RequestSpotInstances (Maybe UTCTime)
rsiValidFrom = lens _rsiValidFrom (\ s a -> s{_rsiValidFrom = a}) . mapping _Time;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is 'DryRunOperation'.
-- Otherwise, it is 'UnauthorizedOperation'.
rsiDryRun :: Lens' RequestSpotInstances (Maybe Bool)
rsiDryRun = lens _rsiDryRun (\ s a -> s{_rsiDryRun = a});

-- | The maximum hourly price (bid) for any Spot instance launched to fulfill
-- the request.
rsiSpotPrice :: Lens' RequestSpotInstances Text
rsiSpotPrice = lens _rsiSpotPrice (\ s a -> s{_rsiSpotPrice = a});

instance AWSRequest RequestSpotInstances where
        type Rs RequestSpotInstances =
             RequestSpotInstancesResponse
        request = postQuery eC2
        response
          = receiveXML
              (\ s h x ->
                 RequestSpotInstancesResponse' <$>
                   (x .@? "spotInstanceRequestSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance ToHeaders RequestSpotInstances where
        toHeaders = const mempty

instance ToPath RequestSpotInstances where
        toPath = const "/"

instance ToQuery RequestSpotInstances where
        toQuery RequestSpotInstances'{..}
          = mconcat
              ["Action" =: ("RequestSpotInstances" :: ByteString),
               "Version" =: ("2015-10-01" :: ByteString),
               "BlockDurationMinutes" =: _rsiBlockDurationMinutes,
               "ClientToken" =: _rsiClientToken,
               "InstanceCount" =: _rsiInstanceCount,
               "LaunchSpecification" =: _rsiLaunchSpecification,
               "AvailabilityZoneGroup" =: _rsiAvailabilityZoneGroup,
               "ValidUntil" =: _rsiValidUntil,
               "LaunchGroup" =: _rsiLaunchGroup, "Type" =: _rsiType,
               "ValidFrom" =: _rsiValidFrom, "DryRun" =: _rsiDryRun,
               "SpotPrice" =: _rsiSpotPrice]

-- | Contains the output of RequestSpotInstances.
--
-- /See:/ 'requestSpotInstancesResponse' smart constructor.
data RequestSpotInstancesResponse = RequestSpotInstancesResponse'
    { _rsirsSpotInstanceRequests :: !(Maybe [SpotInstanceRequest])
    , _rsirsResponseStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RequestSpotInstancesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsirsSpotInstanceRequests'
--
-- * 'rsirsResponseStatus'
requestSpotInstancesResponse
    :: Int -- ^ 'rsirsResponseStatus'
    -> RequestSpotInstancesResponse
requestSpotInstancesResponse pResponseStatus_ =
    RequestSpotInstancesResponse'
    { _rsirsSpotInstanceRequests = Nothing
    , _rsirsResponseStatus = pResponseStatus_
    }

-- | One or more Spot instance requests.
rsirsSpotInstanceRequests :: Lens' RequestSpotInstancesResponse [SpotInstanceRequest]
rsirsSpotInstanceRequests = lens _rsirsSpotInstanceRequests (\ s a -> s{_rsirsSpotInstanceRequests = a}) . _Default . _Coerce;

-- | The response status code.
rsirsResponseStatus :: Lens' RequestSpotInstancesResponse Int
rsirsResponseStatus = lens _rsirsResponseStatus (\ s a -> s{_rsirsResponseStatus = a});
