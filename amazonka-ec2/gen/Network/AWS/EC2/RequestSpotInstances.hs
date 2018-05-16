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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Spot Instance request. Spot Instances are instances that Amazon EC2 launches when the maximum price that you specify exceeds the current Spot price. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-requests.html Spot Instance Requests> in the /Amazon EC2 User Guide for Linux Instances/ .
--
--
module Network.AWS.EC2.RequestSpotInstances
    (
    -- * Creating a Request
      requestSpotInstances
    , RequestSpotInstances
    -- * Request Lenses
    , rBlockDurationMinutes
    , rClientToken
    , rInstanceCount
    , rInstanceInterruptionBehavior
    , rSpotPrice
    , rLaunchSpecification
    , rAvailabilityZoneGroup
    , rValidUntil
    , rLaunchGroup
    , rType
    , rValidFrom
    , rDryRun

    -- * Destructuring the Response
    , requestSpotInstancesResponse
    , RequestSpotInstancesResponse
    -- * Response Lenses
    , rsirsSpotInstanceRequests
    , rsirsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for RequestSpotInstances.
--
--
--
-- /See:/ 'requestSpotInstances' smart constructor.
data RequestSpotInstances = RequestSpotInstances'
  { _rBlockDurationMinutes         :: !(Maybe Int)
  , _rClientToken                  :: !(Maybe Text)
  , _rInstanceCount                :: !(Maybe Int)
  , _rInstanceInterruptionBehavior :: !(Maybe InstanceInterruptionBehavior)
  , _rSpotPrice                    :: !(Maybe Text)
  , _rLaunchSpecification          :: !(Maybe RequestSpotLaunchSpecification)
  , _rAvailabilityZoneGroup        :: !(Maybe Text)
  , _rValidUntil                   :: !(Maybe ISO8601)
  , _rLaunchGroup                  :: !(Maybe Text)
  , _rType                         :: !(Maybe SpotInstanceType)
  , _rValidFrom                    :: !(Maybe ISO8601)
  , _rDryRun                       :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RequestSpotInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rBlockDurationMinutes' - The required duration for the Spot Instances (also known as Spot blocks), in minutes. This value must be a multiple of 60 (60, 120, 180, 240, 300, or 360). The duration period starts as soon as your Spot Instance receives its instance ID. At the end of the duration period, Amazon EC2 marks the Spot Instance for termination and provides a Spot Instance termination notice, which gives the instance a two-minute warning before it terminates. You can't specify an Availability Zone group or a launch group if you specify a duration.
--
-- * 'rClientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- * 'rInstanceCount' - The maximum number of Spot Instances to launch. Default: 1
--
-- * 'rInstanceInterruptionBehavior' - The behavior when a Spot Instance is interrupted. The default is @terminate@ .
--
-- * 'rSpotPrice' - The maximum price per hour that you are willing to pay for a Spot Instance. The default is the On-Demand price.
--
-- * 'rLaunchSpecification' - The launch specification.
--
-- * 'rAvailabilityZoneGroup' - The user-specified name for a logical grouping of requests. When you specify an Availability Zone group in a Spot Instance request, all Spot Instances in the request are launched in the same Availability Zone. Instance proximity is maintained with this parameter, but the choice of Availability Zone is not. The group applies only to requests for Spot Instances of the same instance type. Any additional Spot Instance requests that are specified with the same Availability Zone group name are launched in that same Availability Zone, as long as at least one instance from the group is still active. If there is no active instance running in the Availability Zone group that you specify for a new Spot Instance request (all instances are terminated, the request is expired, or the maximum price you specified falls below current Spot price), then Amazon EC2 launches the instance in any Availability Zone where the constraint can be met. Consequently, the subsequent set of Spot Instances could be placed in a different zone from the original request, even if you specified the same Availability Zone group. Default: Instances are launched in any available Availability Zone.
--
-- * 'rValidUntil' - The end date of the request. If this is a one-time request, the request remains active until all instances launch, the request is canceled, or this date is reached. If the request is persistent, it remains active until it is canceled or this date is reached. The default end date is 7 days from the current date.
--
-- * 'rLaunchGroup' - The instance launch group. Launch groups are Spot Instances that launch together and terminate together. Default: Instances are launched and terminated individually
--
-- * 'rType' - The Spot Instance request type. Default: @one-time@
--
-- * 'rValidFrom' - The start date of the request. If this is a one-time request, the request becomes active at this date and time and remains active until all instances launch, the request expires, or the request is canceled. If the request is persistent, the request becomes active at this date and time and remains active until it expires or is canceled.
--
-- * 'rDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
requestSpotInstances
    :: RequestSpotInstances
requestSpotInstances =
  RequestSpotInstances'
    { _rBlockDurationMinutes = Nothing
    , _rClientToken = Nothing
    , _rInstanceCount = Nothing
    , _rInstanceInterruptionBehavior = Nothing
    , _rSpotPrice = Nothing
    , _rLaunchSpecification = Nothing
    , _rAvailabilityZoneGroup = Nothing
    , _rValidUntil = Nothing
    , _rLaunchGroup = Nothing
    , _rType = Nothing
    , _rValidFrom = Nothing
    , _rDryRun = Nothing
    }


-- | The required duration for the Spot Instances (also known as Spot blocks), in minutes. This value must be a multiple of 60 (60, 120, 180, 240, 300, or 360). The duration period starts as soon as your Spot Instance receives its instance ID. At the end of the duration period, Amazon EC2 marks the Spot Instance for termination and provides a Spot Instance termination notice, which gives the instance a two-minute warning before it terminates. You can't specify an Availability Zone group or a launch group if you specify a duration.
rBlockDurationMinutes :: Lens' RequestSpotInstances (Maybe Int)
rBlockDurationMinutes = lens _rBlockDurationMinutes (\ s a -> s{_rBlockDurationMinutes = a})

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> in the /Amazon EC2 User Guide for Linux Instances/ .
rClientToken :: Lens' RequestSpotInstances (Maybe Text)
rClientToken = lens _rClientToken (\ s a -> s{_rClientToken = a})

-- | The maximum number of Spot Instances to launch. Default: 1
rInstanceCount :: Lens' RequestSpotInstances (Maybe Int)
rInstanceCount = lens _rInstanceCount (\ s a -> s{_rInstanceCount = a})

-- | The behavior when a Spot Instance is interrupted. The default is @terminate@ .
rInstanceInterruptionBehavior :: Lens' RequestSpotInstances (Maybe InstanceInterruptionBehavior)
rInstanceInterruptionBehavior = lens _rInstanceInterruptionBehavior (\ s a -> s{_rInstanceInterruptionBehavior = a})

-- | The maximum price per hour that you are willing to pay for a Spot Instance. The default is the On-Demand price.
rSpotPrice :: Lens' RequestSpotInstances (Maybe Text)
rSpotPrice = lens _rSpotPrice (\ s a -> s{_rSpotPrice = a})

-- | The launch specification.
rLaunchSpecification :: Lens' RequestSpotInstances (Maybe RequestSpotLaunchSpecification)
rLaunchSpecification = lens _rLaunchSpecification (\ s a -> s{_rLaunchSpecification = a})

-- | The user-specified name for a logical grouping of requests. When you specify an Availability Zone group in a Spot Instance request, all Spot Instances in the request are launched in the same Availability Zone. Instance proximity is maintained with this parameter, but the choice of Availability Zone is not. The group applies only to requests for Spot Instances of the same instance type. Any additional Spot Instance requests that are specified with the same Availability Zone group name are launched in that same Availability Zone, as long as at least one instance from the group is still active. If there is no active instance running in the Availability Zone group that you specify for a new Spot Instance request (all instances are terminated, the request is expired, or the maximum price you specified falls below current Spot price), then Amazon EC2 launches the instance in any Availability Zone where the constraint can be met. Consequently, the subsequent set of Spot Instances could be placed in a different zone from the original request, even if you specified the same Availability Zone group. Default: Instances are launched in any available Availability Zone.
rAvailabilityZoneGroup :: Lens' RequestSpotInstances (Maybe Text)
rAvailabilityZoneGroup = lens _rAvailabilityZoneGroup (\ s a -> s{_rAvailabilityZoneGroup = a})

-- | The end date of the request. If this is a one-time request, the request remains active until all instances launch, the request is canceled, or this date is reached. If the request is persistent, it remains active until it is canceled or this date is reached. The default end date is 7 days from the current date.
rValidUntil :: Lens' RequestSpotInstances (Maybe UTCTime)
rValidUntil = lens _rValidUntil (\ s a -> s{_rValidUntil = a}) . mapping _Time

-- | The instance launch group. Launch groups are Spot Instances that launch together and terminate together. Default: Instances are launched and terminated individually
rLaunchGroup :: Lens' RequestSpotInstances (Maybe Text)
rLaunchGroup = lens _rLaunchGroup (\ s a -> s{_rLaunchGroup = a})

-- | The Spot Instance request type. Default: @one-time@
rType :: Lens' RequestSpotInstances (Maybe SpotInstanceType)
rType = lens _rType (\ s a -> s{_rType = a})

-- | The start date of the request. If this is a one-time request, the request becomes active at this date and time and remains active until all instances launch, the request expires, or the request is canceled. If the request is persistent, the request becomes active at this date and time and remains active until it expires or is canceled.
rValidFrom :: Lens' RequestSpotInstances (Maybe UTCTime)
rValidFrom = lens _rValidFrom (\ s a -> s{_rValidFrom = a}) . mapping _Time

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
rDryRun :: Lens' RequestSpotInstances (Maybe Bool)
rDryRun = lens _rDryRun (\ s a -> s{_rDryRun = a})

instance AWSRequest RequestSpotInstances where
        type Rs RequestSpotInstances =
             RequestSpotInstancesResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 RequestSpotInstancesResponse' <$>
                   (x .@? "spotInstanceRequestSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable RequestSpotInstances where

instance NFData RequestSpotInstances where

instance ToHeaders RequestSpotInstances where
        toHeaders = const mempty

instance ToPath RequestSpotInstances where
        toPath = const "/"

instance ToQuery RequestSpotInstances where
        toQuery RequestSpotInstances'{..}
          = mconcat
              ["Action" =: ("RequestSpotInstances" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "BlockDurationMinutes" =: _rBlockDurationMinutes,
               "ClientToken" =: _rClientToken,
               "InstanceCount" =: _rInstanceCount,
               "InstanceInterruptionBehavior" =:
                 _rInstanceInterruptionBehavior,
               "SpotPrice" =: _rSpotPrice,
               "LaunchSpecification" =: _rLaunchSpecification,
               "AvailabilityZoneGroup" =: _rAvailabilityZoneGroup,
               "ValidUntil" =: _rValidUntil,
               "LaunchGroup" =: _rLaunchGroup, "Type" =: _rType,
               "ValidFrom" =: _rValidFrom, "DryRun" =: _rDryRun]

-- | Contains the output of RequestSpotInstances.
--
--
--
-- /See:/ 'requestSpotInstancesResponse' smart constructor.
data RequestSpotInstancesResponse = RequestSpotInstancesResponse'
  { _rsirsSpotInstanceRequests :: !(Maybe [SpotInstanceRequest])
  , _rsirsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RequestSpotInstancesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsirsSpotInstanceRequests' - One or more Spot Instance requests.
--
-- * 'rsirsResponseStatus' - -- | The response status code.
requestSpotInstancesResponse
    :: Int -- ^ 'rsirsResponseStatus'
    -> RequestSpotInstancesResponse
requestSpotInstancesResponse pResponseStatus_ =
  RequestSpotInstancesResponse'
    { _rsirsSpotInstanceRequests = Nothing
    , _rsirsResponseStatus = pResponseStatus_
    }


-- | One or more Spot Instance requests.
rsirsSpotInstanceRequests :: Lens' RequestSpotInstancesResponse [SpotInstanceRequest]
rsirsSpotInstanceRequests = lens _rsirsSpotInstanceRequests (\ s a -> s{_rsirsSpotInstanceRequests = a}) . _Default . _Coerce

-- | -- | The response status code.
rsirsResponseStatus :: Lens' RequestSpotInstancesResponse Int
rsirsResponseStatus = lens _rsirsResponseStatus (\ s a -> s{_rsirsResponseStatus = a})

instance NFData RequestSpotInstancesResponse where
