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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Spot instance request. Spot instances are instances that Amazon EC2 launches when the bid price that you specify exceeds the current Spot price. Amazon EC2 periodically sets the Spot price based on available Spot Instance capacity and current Spot instance requests. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-requests.html Spot Instance Requests> in the /Amazon Elastic Compute Cloud User Guide/ .
--
--
module Network.AWS.EC2.RequestSpotInstances
    (
    -- * Creating a Request
      requestSpotInstances
    , RequestSpotInstances
    -- * Request Lenses
    , rsisBlockDurationMinutes
    , rsisClientToken
    , rsisInstanceCount
    , rsisLaunchSpecification
    , rsisAvailabilityZoneGroup
    , rsisValidUntil
    , rsisLaunchGroup
    , rsisType
    , rsisValidFrom
    , rsisDryRun
    , rsisSpotPrice

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
--
--
-- /See:/ 'requestSpotInstances' smart constructor.
data RequestSpotInstances = RequestSpotInstances'
    { _rsisBlockDurationMinutes  :: !(Maybe Int)
    , _rsisClientToken           :: !(Maybe Text)
    , _rsisInstanceCount         :: !(Maybe Int)
    , _rsisLaunchSpecification   :: !(Maybe RequestSpotLaunchSpecification)
    , _rsisAvailabilityZoneGroup :: !(Maybe Text)
    , _rsisValidUntil            :: !(Maybe ISO8601)
    , _rsisLaunchGroup           :: !(Maybe Text)
    , _rsisType                  :: !(Maybe SpotInstanceType)
    , _rsisValidFrom             :: !(Maybe ISO8601)
    , _rsisDryRun                :: !(Maybe Bool)
    , _rsisSpotPrice             :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RequestSpotInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsisBlockDurationMinutes' - The required duration for the Spot instances (also known as Spot blocks), in minutes. This value must be a multiple of 60 (60, 120, 180, 240, 300, or 360). The duration period starts as soon as your Spot instance receives its instance ID. At the end of the duration period, Amazon EC2 marks the Spot instance for termination and provides a Spot instance termination notice, which gives the instance a two-minute warning before it terminates. Note that you can't specify an Availability Zone group or a launch group if you specify a duration.
--
-- * 'rsisClientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'rsisInstanceCount' - The maximum number of Spot instances to launch. Default: 1
--
-- * 'rsisLaunchSpecification' - The launch specification.
--
-- * 'rsisAvailabilityZoneGroup' - The user-specified name for a logical grouping of bids. When you specify an Availability Zone group in a Spot Instance request, all Spot instances in the request are launched in the same Availability Zone. Instance proximity is maintained with this parameter, but the choice of Availability Zone is not. The group applies only to bids for Spot Instances of the same instance type. Any additional Spot instance requests that are specified with the same Availability Zone group name are launched in that same Availability Zone, as long as at least one instance from the group is still active. If there is no active instance running in the Availability Zone group that you specify for a new Spot instance request (all instances are terminated, the bid is expired, or the bid falls below current market), then Amazon EC2 launches the instance in any Availability Zone where the constraint can be met. Consequently, the subsequent set of Spot instances could be placed in a different zone from the original request, even if you specified the same Availability Zone group. Default: Instances are launched in any available Availability Zone.
--
-- * 'rsisValidUntil' - The end date of the request. If this is a one-time request, the request remains active until all instances launch, the request is canceled, or this date is reached. If the request is persistent, it remains active until it is canceled or this date and time is reached. Default: The request is effective indefinitely.
--
-- * 'rsisLaunchGroup' - The instance launch group. Launch groups are Spot instances that launch together and terminate together. Default: Instances are launched and terminated individually
--
-- * 'rsisType' - The Spot instance request type. Default: @one-time@
--
-- * 'rsisValidFrom' - The start date of the request. If this is a one-time request, the request becomes active at this date and time and remains active until all instances launch, the request expires, or the request is canceled. If the request is persistent, the request becomes active at this date and time and remains active until it expires or is canceled. Default: The request is effective indefinitely.
--
-- * 'rsisDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'rsisSpotPrice' - The maximum hourly price (bid) for any Spot instance launched to fulfill the request.
requestSpotInstances
    :: Text -- ^ 'rsisSpotPrice'
    -> RequestSpotInstances
requestSpotInstances pSpotPrice_ =
    RequestSpotInstances'
    { _rsisBlockDurationMinutes = Nothing
    , _rsisClientToken = Nothing
    , _rsisInstanceCount = Nothing
    , _rsisLaunchSpecification = Nothing
    , _rsisAvailabilityZoneGroup = Nothing
    , _rsisValidUntil = Nothing
    , _rsisLaunchGroup = Nothing
    , _rsisType = Nothing
    , _rsisValidFrom = Nothing
    , _rsisDryRun = Nothing
    , _rsisSpotPrice = pSpotPrice_
    }

-- | The required duration for the Spot instances (also known as Spot blocks), in minutes. This value must be a multiple of 60 (60, 120, 180, 240, 300, or 360). The duration period starts as soon as your Spot instance receives its instance ID. At the end of the duration period, Amazon EC2 marks the Spot instance for termination and provides a Spot instance termination notice, which gives the instance a two-minute warning before it terminates. Note that you can't specify an Availability Zone group or a launch group if you specify a duration.
rsisBlockDurationMinutes :: Lens' RequestSpotInstances (Maybe Int)
rsisBlockDurationMinutes = lens _rsisBlockDurationMinutes (\ s a -> s{_rsisBlockDurationMinutes = a});

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> in the /Amazon Elastic Compute Cloud User Guide/ .
rsisClientToken :: Lens' RequestSpotInstances (Maybe Text)
rsisClientToken = lens _rsisClientToken (\ s a -> s{_rsisClientToken = a});

-- | The maximum number of Spot instances to launch. Default: 1
rsisInstanceCount :: Lens' RequestSpotInstances (Maybe Int)
rsisInstanceCount = lens _rsisInstanceCount (\ s a -> s{_rsisInstanceCount = a});

-- | The launch specification.
rsisLaunchSpecification :: Lens' RequestSpotInstances (Maybe RequestSpotLaunchSpecification)
rsisLaunchSpecification = lens _rsisLaunchSpecification (\ s a -> s{_rsisLaunchSpecification = a});

-- | The user-specified name for a logical grouping of bids. When you specify an Availability Zone group in a Spot Instance request, all Spot instances in the request are launched in the same Availability Zone. Instance proximity is maintained with this parameter, but the choice of Availability Zone is not. The group applies only to bids for Spot Instances of the same instance type. Any additional Spot instance requests that are specified with the same Availability Zone group name are launched in that same Availability Zone, as long as at least one instance from the group is still active. If there is no active instance running in the Availability Zone group that you specify for a new Spot instance request (all instances are terminated, the bid is expired, or the bid falls below current market), then Amazon EC2 launches the instance in any Availability Zone where the constraint can be met. Consequently, the subsequent set of Spot instances could be placed in a different zone from the original request, even if you specified the same Availability Zone group. Default: Instances are launched in any available Availability Zone.
rsisAvailabilityZoneGroup :: Lens' RequestSpotInstances (Maybe Text)
rsisAvailabilityZoneGroup = lens _rsisAvailabilityZoneGroup (\ s a -> s{_rsisAvailabilityZoneGroup = a});

-- | The end date of the request. If this is a one-time request, the request remains active until all instances launch, the request is canceled, or this date is reached. If the request is persistent, it remains active until it is canceled or this date and time is reached. Default: The request is effective indefinitely.
rsisValidUntil :: Lens' RequestSpotInstances (Maybe UTCTime)
rsisValidUntil = lens _rsisValidUntil (\ s a -> s{_rsisValidUntil = a}) . mapping _Time;

-- | The instance launch group. Launch groups are Spot instances that launch together and terminate together. Default: Instances are launched and terminated individually
rsisLaunchGroup :: Lens' RequestSpotInstances (Maybe Text)
rsisLaunchGroup = lens _rsisLaunchGroup (\ s a -> s{_rsisLaunchGroup = a});

-- | The Spot instance request type. Default: @one-time@
rsisType :: Lens' RequestSpotInstances (Maybe SpotInstanceType)
rsisType = lens _rsisType (\ s a -> s{_rsisType = a});

-- | The start date of the request. If this is a one-time request, the request becomes active at this date and time and remains active until all instances launch, the request expires, or the request is canceled. If the request is persistent, the request becomes active at this date and time and remains active until it expires or is canceled. Default: The request is effective indefinitely.
rsisValidFrom :: Lens' RequestSpotInstances (Maybe UTCTime)
rsisValidFrom = lens _rsisValidFrom (\ s a -> s{_rsisValidFrom = a}) . mapping _Time;

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
rsisDryRun :: Lens' RequestSpotInstances (Maybe Bool)
rsisDryRun = lens _rsisDryRun (\ s a -> s{_rsisDryRun = a});

-- | The maximum hourly price (bid) for any Spot instance launched to fulfill the request.
rsisSpotPrice :: Lens' RequestSpotInstances Text
rsisSpotPrice = lens _rsisSpotPrice (\ s a -> s{_rsisSpotPrice = a});

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

instance Hashable RequestSpotInstances

instance NFData RequestSpotInstances

instance ToHeaders RequestSpotInstances where
        toHeaders = const mempty

instance ToPath RequestSpotInstances where
        toPath = const "/"

instance ToQuery RequestSpotInstances where
        toQuery RequestSpotInstances'{..}
          = mconcat
              ["Action" =: ("RequestSpotInstances" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "BlockDurationMinutes" =: _rsisBlockDurationMinutes,
               "ClientToken" =: _rsisClientToken,
               "InstanceCount" =: _rsisInstanceCount,
               "LaunchSpecification" =: _rsisLaunchSpecification,
               "AvailabilityZoneGroup" =:
                 _rsisAvailabilityZoneGroup,
               "ValidUntil" =: _rsisValidUntil,
               "LaunchGroup" =: _rsisLaunchGroup,
               "Type" =: _rsisType, "ValidFrom" =: _rsisValidFrom,
               "DryRun" =: _rsisDryRun,
               "SpotPrice" =: _rsisSpotPrice]

-- | Contains the output of RequestSpotInstances.
--
--
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
-- * 'rsirsSpotInstanceRequests' - One or more Spot instance requests.
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

-- | One or more Spot instance requests.
rsirsSpotInstanceRequests :: Lens' RequestSpotInstancesResponse [SpotInstanceRequest]
rsirsSpotInstanceRequests = lens _rsirsSpotInstanceRequests (\ s a -> s{_rsirsSpotInstanceRequests = a}) . _Default . _Coerce;

-- | -- | The response status code.
rsirsResponseStatus :: Lens' RequestSpotInstancesResponse Int
rsirsResponseStatus = lens _rsirsResponseStatus (\ s a -> s{_rsirsResponseStatus = a});

instance NFData RequestSpotInstancesResponse
