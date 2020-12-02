{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.RequestSpotInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Spot Instance request.
--
--
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-requests.html Spot Instance requests> in the /Amazon EC2 User Guide for Linux Instances/ .
module Network.AWS.EC2.RequestSpotInstances
  ( -- * Creating a Request
    requestSpotInstances,
    RequestSpotInstances,

    -- * Request Lenses
    rsisBlockDurationMinutes,
    rsisClientToken,
    rsisInstanceCount,
    rsisInstanceInterruptionBehavior,
    rsisSpotPrice,
    rsisLaunchSpecification,
    rsisAvailabilityZoneGroup,
    rsisTagSpecifications,
    rsisValidUntil,
    rsisLaunchGroup,
    rsisType,
    rsisValidFrom,
    rsisDryRun,

    -- * Destructuring the Response
    requestSpotInstancesResponse,
    RequestSpotInstancesResponse,

    -- * Response Lenses
    rsirsSpotInstanceRequests,
    rsirsResponseStatus,
  )
where

import Network.AWS.EC2.Types
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
  { _rsisBlockDurationMinutes ::
      !(Maybe Int),
    _rsisClientToken :: !(Maybe Text),
    _rsisInstanceCount :: !(Maybe Int),
    _rsisInstanceInterruptionBehavior ::
      !(Maybe InstanceInterruptionBehavior),
    _rsisSpotPrice :: !(Maybe Text),
    _rsisLaunchSpecification ::
      !(Maybe RequestSpotLaunchSpecification),
    _rsisAvailabilityZoneGroup :: !(Maybe Text),
    _rsisTagSpecifications ::
      !(Maybe [TagSpecification]),
    _rsisValidUntil :: !(Maybe ISO8601),
    _rsisLaunchGroup :: !(Maybe Text),
    _rsisType :: !(Maybe SpotInstanceType),
    _rsisValidFrom :: !(Maybe ISO8601),
    _rsisDryRun :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RequestSpotInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsisBlockDurationMinutes' - The required duration for the Spot Instances (also known as Spot blocks), in minutes. This value must be a multiple of 60 (60, 120, 180, 240, 300, or 360). The duration period starts as soon as your Spot Instance receives its instance ID. At the end of the duration period, Amazon EC2 marks the Spot Instance for termination and provides a Spot Instance termination notice, which gives the instance a two-minute warning before it terminates. You can't specify an Availability Zone group or a launch group if you specify a duration. New accounts or accounts with no previous billing history with AWS are not eligible for Spot Instances with a defined duration (also known as Spot blocks).
--
-- * 'rsisClientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- * 'rsisInstanceCount' - The maximum number of Spot Instances to launch. Default: 1
--
-- * 'rsisInstanceInterruptionBehavior' - The behavior when a Spot Instance is interrupted. The default is @terminate@ .
--
-- * 'rsisSpotPrice' - The maximum price per hour that you are willing to pay for a Spot Instance. The default is the On-Demand price.
--
-- * 'rsisLaunchSpecification' - The launch specification.
--
-- * 'rsisAvailabilityZoneGroup' - The user-specified name for a logical grouping of requests. When you specify an Availability Zone group in a Spot Instance request, all Spot Instances in the request are launched in the same Availability Zone. Instance proximity is maintained with this parameter, but the choice of Availability Zone is not. The group applies only to requests for Spot Instances of the same instance type. Any additional Spot Instance requests that are specified with the same Availability Zone group name are launched in that same Availability Zone, as long as at least one instance from the group is still active. If there is no active instance running in the Availability Zone group that you specify for a new Spot Instance request (all instances are terminated, the request is expired, or the maximum price you specified falls below current Spot price), then Amazon EC2 launches the instance in any Availability Zone where the constraint can be met. Consequently, the subsequent set of Spot Instances could be placed in a different zone from the original request, even if you specified the same Availability Zone group. Default: Instances are launched in any available Availability Zone.
--
-- * 'rsisTagSpecifications' - The key-value pair for tagging the Spot Instance request on creation. The value for @ResourceType@ must be @spot-instances-request@ , otherwise the Spot Instance request fails. To tag the Spot Instance request after it has been created, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags> .
--
-- * 'rsisValidUntil' - The end date of the request, in UTC format (/YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).     * For a persistent request, the request remains active until the @ValidUntil@ date and time is reached. Otherwise, the request remains active until you cancel it.      * For a one-time request, the request remains active until all instances launch, the request is canceled, or the @ValidUntil@ date and time is reached. By default, the request is valid for 7 days from the date the request was created.
--
-- * 'rsisLaunchGroup' - The instance launch group. Launch groups are Spot Instances that launch together and terminate together. Default: Instances are launched and terminated individually
--
-- * 'rsisType' - The Spot Instance request type. Default: @one-time@
--
-- * 'rsisValidFrom' - The start date of the request. If this is a one-time request, the request becomes active at this date and time and remains active until all instances launch, the request expires, or the request is canceled. If the request is persistent, the request becomes active at this date and time and remains active until it expires or is canceled. The specified start date and time cannot be equal to the current date and time. You must specify a start date and time that occurs after the current date and time.
--
-- * 'rsisDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
requestSpotInstances ::
  RequestSpotInstances
requestSpotInstances =
  RequestSpotInstances'
    { _rsisBlockDurationMinutes = Nothing,
      _rsisClientToken = Nothing,
      _rsisInstanceCount = Nothing,
      _rsisInstanceInterruptionBehavior = Nothing,
      _rsisSpotPrice = Nothing,
      _rsisLaunchSpecification = Nothing,
      _rsisAvailabilityZoneGroup = Nothing,
      _rsisTagSpecifications = Nothing,
      _rsisValidUntil = Nothing,
      _rsisLaunchGroup = Nothing,
      _rsisType = Nothing,
      _rsisValidFrom = Nothing,
      _rsisDryRun = Nothing
    }

-- | The required duration for the Spot Instances (also known as Spot blocks), in minutes. This value must be a multiple of 60 (60, 120, 180, 240, 300, or 360). The duration period starts as soon as your Spot Instance receives its instance ID. At the end of the duration period, Amazon EC2 marks the Spot Instance for termination and provides a Spot Instance termination notice, which gives the instance a two-minute warning before it terminates. You can't specify an Availability Zone group or a launch group if you specify a duration. New accounts or accounts with no previous billing history with AWS are not eligible for Spot Instances with a defined duration (also known as Spot blocks).
rsisBlockDurationMinutes :: Lens' RequestSpotInstances (Maybe Int)
rsisBlockDurationMinutes = lens _rsisBlockDurationMinutes (\s a -> s {_rsisBlockDurationMinutes = a})

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> in the /Amazon EC2 User Guide for Linux Instances/ .
rsisClientToken :: Lens' RequestSpotInstances (Maybe Text)
rsisClientToken = lens _rsisClientToken (\s a -> s {_rsisClientToken = a})

-- | The maximum number of Spot Instances to launch. Default: 1
rsisInstanceCount :: Lens' RequestSpotInstances (Maybe Int)
rsisInstanceCount = lens _rsisInstanceCount (\s a -> s {_rsisInstanceCount = a})

-- | The behavior when a Spot Instance is interrupted. The default is @terminate@ .
rsisInstanceInterruptionBehavior :: Lens' RequestSpotInstances (Maybe InstanceInterruptionBehavior)
rsisInstanceInterruptionBehavior = lens _rsisInstanceInterruptionBehavior (\s a -> s {_rsisInstanceInterruptionBehavior = a})

-- | The maximum price per hour that you are willing to pay for a Spot Instance. The default is the On-Demand price.
rsisSpotPrice :: Lens' RequestSpotInstances (Maybe Text)
rsisSpotPrice = lens _rsisSpotPrice (\s a -> s {_rsisSpotPrice = a})

-- | The launch specification.
rsisLaunchSpecification :: Lens' RequestSpotInstances (Maybe RequestSpotLaunchSpecification)
rsisLaunchSpecification = lens _rsisLaunchSpecification (\s a -> s {_rsisLaunchSpecification = a})

-- | The user-specified name for a logical grouping of requests. When you specify an Availability Zone group in a Spot Instance request, all Spot Instances in the request are launched in the same Availability Zone. Instance proximity is maintained with this parameter, but the choice of Availability Zone is not. The group applies only to requests for Spot Instances of the same instance type. Any additional Spot Instance requests that are specified with the same Availability Zone group name are launched in that same Availability Zone, as long as at least one instance from the group is still active. If there is no active instance running in the Availability Zone group that you specify for a new Spot Instance request (all instances are terminated, the request is expired, or the maximum price you specified falls below current Spot price), then Amazon EC2 launches the instance in any Availability Zone where the constraint can be met. Consequently, the subsequent set of Spot Instances could be placed in a different zone from the original request, even if you specified the same Availability Zone group. Default: Instances are launched in any available Availability Zone.
rsisAvailabilityZoneGroup :: Lens' RequestSpotInstances (Maybe Text)
rsisAvailabilityZoneGroup = lens _rsisAvailabilityZoneGroup (\s a -> s {_rsisAvailabilityZoneGroup = a})

-- | The key-value pair for tagging the Spot Instance request on creation. The value for @ResourceType@ must be @spot-instances-request@ , otherwise the Spot Instance request fails. To tag the Spot Instance request after it has been created, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags> .
rsisTagSpecifications :: Lens' RequestSpotInstances [TagSpecification]
rsisTagSpecifications = lens _rsisTagSpecifications (\s a -> s {_rsisTagSpecifications = a}) . _Default . _Coerce

-- | The end date of the request, in UTC format (/YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).     * For a persistent request, the request remains active until the @ValidUntil@ date and time is reached. Otherwise, the request remains active until you cancel it.      * For a one-time request, the request remains active until all instances launch, the request is canceled, or the @ValidUntil@ date and time is reached. By default, the request is valid for 7 days from the date the request was created.
rsisValidUntil :: Lens' RequestSpotInstances (Maybe UTCTime)
rsisValidUntil = lens _rsisValidUntil (\s a -> s {_rsisValidUntil = a}) . mapping _Time

-- | The instance launch group. Launch groups are Spot Instances that launch together and terminate together. Default: Instances are launched and terminated individually
rsisLaunchGroup :: Lens' RequestSpotInstances (Maybe Text)
rsisLaunchGroup = lens _rsisLaunchGroup (\s a -> s {_rsisLaunchGroup = a})

-- | The Spot Instance request type. Default: @one-time@
rsisType :: Lens' RequestSpotInstances (Maybe SpotInstanceType)
rsisType = lens _rsisType (\s a -> s {_rsisType = a})

-- | The start date of the request. If this is a one-time request, the request becomes active at this date and time and remains active until all instances launch, the request expires, or the request is canceled. If the request is persistent, the request becomes active at this date and time and remains active until it expires or is canceled. The specified start date and time cannot be equal to the current date and time. You must specify a start date and time that occurs after the current date and time.
rsisValidFrom :: Lens' RequestSpotInstances (Maybe UTCTime)
rsisValidFrom = lens _rsisValidFrom (\s a -> s {_rsisValidFrom = a}) . mapping _Time

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
rsisDryRun :: Lens' RequestSpotInstances (Maybe Bool)
rsisDryRun = lens _rsisDryRun (\s a -> s {_rsisDryRun = a})

instance AWSRequest RequestSpotInstances where
  type Rs RequestSpotInstances = RequestSpotInstancesResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          RequestSpotInstancesResponse'
            <$> ( x .@? "spotInstanceRequestSet" .!@ mempty
                    >>= may (parseXMLList "item")
                )
            <*> (pure (fromEnum s))
      )

instance Hashable RequestSpotInstances

instance NFData RequestSpotInstances

instance ToHeaders RequestSpotInstances where
  toHeaders = const mempty

instance ToPath RequestSpotInstances where
  toPath = const "/"

instance ToQuery RequestSpotInstances where
  toQuery RequestSpotInstances' {..} =
    mconcat
      [ "Action" =: ("RequestSpotInstances" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "BlockDurationMinutes" =: _rsisBlockDurationMinutes,
        "ClientToken" =: _rsisClientToken,
        "InstanceCount" =: _rsisInstanceCount,
        "InstanceInterruptionBehavior"
          =: _rsisInstanceInterruptionBehavior,
        "SpotPrice" =: _rsisSpotPrice,
        "LaunchSpecification" =: _rsisLaunchSpecification,
        "AvailabilityZoneGroup" =: _rsisAvailabilityZoneGroup,
        toQuery
          (toQueryList "TagSpecification" <$> _rsisTagSpecifications),
        "ValidUntil" =: _rsisValidUntil,
        "LaunchGroup" =: _rsisLaunchGroup,
        "Type" =: _rsisType,
        "ValidFrom" =: _rsisValidFrom,
        "DryRun" =: _rsisDryRun
      ]

-- | Contains the output of RequestSpotInstances.
--
--
--
-- /See:/ 'requestSpotInstancesResponse' smart constructor.
data RequestSpotInstancesResponse = RequestSpotInstancesResponse'
  { _rsirsSpotInstanceRequests ::
      !(Maybe [SpotInstanceRequest]),
    _rsirsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RequestSpotInstancesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsirsSpotInstanceRequests' - One or more Spot Instance requests.
--
-- * 'rsirsResponseStatus' - -- | The response status code.
requestSpotInstancesResponse ::
  -- | 'rsirsResponseStatus'
  Int ->
  RequestSpotInstancesResponse
requestSpotInstancesResponse pResponseStatus_ =
  RequestSpotInstancesResponse'
    { _rsirsSpotInstanceRequests =
        Nothing,
      _rsirsResponseStatus = pResponseStatus_
    }

-- | One or more Spot Instance requests.
rsirsSpotInstanceRequests :: Lens' RequestSpotInstancesResponse [SpotInstanceRequest]
rsirsSpotInstanceRequests = lens _rsirsSpotInstanceRequests (\s a -> s {_rsirsSpotInstanceRequests = a}) . _Default . _Coerce

-- | -- | The response status code.
rsirsResponseStatus :: Lens' RequestSpotInstancesResponse Int
rsirsResponseStatus = lens _rsirsResponseStatus (\s a -> s {_rsirsResponseStatus = a})

instance NFData RequestSpotInstancesResponse
