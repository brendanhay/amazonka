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
-- Module      : Network.AWS.EC2.CreateCapacityReservation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Capacity Reservation with the specified attributes.
--
--
-- Capacity Reservations enable you to reserve capacity for your Amazon EC2 instances in a specific Availability Zone for any duration. This gives you the flexibility to selectively add capacity reservations and still get the Regional RI discounts for that usage. By creating Capacity Reservations, you ensure that you always have access to Amazon EC2 capacity when you need it, for as long as you need it. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-capacity-reservations.html Capacity Reservations> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- Your request to create a Capacity Reservation could fail if Amazon EC2 does not have sufficient capacity to fulfill the request. If your request fails due to Amazon EC2 capacity constraints, either try again at a later time, try in a different Availability Zone, or request a smaller capacity reservation. If your application is flexible across instance types and sizes, try to create a Capacity Reservation with different instance attributes.
--
-- Your request could also fail if the requested quantity exceeds your On-Demand Instance limit for the selected instance type. If your request fails due to limit constraints, increase your On-Demand Instance limit for the required instance type and try again. For more information about increasing your instance limits, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-resource-limits.html Amazon EC2 Service Limits> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.CreateCapacityReservation
  ( -- * Creating a Request
    createCapacityReservation,
    CreateCapacityReservation,

    -- * Request Lenses
    ccrClientToken,
    ccrAvailabilityZoneId,
    ccrEndDate,
    ccrEphemeralStorage,
    ccrInstanceMatchCriteria,
    ccrEBSOptimized,
    ccrTagSpecifications,
    ccrAvailabilityZone,
    ccrTenancy,
    ccrEndDateType,
    ccrDryRun,
    ccrInstanceType,
    ccrInstancePlatform,
    ccrInstanceCount,

    -- * Destructuring the Response
    createCapacityReservationResponse,
    CreateCapacityReservationResponse,

    -- * Response Lenses
    ccrrsCapacityReservation,
    ccrrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createCapacityReservation' smart constructor.
data CreateCapacityReservation = CreateCapacityReservation'
  { _ccrClientToken ::
      !(Maybe Text),
    _ccrAvailabilityZoneId :: !(Maybe Text),
    _ccrEndDate :: !(Maybe ISO8601),
    _ccrEphemeralStorage :: !(Maybe Bool),
    _ccrInstanceMatchCriteria ::
      !(Maybe InstanceMatchCriteria),
    _ccrEBSOptimized :: !(Maybe Bool),
    _ccrTagSpecifications ::
      !(Maybe [TagSpecification]),
    _ccrAvailabilityZone :: !(Maybe Text),
    _ccrTenancy ::
      !(Maybe CapacityReservationTenancy),
    _ccrEndDateType :: !(Maybe EndDateType),
    _ccrDryRun :: !(Maybe Bool),
    _ccrInstanceType :: !Text,
    _ccrInstancePlatform ::
      !CapacityReservationInstancePlatform,
    _ccrInstanceCount :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateCapacityReservation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccrClientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- * 'ccrAvailabilityZoneId' - The ID of the Availability Zone in which to create the Capacity Reservation.
--
-- * 'ccrEndDate' - The date and time at which the Capacity Reservation expires. When a Capacity Reservation expires, the reserved capacity is released and you can no longer launch instances into it. The Capacity Reservation's state changes to @expired@ when it reaches its end date and time. You must provide an @EndDate@ value if @EndDateType@ is @limited@ . Omit @EndDate@ if @EndDateType@ is @unlimited@ . If the @EndDateType@ is @limited@ , the Capacity Reservation is cancelled within an hour from the specified time. For example, if you specify 5/31/2019, 13:30:55, the Capacity Reservation is guaranteed to end between 13:30:55 and 14:30:55 on 5/31/2019.
--
-- * 'ccrEphemeralStorage' - Indicates whether the Capacity Reservation supports instances with temporary, block-level storage.
--
-- * 'ccrInstanceMatchCriteria' - Indicates the type of instance launches that the Capacity Reservation accepts. The options include:     * @open@ - The Capacity Reservation automatically matches all instances that have matching attributes (instance type, platform, and Availability Zone). Instances that have matching attributes run in the Capacity Reservation automatically without specifying any additional parameters.     * @targeted@ - The Capacity Reservation only accepts instances that have matching attributes (instance type, platform, and Availability Zone), and explicitly target the Capacity Reservation. This ensures that only permitted instances can use the reserved capacity.  Default: @open@
--
-- * 'ccrEBSOptimized' - Indicates whether the Capacity Reservation supports EBS-optimized instances. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS- optimized instance.
--
-- * 'ccrTagSpecifications' - The tags to apply to the Capacity Reservation during launch.
--
-- * 'ccrAvailabilityZone' - The Availability Zone in which to create the Capacity Reservation.
--
-- * 'ccrTenancy' - Indicates the tenancy of the Capacity Reservation. A Capacity Reservation can have one of the following tenancy settings:     * @default@ - The Capacity Reservation is created on hardware that is shared with other AWS accounts.     * @dedicated@ - The Capacity Reservation is created on single-tenant hardware that is dedicated to a single AWS account.
--
-- * 'ccrEndDateType' - Indicates the way in which the Capacity Reservation ends. A Capacity Reservation can have one of the following end types:     * @unlimited@ - The Capacity Reservation remains active until you explicitly cancel it. Do not provide an @EndDate@ if the @EndDateType@ is @unlimited@ .     * @limited@ - The Capacity Reservation expires automatically at a specified date and time. You must provide an @EndDate@ value if the @EndDateType@ value is @limited@ .
--
-- * 'ccrDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'ccrInstanceType' - The instance type for which to reserve capacity. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'ccrInstancePlatform' - The type of operating system for which to reserve capacity.
--
-- * 'ccrInstanceCount' - The number of instances for which to reserve capacity.
createCapacityReservation ::
  -- | 'ccrInstanceType'
  Text ->
  -- | 'ccrInstancePlatform'
  CapacityReservationInstancePlatform ->
  -- | 'ccrInstanceCount'
  Int ->
  CreateCapacityReservation
createCapacityReservation
  pInstanceType_
  pInstancePlatform_
  pInstanceCount_ =
    CreateCapacityReservation'
      { _ccrClientToken = Nothing,
        _ccrAvailabilityZoneId = Nothing,
        _ccrEndDate = Nothing,
        _ccrEphemeralStorage = Nothing,
        _ccrInstanceMatchCriteria = Nothing,
        _ccrEBSOptimized = Nothing,
        _ccrTagSpecifications = Nothing,
        _ccrAvailabilityZone = Nothing,
        _ccrTenancy = Nothing,
        _ccrEndDateType = Nothing,
        _ccrDryRun = Nothing,
        _ccrInstanceType = pInstanceType_,
        _ccrInstancePlatform = pInstancePlatform_,
        _ccrInstanceCount = pInstanceCount_
      }

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
ccrClientToken :: Lens' CreateCapacityReservation (Maybe Text)
ccrClientToken = lens _ccrClientToken (\s a -> s {_ccrClientToken = a})

-- | The ID of the Availability Zone in which to create the Capacity Reservation.
ccrAvailabilityZoneId :: Lens' CreateCapacityReservation (Maybe Text)
ccrAvailabilityZoneId = lens _ccrAvailabilityZoneId (\s a -> s {_ccrAvailabilityZoneId = a})

-- | The date and time at which the Capacity Reservation expires. When a Capacity Reservation expires, the reserved capacity is released and you can no longer launch instances into it. The Capacity Reservation's state changes to @expired@ when it reaches its end date and time. You must provide an @EndDate@ value if @EndDateType@ is @limited@ . Omit @EndDate@ if @EndDateType@ is @unlimited@ . If the @EndDateType@ is @limited@ , the Capacity Reservation is cancelled within an hour from the specified time. For example, if you specify 5/31/2019, 13:30:55, the Capacity Reservation is guaranteed to end between 13:30:55 and 14:30:55 on 5/31/2019.
ccrEndDate :: Lens' CreateCapacityReservation (Maybe UTCTime)
ccrEndDate = lens _ccrEndDate (\s a -> s {_ccrEndDate = a}) . mapping _Time

-- | Indicates whether the Capacity Reservation supports instances with temporary, block-level storage.
ccrEphemeralStorage :: Lens' CreateCapacityReservation (Maybe Bool)
ccrEphemeralStorage = lens _ccrEphemeralStorage (\s a -> s {_ccrEphemeralStorage = a})

-- | Indicates the type of instance launches that the Capacity Reservation accepts. The options include:     * @open@ - The Capacity Reservation automatically matches all instances that have matching attributes (instance type, platform, and Availability Zone). Instances that have matching attributes run in the Capacity Reservation automatically without specifying any additional parameters.     * @targeted@ - The Capacity Reservation only accepts instances that have matching attributes (instance type, platform, and Availability Zone), and explicitly target the Capacity Reservation. This ensures that only permitted instances can use the reserved capacity.  Default: @open@
ccrInstanceMatchCriteria :: Lens' CreateCapacityReservation (Maybe InstanceMatchCriteria)
ccrInstanceMatchCriteria = lens _ccrInstanceMatchCriteria (\s a -> s {_ccrInstanceMatchCriteria = a})

-- | Indicates whether the Capacity Reservation supports EBS-optimized instances. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS- optimized instance.
ccrEBSOptimized :: Lens' CreateCapacityReservation (Maybe Bool)
ccrEBSOptimized = lens _ccrEBSOptimized (\s a -> s {_ccrEBSOptimized = a})

-- | The tags to apply to the Capacity Reservation during launch.
ccrTagSpecifications :: Lens' CreateCapacityReservation [TagSpecification]
ccrTagSpecifications = lens _ccrTagSpecifications (\s a -> s {_ccrTagSpecifications = a}) . _Default . _Coerce

-- | The Availability Zone in which to create the Capacity Reservation.
ccrAvailabilityZone :: Lens' CreateCapacityReservation (Maybe Text)
ccrAvailabilityZone = lens _ccrAvailabilityZone (\s a -> s {_ccrAvailabilityZone = a})

-- | Indicates the tenancy of the Capacity Reservation. A Capacity Reservation can have one of the following tenancy settings:     * @default@ - The Capacity Reservation is created on hardware that is shared with other AWS accounts.     * @dedicated@ - The Capacity Reservation is created on single-tenant hardware that is dedicated to a single AWS account.
ccrTenancy :: Lens' CreateCapacityReservation (Maybe CapacityReservationTenancy)
ccrTenancy = lens _ccrTenancy (\s a -> s {_ccrTenancy = a})

-- | Indicates the way in which the Capacity Reservation ends. A Capacity Reservation can have one of the following end types:     * @unlimited@ - The Capacity Reservation remains active until you explicitly cancel it. Do not provide an @EndDate@ if the @EndDateType@ is @unlimited@ .     * @limited@ - The Capacity Reservation expires automatically at a specified date and time. You must provide an @EndDate@ value if the @EndDateType@ value is @limited@ .
ccrEndDateType :: Lens' CreateCapacityReservation (Maybe EndDateType)
ccrEndDateType = lens _ccrEndDateType (\s a -> s {_ccrEndDateType = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
ccrDryRun :: Lens' CreateCapacityReservation (Maybe Bool)
ccrDryRun = lens _ccrDryRun (\s a -> s {_ccrDryRun = a})

-- | The instance type for which to reserve capacity. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types> in the /Amazon Elastic Compute Cloud User Guide/ .
ccrInstanceType :: Lens' CreateCapacityReservation Text
ccrInstanceType = lens _ccrInstanceType (\s a -> s {_ccrInstanceType = a})

-- | The type of operating system for which to reserve capacity.
ccrInstancePlatform :: Lens' CreateCapacityReservation CapacityReservationInstancePlatform
ccrInstancePlatform = lens _ccrInstancePlatform (\s a -> s {_ccrInstancePlatform = a})

-- | The number of instances for which to reserve capacity.
ccrInstanceCount :: Lens' CreateCapacityReservation Int
ccrInstanceCount = lens _ccrInstanceCount (\s a -> s {_ccrInstanceCount = a})

instance AWSRequest CreateCapacityReservation where
  type
    Rs CreateCapacityReservation =
      CreateCapacityReservationResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          CreateCapacityReservationResponse'
            <$> (x .@? "capacityReservation") <*> (pure (fromEnum s))
      )

instance Hashable CreateCapacityReservation

instance NFData CreateCapacityReservation

instance ToHeaders CreateCapacityReservation where
  toHeaders = const mempty

instance ToPath CreateCapacityReservation where
  toPath = const "/"

instance ToQuery CreateCapacityReservation where
  toQuery CreateCapacityReservation' {..} =
    mconcat
      [ "Action" =: ("CreateCapacityReservation" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "ClientToken" =: _ccrClientToken,
        "AvailabilityZoneId" =: _ccrAvailabilityZoneId,
        "EndDate" =: _ccrEndDate,
        "EphemeralStorage" =: _ccrEphemeralStorage,
        "InstanceMatchCriteria" =: _ccrInstanceMatchCriteria,
        "EbsOptimized" =: _ccrEBSOptimized,
        toQuery
          (toQueryList "TagSpecifications" <$> _ccrTagSpecifications),
        "AvailabilityZone" =: _ccrAvailabilityZone,
        "Tenancy" =: _ccrTenancy,
        "EndDateType" =: _ccrEndDateType,
        "DryRun" =: _ccrDryRun,
        "InstanceType" =: _ccrInstanceType,
        "InstancePlatform" =: _ccrInstancePlatform,
        "InstanceCount" =: _ccrInstanceCount
      ]

-- | /See:/ 'createCapacityReservationResponse' smart constructor.
data CreateCapacityReservationResponse = CreateCapacityReservationResponse'
  { _ccrrsCapacityReservation ::
      !( Maybe
           CapacityReservation
       ),
    _ccrrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateCapacityReservationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccrrsCapacityReservation' - Information about the Capacity Reservation.
--
-- * 'ccrrsResponseStatus' - -- | The response status code.
createCapacityReservationResponse ::
  -- | 'ccrrsResponseStatus'
  Int ->
  CreateCapacityReservationResponse
createCapacityReservationResponse pResponseStatus_ =
  CreateCapacityReservationResponse'
    { _ccrrsCapacityReservation =
        Nothing,
      _ccrrsResponseStatus = pResponseStatus_
    }

-- | Information about the Capacity Reservation.
ccrrsCapacityReservation :: Lens' CreateCapacityReservationResponse (Maybe CapacityReservation)
ccrrsCapacityReservation = lens _ccrrsCapacityReservation (\s a -> s {_ccrrsCapacityReservation = a})

-- | -- | The response status code.
ccrrsResponseStatus :: Lens' CreateCapacityReservationResponse Int
ccrrsResponseStatus = lens _ccrrsResponseStatus (\s a -> s {_ccrrsResponseStatus = a})

instance NFData CreateCapacityReservationResponse
