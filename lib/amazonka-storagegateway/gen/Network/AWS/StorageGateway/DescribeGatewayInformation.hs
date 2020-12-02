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
-- Module      : Network.AWS.StorageGateway.DescribeGatewayInformation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata about a gateway such as its name, network interfaces, configured time zone, and the state (whether the gateway is running or not). To specify which gateway to describe, use the Amazon Resource Name (ARN) of the gateway in your request.
module Network.AWS.StorageGateway.DescribeGatewayInformation
  ( -- * Creating a Request
    describeGatewayInformation,
    DescribeGatewayInformation,

    -- * Request Lenses
    dgiGatewayARN,

    -- * Destructuring the Response
    describeGatewayInformationResponse,
    DescribeGatewayInformationResponse,

    -- * Response Lenses
    dgirsGatewayState,
    dgirsEC2InstanceRegion,
    dgirsGatewayARN,
    dgirsGatewayNetworkInterfaces,
    dgirsEC2InstanceId,
    dgirsNextUpdateAvailabilityDate,
    dgirsEndpointType,
    dgirsDeprecationDate,
    dgirsLastSoftwareUpdate,
    dgirsGatewayName,
    dgirsGatewayId,
    dgirsHostEnvironment,
    dgirsGatewayType,
    dgirsGatewayTimezone,
    dgirsSoftwareUpdatesEndDate,
    dgirsCloudWatchLogGroupARN,
    dgirsVPCEndpoint,
    dgirsTags,
    dgirsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types

-- | A JSON object containing the ID of the gateway.
--
--
--
-- /See:/ 'describeGatewayInformation' smart constructor.
newtype DescribeGatewayInformation = DescribeGatewayInformation'
  { _dgiGatewayARN ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeGatewayInformation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dgiGatewayARN' - Undocumented member.
describeGatewayInformation ::
  -- | 'dgiGatewayARN'
  Text ->
  DescribeGatewayInformation
describeGatewayInformation pGatewayARN_ =
  DescribeGatewayInformation' {_dgiGatewayARN = pGatewayARN_}

-- | Undocumented member.
dgiGatewayARN :: Lens' DescribeGatewayInformation Text
dgiGatewayARN = lens _dgiGatewayARN (\s a -> s {_dgiGatewayARN = a})

instance AWSRequest DescribeGatewayInformation where
  type
    Rs DescribeGatewayInformation =
      DescribeGatewayInformationResponse
  request = postJSON storageGateway
  response =
    receiveJSON
      ( \s h x ->
          DescribeGatewayInformationResponse'
            <$> (x .?> "GatewayState")
            <*> (x .?> "Ec2InstanceRegion")
            <*> (x .?> "GatewayARN")
            <*> (x .?> "GatewayNetworkInterfaces" .!@ mempty)
            <*> (x .?> "Ec2InstanceId")
            <*> (x .?> "NextUpdateAvailabilityDate")
            <*> (x .?> "EndpointType")
            <*> (x .?> "DeprecationDate")
            <*> (x .?> "LastSoftwareUpdate")
            <*> (x .?> "GatewayName")
            <*> (x .?> "GatewayId")
            <*> (x .?> "HostEnvironment")
            <*> (x .?> "GatewayType")
            <*> (x .?> "GatewayTimezone")
            <*> (x .?> "SoftwareUpdatesEndDate")
            <*> (x .?> "CloudWatchLogGroupARN")
            <*> (x .?> "VPCEndpoint")
            <*> (x .?> "Tags" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeGatewayInformation

instance NFData DescribeGatewayInformation

instance ToHeaders DescribeGatewayInformation where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "StorageGateway_20130630.DescribeGatewayInformation" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeGatewayInformation where
  toJSON DescribeGatewayInformation' {..} =
    object (catMaybes [Just ("GatewayARN" .= _dgiGatewayARN)])

instance ToPath DescribeGatewayInformation where
  toPath = const "/"

instance ToQuery DescribeGatewayInformation where
  toQuery = const mempty

-- | A JSON object containing the following fields:
--
--
--
-- /See:/ 'describeGatewayInformationResponse' smart constructor.
data DescribeGatewayInformationResponse = DescribeGatewayInformationResponse'
  { _dgirsGatewayState ::
      !(Maybe Text),
    _dgirsEC2InstanceRegion ::
      !(Maybe Text),
    _dgirsGatewayARN ::
      !(Maybe Text),
    _dgirsGatewayNetworkInterfaces ::
      !( Maybe
           [NetworkInterface]
       ),
    _dgirsEC2InstanceId ::
      !(Maybe Text),
    _dgirsNextUpdateAvailabilityDate ::
      !(Maybe Text),
    _dgirsEndpointType ::
      !(Maybe Text),
    _dgirsDeprecationDate ::
      !(Maybe Text),
    _dgirsLastSoftwareUpdate ::
      !(Maybe Text),
    _dgirsGatewayName ::
      !(Maybe Text),
    _dgirsGatewayId ::
      !(Maybe Text),
    _dgirsHostEnvironment ::
      !( Maybe
           HostEnvironment
       ),
    _dgirsGatewayType ::
      !(Maybe Text),
    _dgirsGatewayTimezone ::
      !(Maybe Text),
    _dgirsSoftwareUpdatesEndDate ::
      !(Maybe Text),
    _dgirsCloudWatchLogGroupARN ::
      !(Maybe Text),
    _dgirsVPCEndpoint ::
      !(Maybe Text),
    _dgirsTags ::
      !(Maybe [Tag]),
    _dgirsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeGatewayInformationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dgirsGatewayState' - A value that indicates the operating state of the gateway.
--
-- * 'dgirsEC2InstanceRegion' - The AWS Region where the Amazon EC2 instance is located.
--
-- * 'dgirsGatewayARN' - Undocumented member.
--
-- * 'dgirsGatewayNetworkInterfaces' - A 'NetworkInterface' array that contains descriptions of the gateway network interfaces.
--
-- * 'dgirsEC2InstanceId' - The ID of the Amazon EC2 instance that was used to launch the gateway.
--
-- * 'dgirsNextUpdateAvailabilityDate' - The date on which an update to the gateway is available. This date is in the time zone of the gateway. If the gateway is not available for an update this field is not returned in the response.
--
-- * 'dgirsEndpointType' - The type of endpoint for your gateway. Valid Values: @STANDARD@ | @FIPS@
--
-- * 'dgirsDeprecationDate' - Date after which this gateway will not receive software updates for new features and bug fixes.
--
-- * 'dgirsLastSoftwareUpdate' - The date on which the last software update was applied to the gateway. If the gateway has never been updated, this field does not return a value in the response.
--
-- * 'dgirsGatewayName' - The name you configured for your gateway.
--
-- * 'dgirsGatewayId' - The unique identifier assigned to your gateway during activation. This ID becomes part of the gateway Amazon Resource Name (ARN), which you use as input for other operations.
--
-- * 'dgirsHostEnvironment' - The type of hypervisor environment used by the host.
--
-- * 'dgirsGatewayType' - The type of the gateway.
--
-- * 'dgirsGatewayTimezone' - A value that indicates the time zone configured for the gateway.
--
-- * 'dgirsSoftwareUpdatesEndDate' - Date after which this gateway will not receive software updates for new features.
--
-- * 'dgirsCloudWatchLogGroupARN' - The Amazon Resource Name (ARN) of the Amazon CloudWatch log group that is used to monitor events in the gateway.
--
-- * 'dgirsVPCEndpoint' - The configuration settings for the virtual private cloud (VPC) endpoint for your gateway.
--
-- * 'dgirsTags' - A list of up to 50 tags assigned to the gateway, sorted alphabetically by key name. Each tag is a key-value pair. For a gateway with more than 10 tags assigned, you can view all tags using the @ListTagsForResource@ API operation.
--
-- * 'dgirsResponseStatus' - -- | The response status code.
describeGatewayInformationResponse ::
  -- | 'dgirsResponseStatus'
  Int ->
  DescribeGatewayInformationResponse
describeGatewayInformationResponse pResponseStatus_ =
  DescribeGatewayInformationResponse'
    { _dgirsGatewayState = Nothing,
      _dgirsEC2InstanceRegion = Nothing,
      _dgirsGatewayARN = Nothing,
      _dgirsGatewayNetworkInterfaces = Nothing,
      _dgirsEC2InstanceId = Nothing,
      _dgirsNextUpdateAvailabilityDate = Nothing,
      _dgirsEndpointType = Nothing,
      _dgirsDeprecationDate = Nothing,
      _dgirsLastSoftwareUpdate = Nothing,
      _dgirsGatewayName = Nothing,
      _dgirsGatewayId = Nothing,
      _dgirsHostEnvironment = Nothing,
      _dgirsGatewayType = Nothing,
      _dgirsGatewayTimezone = Nothing,
      _dgirsSoftwareUpdatesEndDate = Nothing,
      _dgirsCloudWatchLogGroupARN = Nothing,
      _dgirsVPCEndpoint = Nothing,
      _dgirsTags = Nothing,
      _dgirsResponseStatus = pResponseStatus_
    }

-- | A value that indicates the operating state of the gateway.
dgirsGatewayState :: Lens' DescribeGatewayInformationResponse (Maybe Text)
dgirsGatewayState = lens _dgirsGatewayState (\s a -> s {_dgirsGatewayState = a})

-- | The AWS Region where the Amazon EC2 instance is located.
dgirsEC2InstanceRegion :: Lens' DescribeGatewayInformationResponse (Maybe Text)
dgirsEC2InstanceRegion = lens _dgirsEC2InstanceRegion (\s a -> s {_dgirsEC2InstanceRegion = a})

-- | Undocumented member.
dgirsGatewayARN :: Lens' DescribeGatewayInformationResponse (Maybe Text)
dgirsGatewayARN = lens _dgirsGatewayARN (\s a -> s {_dgirsGatewayARN = a})

-- | A 'NetworkInterface' array that contains descriptions of the gateway network interfaces.
dgirsGatewayNetworkInterfaces :: Lens' DescribeGatewayInformationResponse [NetworkInterface]
dgirsGatewayNetworkInterfaces = lens _dgirsGatewayNetworkInterfaces (\s a -> s {_dgirsGatewayNetworkInterfaces = a}) . _Default . _Coerce

-- | The ID of the Amazon EC2 instance that was used to launch the gateway.
dgirsEC2InstanceId :: Lens' DescribeGatewayInformationResponse (Maybe Text)
dgirsEC2InstanceId = lens _dgirsEC2InstanceId (\s a -> s {_dgirsEC2InstanceId = a})

-- | The date on which an update to the gateway is available. This date is in the time zone of the gateway. If the gateway is not available for an update this field is not returned in the response.
dgirsNextUpdateAvailabilityDate :: Lens' DescribeGatewayInformationResponse (Maybe Text)
dgirsNextUpdateAvailabilityDate = lens _dgirsNextUpdateAvailabilityDate (\s a -> s {_dgirsNextUpdateAvailabilityDate = a})

-- | The type of endpoint for your gateway. Valid Values: @STANDARD@ | @FIPS@
dgirsEndpointType :: Lens' DescribeGatewayInformationResponse (Maybe Text)
dgirsEndpointType = lens _dgirsEndpointType (\s a -> s {_dgirsEndpointType = a})

-- | Date after which this gateway will not receive software updates for new features and bug fixes.
dgirsDeprecationDate :: Lens' DescribeGatewayInformationResponse (Maybe Text)
dgirsDeprecationDate = lens _dgirsDeprecationDate (\s a -> s {_dgirsDeprecationDate = a})

-- | The date on which the last software update was applied to the gateway. If the gateway has never been updated, this field does not return a value in the response.
dgirsLastSoftwareUpdate :: Lens' DescribeGatewayInformationResponse (Maybe Text)
dgirsLastSoftwareUpdate = lens _dgirsLastSoftwareUpdate (\s a -> s {_dgirsLastSoftwareUpdate = a})

-- | The name you configured for your gateway.
dgirsGatewayName :: Lens' DescribeGatewayInformationResponse (Maybe Text)
dgirsGatewayName = lens _dgirsGatewayName (\s a -> s {_dgirsGatewayName = a})

-- | The unique identifier assigned to your gateway during activation. This ID becomes part of the gateway Amazon Resource Name (ARN), which you use as input for other operations.
dgirsGatewayId :: Lens' DescribeGatewayInformationResponse (Maybe Text)
dgirsGatewayId = lens _dgirsGatewayId (\s a -> s {_dgirsGatewayId = a})

-- | The type of hypervisor environment used by the host.
dgirsHostEnvironment :: Lens' DescribeGatewayInformationResponse (Maybe HostEnvironment)
dgirsHostEnvironment = lens _dgirsHostEnvironment (\s a -> s {_dgirsHostEnvironment = a})

-- | The type of the gateway.
dgirsGatewayType :: Lens' DescribeGatewayInformationResponse (Maybe Text)
dgirsGatewayType = lens _dgirsGatewayType (\s a -> s {_dgirsGatewayType = a})

-- | A value that indicates the time zone configured for the gateway.
dgirsGatewayTimezone :: Lens' DescribeGatewayInformationResponse (Maybe Text)
dgirsGatewayTimezone = lens _dgirsGatewayTimezone (\s a -> s {_dgirsGatewayTimezone = a})

-- | Date after which this gateway will not receive software updates for new features.
dgirsSoftwareUpdatesEndDate :: Lens' DescribeGatewayInformationResponse (Maybe Text)
dgirsSoftwareUpdatesEndDate = lens _dgirsSoftwareUpdatesEndDate (\s a -> s {_dgirsSoftwareUpdatesEndDate = a})

-- | The Amazon Resource Name (ARN) of the Amazon CloudWatch log group that is used to monitor events in the gateway.
dgirsCloudWatchLogGroupARN :: Lens' DescribeGatewayInformationResponse (Maybe Text)
dgirsCloudWatchLogGroupARN = lens _dgirsCloudWatchLogGroupARN (\s a -> s {_dgirsCloudWatchLogGroupARN = a})

-- | The configuration settings for the virtual private cloud (VPC) endpoint for your gateway.
dgirsVPCEndpoint :: Lens' DescribeGatewayInformationResponse (Maybe Text)
dgirsVPCEndpoint = lens _dgirsVPCEndpoint (\s a -> s {_dgirsVPCEndpoint = a})

-- | A list of up to 50 tags assigned to the gateway, sorted alphabetically by key name. Each tag is a key-value pair. For a gateway with more than 10 tags assigned, you can view all tags using the @ListTagsForResource@ API operation.
dgirsTags :: Lens' DescribeGatewayInformationResponse [Tag]
dgirsTags = lens _dgirsTags (\s a -> s {_dgirsTags = a}) . _Default . _Coerce

-- | -- | The response status code.
dgirsResponseStatus :: Lens' DescribeGatewayInformationResponse Int
dgirsResponseStatus = lens _dgirsResponseStatus (\s a -> s {_dgirsResponseStatus = a})

instance NFData DescribeGatewayInformationResponse
