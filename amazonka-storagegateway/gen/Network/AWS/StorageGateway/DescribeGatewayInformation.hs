{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DescribeGatewayInformation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata about a gateway such as its name, network interfaces,
-- configured time zone, and the state (whether the gateway is running or
-- not). To specify which gateway to describe, use the Amazon Resource Name
-- (ARN) of the gateway in your request.
module Network.AWS.StorageGateway.DescribeGatewayInformation
  ( -- * Creating a Request
    DescribeGatewayInformation (..),
    newDescribeGatewayInformation,

    -- * Request Lenses
    describeGatewayInformation_gatewayARN,

    -- * Destructuring the Response
    DescribeGatewayInformationResponse (..),
    newDescribeGatewayInformationResponse,

    -- * Response Lenses
    describeGatewayInformationResponse_gatewayState,
    describeGatewayInformationResponse_gatewayName,
    describeGatewayInformationResponse_deprecationDate,
    describeGatewayInformationResponse_softwareUpdatesEndDate,
    describeGatewayInformationResponse_endpointType,
    describeGatewayInformationResponse_nextUpdateAvailabilityDate,
    describeGatewayInformationResponse_gatewayTimezone,
    describeGatewayInformationResponse_gatewayNetworkInterfaces,
    describeGatewayInformationResponse_gatewayType,
    describeGatewayInformationResponse_ec2InstanceRegion,
    describeGatewayInformationResponse_lastSoftwareUpdate,
    describeGatewayInformationResponse_tags,
    describeGatewayInformationResponse_vPCEndpoint,
    describeGatewayInformationResponse_cloudWatchLogGroupARN,
    describeGatewayInformationResponse_ec2InstanceId,
    describeGatewayInformationResponse_hostEnvironment,
    describeGatewayInformationResponse_gatewayARN,
    describeGatewayInformationResponse_gatewayId,
    describeGatewayInformationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | A JSON object containing the ID of the gateway.
--
-- /See:/ 'newDescribeGatewayInformation' smart constructor.
data DescribeGatewayInformation = DescribeGatewayInformation'
  { gatewayARN :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeGatewayInformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'describeGatewayInformation_gatewayARN' - Undocumented member.
newDescribeGatewayInformation ::
  -- | 'gatewayARN'
  Core.Text ->
  DescribeGatewayInformation
newDescribeGatewayInformation pGatewayARN_ =
  DescribeGatewayInformation'
    { gatewayARN =
        pGatewayARN_
    }

-- | Undocumented member.
describeGatewayInformation_gatewayARN :: Lens.Lens' DescribeGatewayInformation Core.Text
describeGatewayInformation_gatewayARN = Lens.lens (\DescribeGatewayInformation' {gatewayARN} -> gatewayARN) (\s@DescribeGatewayInformation' {} a -> s {gatewayARN = a} :: DescribeGatewayInformation)

instance Core.AWSRequest DescribeGatewayInformation where
  type
    AWSResponse DescribeGatewayInformation =
      DescribeGatewayInformationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeGatewayInformationResponse'
            Core.<$> (x Core..?> "GatewayState")
            Core.<*> (x Core..?> "GatewayName")
            Core.<*> (x Core..?> "DeprecationDate")
            Core.<*> (x Core..?> "SoftwareUpdatesEndDate")
            Core.<*> (x Core..?> "EndpointType")
            Core.<*> (x Core..?> "NextUpdateAvailabilityDate")
            Core.<*> (x Core..?> "GatewayTimezone")
            Core.<*> ( x Core..?> "GatewayNetworkInterfaces"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "GatewayType")
            Core.<*> (x Core..?> "Ec2InstanceRegion")
            Core.<*> (x Core..?> "LastSoftwareUpdate")
            Core.<*> (x Core..?> "Tags" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "VPCEndpoint")
            Core.<*> (x Core..?> "CloudWatchLogGroupARN")
            Core.<*> (x Core..?> "Ec2InstanceId")
            Core.<*> (x Core..?> "HostEnvironment")
            Core.<*> (x Core..?> "GatewayARN")
            Core.<*> (x Core..?> "GatewayId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeGatewayInformation

instance Core.NFData DescribeGatewayInformation

instance Core.ToHeaders DescribeGatewayInformation where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.DescribeGatewayInformation" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeGatewayInformation where
  toJSON DescribeGatewayInformation' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("GatewayARN" Core..= gatewayARN)]
      )

instance Core.ToPath DescribeGatewayInformation where
  toPath = Core.const "/"

instance Core.ToQuery DescribeGatewayInformation where
  toQuery = Core.const Core.mempty

-- | A JSON object containing the following fields:
--
-- /See:/ 'newDescribeGatewayInformationResponse' smart constructor.
data DescribeGatewayInformationResponse = DescribeGatewayInformationResponse'
  { -- | A value that indicates the operating state of the gateway.
    gatewayState :: Core.Maybe Core.Text,
    -- | The name you configured for your gateway.
    gatewayName :: Core.Maybe Core.Text,
    -- | Date after which this gateway will not receive software updates for new
    -- features and bug fixes.
    deprecationDate :: Core.Maybe Core.Text,
    -- | Date after which this gateway will not receive software updates for new
    -- features.
    softwareUpdatesEndDate :: Core.Maybe Core.Text,
    -- | The type of endpoint for your gateway.
    --
    -- Valid Values: @STANDARD@ | @FIPS@
    endpointType :: Core.Maybe Core.Text,
    -- | The date on which an update to the gateway is available. This date is in
    -- the time zone of the gateway. If the gateway is not available for an
    -- update this field is not returned in the response.
    nextUpdateAvailabilityDate :: Core.Maybe Core.Text,
    -- | A value that indicates the time zone configured for the gateway.
    gatewayTimezone :: Core.Maybe Core.Text,
    -- | A NetworkInterface array that contains descriptions of the gateway
    -- network interfaces.
    gatewayNetworkInterfaces :: Core.Maybe [NetworkInterface],
    -- | The type of the gateway.
    gatewayType :: Core.Maybe Core.Text,
    -- | The AWS Region where the Amazon EC2 instance is located.
    ec2InstanceRegion :: Core.Maybe Core.Text,
    -- | The date on which the last software update was applied to the gateway.
    -- If the gateway has never been updated, this field does not return a
    -- value in the response.
    lastSoftwareUpdate :: Core.Maybe Core.Text,
    -- | A list of up to 50 tags assigned to the gateway, sorted alphabetically
    -- by key name. Each tag is a key-value pair. For a gateway with more than
    -- 10 tags assigned, you can view all tags using the @ListTagsForResource@
    -- API operation.
    tags :: Core.Maybe [Tag],
    -- | The configuration settings for the virtual private cloud (VPC) endpoint
    -- for your gateway.
    vPCEndpoint :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon CloudWatch log group that
    -- is used to monitor events in the gateway.
    cloudWatchLogGroupARN :: Core.Maybe Core.Text,
    -- | The ID of the Amazon EC2 instance that was used to launch the gateway.
    ec2InstanceId :: Core.Maybe Core.Text,
    -- | The type of hypervisor environment used by the host.
    hostEnvironment :: Core.Maybe HostEnvironment,
    gatewayARN :: Core.Maybe Core.Text,
    -- | The unique identifier assigned to your gateway during activation. This
    -- ID becomes part of the gateway Amazon Resource Name (ARN), which you use
    -- as input for other operations.
    gatewayId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeGatewayInformationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayState', 'describeGatewayInformationResponse_gatewayState' - A value that indicates the operating state of the gateway.
--
-- 'gatewayName', 'describeGatewayInformationResponse_gatewayName' - The name you configured for your gateway.
--
-- 'deprecationDate', 'describeGatewayInformationResponse_deprecationDate' - Date after which this gateway will not receive software updates for new
-- features and bug fixes.
--
-- 'softwareUpdatesEndDate', 'describeGatewayInformationResponse_softwareUpdatesEndDate' - Date after which this gateway will not receive software updates for new
-- features.
--
-- 'endpointType', 'describeGatewayInformationResponse_endpointType' - The type of endpoint for your gateway.
--
-- Valid Values: @STANDARD@ | @FIPS@
--
-- 'nextUpdateAvailabilityDate', 'describeGatewayInformationResponse_nextUpdateAvailabilityDate' - The date on which an update to the gateway is available. This date is in
-- the time zone of the gateway. If the gateway is not available for an
-- update this field is not returned in the response.
--
-- 'gatewayTimezone', 'describeGatewayInformationResponse_gatewayTimezone' - A value that indicates the time zone configured for the gateway.
--
-- 'gatewayNetworkInterfaces', 'describeGatewayInformationResponse_gatewayNetworkInterfaces' - A NetworkInterface array that contains descriptions of the gateway
-- network interfaces.
--
-- 'gatewayType', 'describeGatewayInformationResponse_gatewayType' - The type of the gateway.
--
-- 'ec2InstanceRegion', 'describeGatewayInformationResponse_ec2InstanceRegion' - The AWS Region where the Amazon EC2 instance is located.
--
-- 'lastSoftwareUpdate', 'describeGatewayInformationResponse_lastSoftwareUpdate' - The date on which the last software update was applied to the gateway.
-- If the gateway has never been updated, this field does not return a
-- value in the response.
--
-- 'tags', 'describeGatewayInformationResponse_tags' - A list of up to 50 tags assigned to the gateway, sorted alphabetically
-- by key name. Each tag is a key-value pair. For a gateway with more than
-- 10 tags assigned, you can view all tags using the @ListTagsForResource@
-- API operation.
--
-- 'vPCEndpoint', 'describeGatewayInformationResponse_vPCEndpoint' - The configuration settings for the virtual private cloud (VPC) endpoint
-- for your gateway.
--
-- 'cloudWatchLogGroupARN', 'describeGatewayInformationResponse_cloudWatchLogGroupARN' - The Amazon Resource Name (ARN) of the Amazon CloudWatch log group that
-- is used to monitor events in the gateway.
--
-- 'ec2InstanceId', 'describeGatewayInformationResponse_ec2InstanceId' - The ID of the Amazon EC2 instance that was used to launch the gateway.
--
-- 'hostEnvironment', 'describeGatewayInformationResponse_hostEnvironment' - The type of hypervisor environment used by the host.
--
-- 'gatewayARN', 'describeGatewayInformationResponse_gatewayARN' - Undocumented member.
--
-- 'gatewayId', 'describeGatewayInformationResponse_gatewayId' - The unique identifier assigned to your gateway during activation. This
-- ID becomes part of the gateway Amazon Resource Name (ARN), which you use
-- as input for other operations.
--
-- 'httpStatus', 'describeGatewayInformationResponse_httpStatus' - The response's http status code.
newDescribeGatewayInformationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeGatewayInformationResponse
newDescribeGatewayInformationResponse pHttpStatus_ =
  DescribeGatewayInformationResponse'
    { gatewayState =
        Core.Nothing,
      gatewayName = Core.Nothing,
      deprecationDate = Core.Nothing,
      softwareUpdatesEndDate = Core.Nothing,
      endpointType = Core.Nothing,
      nextUpdateAvailabilityDate =
        Core.Nothing,
      gatewayTimezone = Core.Nothing,
      gatewayNetworkInterfaces = Core.Nothing,
      gatewayType = Core.Nothing,
      ec2InstanceRegion = Core.Nothing,
      lastSoftwareUpdate = Core.Nothing,
      tags = Core.Nothing,
      vPCEndpoint = Core.Nothing,
      cloudWatchLogGroupARN = Core.Nothing,
      ec2InstanceId = Core.Nothing,
      hostEnvironment = Core.Nothing,
      gatewayARN = Core.Nothing,
      gatewayId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A value that indicates the operating state of the gateway.
describeGatewayInformationResponse_gatewayState :: Lens.Lens' DescribeGatewayInformationResponse (Core.Maybe Core.Text)
describeGatewayInformationResponse_gatewayState = Lens.lens (\DescribeGatewayInformationResponse' {gatewayState} -> gatewayState) (\s@DescribeGatewayInformationResponse' {} a -> s {gatewayState = a} :: DescribeGatewayInformationResponse)

-- | The name you configured for your gateway.
describeGatewayInformationResponse_gatewayName :: Lens.Lens' DescribeGatewayInformationResponse (Core.Maybe Core.Text)
describeGatewayInformationResponse_gatewayName = Lens.lens (\DescribeGatewayInformationResponse' {gatewayName} -> gatewayName) (\s@DescribeGatewayInformationResponse' {} a -> s {gatewayName = a} :: DescribeGatewayInformationResponse)

-- | Date after which this gateway will not receive software updates for new
-- features and bug fixes.
describeGatewayInformationResponse_deprecationDate :: Lens.Lens' DescribeGatewayInformationResponse (Core.Maybe Core.Text)
describeGatewayInformationResponse_deprecationDate = Lens.lens (\DescribeGatewayInformationResponse' {deprecationDate} -> deprecationDate) (\s@DescribeGatewayInformationResponse' {} a -> s {deprecationDate = a} :: DescribeGatewayInformationResponse)

-- | Date after which this gateway will not receive software updates for new
-- features.
describeGatewayInformationResponse_softwareUpdatesEndDate :: Lens.Lens' DescribeGatewayInformationResponse (Core.Maybe Core.Text)
describeGatewayInformationResponse_softwareUpdatesEndDate = Lens.lens (\DescribeGatewayInformationResponse' {softwareUpdatesEndDate} -> softwareUpdatesEndDate) (\s@DescribeGatewayInformationResponse' {} a -> s {softwareUpdatesEndDate = a} :: DescribeGatewayInformationResponse)

-- | The type of endpoint for your gateway.
--
-- Valid Values: @STANDARD@ | @FIPS@
describeGatewayInformationResponse_endpointType :: Lens.Lens' DescribeGatewayInformationResponse (Core.Maybe Core.Text)
describeGatewayInformationResponse_endpointType = Lens.lens (\DescribeGatewayInformationResponse' {endpointType} -> endpointType) (\s@DescribeGatewayInformationResponse' {} a -> s {endpointType = a} :: DescribeGatewayInformationResponse)

-- | The date on which an update to the gateway is available. This date is in
-- the time zone of the gateway. If the gateway is not available for an
-- update this field is not returned in the response.
describeGatewayInformationResponse_nextUpdateAvailabilityDate :: Lens.Lens' DescribeGatewayInformationResponse (Core.Maybe Core.Text)
describeGatewayInformationResponse_nextUpdateAvailabilityDate = Lens.lens (\DescribeGatewayInformationResponse' {nextUpdateAvailabilityDate} -> nextUpdateAvailabilityDate) (\s@DescribeGatewayInformationResponse' {} a -> s {nextUpdateAvailabilityDate = a} :: DescribeGatewayInformationResponse)

-- | A value that indicates the time zone configured for the gateway.
describeGatewayInformationResponse_gatewayTimezone :: Lens.Lens' DescribeGatewayInformationResponse (Core.Maybe Core.Text)
describeGatewayInformationResponse_gatewayTimezone = Lens.lens (\DescribeGatewayInformationResponse' {gatewayTimezone} -> gatewayTimezone) (\s@DescribeGatewayInformationResponse' {} a -> s {gatewayTimezone = a} :: DescribeGatewayInformationResponse)

-- | A NetworkInterface array that contains descriptions of the gateway
-- network interfaces.
describeGatewayInformationResponse_gatewayNetworkInterfaces :: Lens.Lens' DescribeGatewayInformationResponse (Core.Maybe [NetworkInterface])
describeGatewayInformationResponse_gatewayNetworkInterfaces = Lens.lens (\DescribeGatewayInformationResponse' {gatewayNetworkInterfaces} -> gatewayNetworkInterfaces) (\s@DescribeGatewayInformationResponse' {} a -> s {gatewayNetworkInterfaces = a} :: DescribeGatewayInformationResponse) Core.. Lens.mapping Lens._Coerce

-- | The type of the gateway.
describeGatewayInformationResponse_gatewayType :: Lens.Lens' DescribeGatewayInformationResponse (Core.Maybe Core.Text)
describeGatewayInformationResponse_gatewayType = Lens.lens (\DescribeGatewayInformationResponse' {gatewayType} -> gatewayType) (\s@DescribeGatewayInformationResponse' {} a -> s {gatewayType = a} :: DescribeGatewayInformationResponse)

-- | The AWS Region where the Amazon EC2 instance is located.
describeGatewayInformationResponse_ec2InstanceRegion :: Lens.Lens' DescribeGatewayInformationResponse (Core.Maybe Core.Text)
describeGatewayInformationResponse_ec2InstanceRegion = Lens.lens (\DescribeGatewayInformationResponse' {ec2InstanceRegion} -> ec2InstanceRegion) (\s@DescribeGatewayInformationResponse' {} a -> s {ec2InstanceRegion = a} :: DescribeGatewayInformationResponse)

-- | The date on which the last software update was applied to the gateway.
-- If the gateway has never been updated, this field does not return a
-- value in the response.
describeGatewayInformationResponse_lastSoftwareUpdate :: Lens.Lens' DescribeGatewayInformationResponse (Core.Maybe Core.Text)
describeGatewayInformationResponse_lastSoftwareUpdate = Lens.lens (\DescribeGatewayInformationResponse' {lastSoftwareUpdate} -> lastSoftwareUpdate) (\s@DescribeGatewayInformationResponse' {} a -> s {lastSoftwareUpdate = a} :: DescribeGatewayInformationResponse)

-- | A list of up to 50 tags assigned to the gateway, sorted alphabetically
-- by key name. Each tag is a key-value pair. For a gateway with more than
-- 10 tags assigned, you can view all tags using the @ListTagsForResource@
-- API operation.
describeGatewayInformationResponse_tags :: Lens.Lens' DescribeGatewayInformationResponse (Core.Maybe [Tag])
describeGatewayInformationResponse_tags = Lens.lens (\DescribeGatewayInformationResponse' {tags} -> tags) (\s@DescribeGatewayInformationResponse' {} a -> s {tags = a} :: DescribeGatewayInformationResponse) Core.. Lens.mapping Lens._Coerce

-- | The configuration settings for the virtual private cloud (VPC) endpoint
-- for your gateway.
describeGatewayInformationResponse_vPCEndpoint :: Lens.Lens' DescribeGatewayInformationResponse (Core.Maybe Core.Text)
describeGatewayInformationResponse_vPCEndpoint = Lens.lens (\DescribeGatewayInformationResponse' {vPCEndpoint} -> vPCEndpoint) (\s@DescribeGatewayInformationResponse' {} a -> s {vPCEndpoint = a} :: DescribeGatewayInformationResponse)

-- | The Amazon Resource Name (ARN) of the Amazon CloudWatch log group that
-- is used to monitor events in the gateway.
describeGatewayInformationResponse_cloudWatchLogGroupARN :: Lens.Lens' DescribeGatewayInformationResponse (Core.Maybe Core.Text)
describeGatewayInformationResponse_cloudWatchLogGroupARN = Lens.lens (\DescribeGatewayInformationResponse' {cloudWatchLogGroupARN} -> cloudWatchLogGroupARN) (\s@DescribeGatewayInformationResponse' {} a -> s {cloudWatchLogGroupARN = a} :: DescribeGatewayInformationResponse)

-- | The ID of the Amazon EC2 instance that was used to launch the gateway.
describeGatewayInformationResponse_ec2InstanceId :: Lens.Lens' DescribeGatewayInformationResponse (Core.Maybe Core.Text)
describeGatewayInformationResponse_ec2InstanceId = Lens.lens (\DescribeGatewayInformationResponse' {ec2InstanceId} -> ec2InstanceId) (\s@DescribeGatewayInformationResponse' {} a -> s {ec2InstanceId = a} :: DescribeGatewayInformationResponse)

-- | The type of hypervisor environment used by the host.
describeGatewayInformationResponse_hostEnvironment :: Lens.Lens' DescribeGatewayInformationResponse (Core.Maybe HostEnvironment)
describeGatewayInformationResponse_hostEnvironment = Lens.lens (\DescribeGatewayInformationResponse' {hostEnvironment} -> hostEnvironment) (\s@DescribeGatewayInformationResponse' {} a -> s {hostEnvironment = a} :: DescribeGatewayInformationResponse)

-- | Undocumented member.
describeGatewayInformationResponse_gatewayARN :: Lens.Lens' DescribeGatewayInformationResponse (Core.Maybe Core.Text)
describeGatewayInformationResponse_gatewayARN = Lens.lens (\DescribeGatewayInformationResponse' {gatewayARN} -> gatewayARN) (\s@DescribeGatewayInformationResponse' {} a -> s {gatewayARN = a} :: DescribeGatewayInformationResponse)

-- | The unique identifier assigned to your gateway during activation. This
-- ID becomes part of the gateway Amazon Resource Name (ARN), which you use
-- as input for other operations.
describeGatewayInformationResponse_gatewayId :: Lens.Lens' DescribeGatewayInformationResponse (Core.Maybe Core.Text)
describeGatewayInformationResponse_gatewayId = Lens.lens (\DescribeGatewayInformationResponse' {gatewayId} -> gatewayId) (\s@DescribeGatewayInformationResponse' {} a -> s {gatewayId = a} :: DescribeGatewayInformationResponse)

-- | The response's http status code.
describeGatewayInformationResponse_httpStatus :: Lens.Lens' DescribeGatewayInformationResponse Core.Int
describeGatewayInformationResponse_httpStatus = Lens.lens (\DescribeGatewayInformationResponse' {httpStatus} -> httpStatus) (\s@DescribeGatewayInformationResponse' {} a -> s {httpStatus = a} :: DescribeGatewayInformationResponse)

instance
  Core.NFData
    DescribeGatewayInformationResponse
