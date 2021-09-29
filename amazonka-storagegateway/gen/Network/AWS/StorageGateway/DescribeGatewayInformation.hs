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
    describeGatewayInformationResponse_gatewayName,
    describeGatewayInformationResponse_gatewayState,
    describeGatewayInformationResponse_deprecationDate,
    describeGatewayInformationResponse_gatewayCapacity,
    describeGatewayInformationResponse_softwareUpdatesEndDate,
    describeGatewayInformationResponse_nextUpdateAvailabilityDate,
    describeGatewayInformationResponse_gatewayTimezone,
    describeGatewayInformationResponse_endpointType,
    describeGatewayInformationResponse_gatewayType,
    describeGatewayInformationResponse_gatewayNetworkInterfaces,
    describeGatewayInformationResponse_ec2InstanceRegion,
    describeGatewayInformationResponse_lastSoftwareUpdate,
    describeGatewayInformationResponse_supportedGatewayCapacities,
    describeGatewayInformationResponse_tags,
    describeGatewayInformationResponse_vPCEndpoint,
    describeGatewayInformationResponse_cloudWatchLogGroupARN,
    describeGatewayInformationResponse_ec2InstanceId,
    describeGatewayInformationResponse_gatewayARN,
    describeGatewayInformationResponse_gatewayId,
    describeGatewayInformationResponse_hostEnvironment,
    describeGatewayInformationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | A JSON object containing the ID of the gateway.
--
-- /See:/ 'newDescribeGatewayInformation' smart constructor.
data DescribeGatewayInformation = DescribeGatewayInformation'
  { gatewayARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DescribeGatewayInformation
newDescribeGatewayInformation pGatewayARN_ =
  DescribeGatewayInformation'
    { gatewayARN =
        pGatewayARN_
    }

-- | Undocumented member.
describeGatewayInformation_gatewayARN :: Lens.Lens' DescribeGatewayInformation Prelude.Text
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
            Prelude.<$> (x Core..?> "GatewayName")
            Prelude.<*> (x Core..?> "GatewayState")
            Prelude.<*> (x Core..?> "DeprecationDate")
            Prelude.<*> (x Core..?> "GatewayCapacity")
            Prelude.<*> (x Core..?> "SoftwareUpdatesEndDate")
            Prelude.<*> (x Core..?> "NextUpdateAvailabilityDate")
            Prelude.<*> (x Core..?> "GatewayTimezone")
            Prelude.<*> (x Core..?> "EndpointType")
            Prelude.<*> (x Core..?> "GatewayType")
            Prelude.<*> ( x Core..?> "GatewayNetworkInterfaces"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "Ec2InstanceRegion")
            Prelude.<*> (x Core..?> "LastSoftwareUpdate")
            Prelude.<*> ( x Core..?> "SupportedGatewayCapacities"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "VPCEndpoint")
            Prelude.<*> (x Core..?> "CloudWatchLogGroupARN")
            Prelude.<*> (x Core..?> "Ec2InstanceId")
            Prelude.<*> (x Core..?> "GatewayARN")
            Prelude.<*> (x Core..?> "GatewayId")
            Prelude.<*> (x Core..?> "HostEnvironment")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeGatewayInformation

instance Prelude.NFData DescribeGatewayInformation

instance Core.ToHeaders DescribeGatewayInformation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.DescribeGatewayInformation" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeGatewayInformation where
  toJSON DescribeGatewayInformation' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("GatewayARN" Core..= gatewayARN)]
      )

instance Core.ToPath DescribeGatewayInformation where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeGatewayInformation where
  toQuery = Prelude.const Prelude.mempty

-- | A JSON object containing the following fields:
--
-- /See:/ 'newDescribeGatewayInformationResponse' smart constructor.
data DescribeGatewayInformationResponse = DescribeGatewayInformationResponse'
  { -- | The name you configured for your gateway.
    gatewayName :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates the operating state of the gateway.
    gatewayState :: Prelude.Maybe Prelude.Text,
    -- | Date after which this gateway will not receive software updates for new
    -- features and bug fixes.
    deprecationDate :: Prelude.Maybe Prelude.Text,
    -- | Specifies the size of the gateway\'s metadata cache.
    gatewayCapacity :: Prelude.Maybe GatewayCapacity,
    -- | Date after which this gateway will not receive software updates for new
    -- features.
    softwareUpdatesEndDate :: Prelude.Maybe Prelude.Text,
    -- | The date on which an update to the gateway is available. This date is in
    -- the time zone of the gateway. If the gateway is not available for an
    -- update this field is not returned in the response.
    nextUpdateAvailabilityDate :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates the time zone configured for the gateway.
    gatewayTimezone :: Prelude.Maybe Prelude.Text,
    -- | The type of endpoint for your gateway.
    --
    -- Valid Values: @STANDARD@ | @FIPS@
    endpointType :: Prelude.Maybe Prelude.Text,
    -- | The type of the gateway.
    gatewayType :: Prelude.Maybe Prelude.Text,
    -- | A NetworkInterface array that contains descriptions of the gateway
    -- network interfaces.
    gatewayNetworkInterfaces :: Prelude.Maybe [NetworkInterface],
    -- | The Region where the Amazon EC2 instance is located.
    ec2InstanceRegion :: Prelude.Maybe Prelude.Text,
    -- | The date on which the last software update was applied to the gateway.
    -- If the gateway has never been updated, this field does not return a
    -- value in the response.
    lastSoftwareUpdate :: Prelude.Maybe Prelude.Text,
    -- | A list of the metadata cache sizes that the gateway can support based on
    -- its current hardware specifications.
    supportedGatewayCapacities :: Prelude.Maybe [GatewayCapacity],
    -- | A list of up to 50 tags assigned to the gateway, sorted alphabetically
    -- by key name. Each tag is a key-value pair. For a gateway with more than
    -- 10 tags assigned, you can view all tags using the @ListTagsForResource@
    -- API operation.
    tags :: Prelude.Maybe [Tag],
    -- | The configuration settings for the virtual private cloud (VPC) endpoint
    -- for your gateway.
    vPCEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon CloudWatch log group that
    -- is used to monitor events in the gateway.
    cloudWatchLogGroupARN :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon EC2 instance that was used to launch the gateway.
    ec2InstanceId :: Prelude.Maybe Prelude.Text,
    gatewayARN :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier assigned to your gateway during activation. This
    -- ID becomes part of the gateway Amazon Resource Name (ARN), which you use
    -- as input for other operations.
    gatewayId :: Prelude.Maybe Prelude.Text,
    -- | The type of hypervisor environment used by the host.
    hostEnvironment :: Prelude.Maybe HostEnvironment,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeGatewayInformationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayName', 'describeGatewayInformationResponse_gatewayName' - The name you configured for your gateway.
--
-- 'gatewayState', 'describeGatewayInformationResponse_gatewayState' - A value that indicates the operating state of the gateway.
--
-- 'deprecationDate', 'describeGatewayInformationResponse_deprecationDate' - Date after which this gateway will not receive software updates for new
-- features and bug fixes.
--
-- 'gatewayCapacity', 'describeGatewayInformationResponse_gatewayCapacity' - Specifies the size of the gateway\'s metadata cache.
--
-- 'softwareUpdatesEndDate', 'describeGatewayInformationResponse_softwareUpdatesEndDate' - Date after which this gateway will not receive software updates for new
-- features.
--
-- 'nextUpdateAvailabilityDate', 'describeGatewayInformationResponse_nextUpdateAvailabilityDate' - The date on which an update to the gateway is available. This date is in
-- the time zone of the gateway. If the gateway is not available for an
-- update this field is not returned in the response.
--
-- 'gatewayTimezone', 'describeGatewayInformationResponse_gatewayTimezone' - A value that indicates the time zone configured for the gateway.
--
-- 'endpointType', 'describeGatewayInformationResponse_endpointType' - The type of endpoint for your gateway.
--
-- Valid Values: @STANDARD@ | @FIPS@
--
-- 'gatewayType', 'describeGatewayInformationResponse_gatewayType' - The type of the gateway.
--
-- 'gatewayNetworkInterfaces', 'describeGatewayInformationResponse_gatewayNetworkInterfaces' - A NetworkInterface array that contains descriptions of the gateway
-- network interfaces.
--
-- 'ec2InstanceRegion', 'describeGatewayInformationResponse_ec2InstanceRegion' - The Region where the Amazon EC2 instance is located.
--
-- 'lastSoftwareUpdate', 'describeGatewayInformationResponse_lastSoftwareUpdate' - The date on which the last software update was applied to the gateway.
-- If the gateway has never been updated, this field does not return a
-- value in the response.
--
-- 'supportedGatewayCapacities', 'describeGatewayInformationResponse_supportedGatewayCapacities' - A list of the metadata cache sizes that the gateway can support based on
-- its current hardware specifications.
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
-- 'gatewayARN', 'describeGatewayInformationResponse_gatewayARN' - Undocumented member.
--
-- 'gatewayId', 'describeGatewayInformationResponse_gatewayId' - The unique identifier assigned to your gateway during activation. This
-- ID becomes part of the gateway Amazon Resource Name (ARN), which you use
-- as input for other operations.
--
-- 'hostEnvironment', 'describeGatewayInformationResponse_hostEnvironment' - The type of hypervisor environment used by the host.
--
-- 'httpStatus', 'describeGatewayInformationResponse_httpStatus' - The response's http status code.
newDescribeGatewayInformationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeGatewayInformationResponse
newDescribeGatewayInformationResponse pHttpStatus_ =
  DescribeGatewayInformationResponse'
    { gatewayName =
        Prelude.Nothing,
      gatewayState = Prelude.Nothing,
      deprecationDate = Prelude.Nothing,
      gatewayCapacity = Prelude.Nothing,
      softwareUpdatesEndDate =
        Prelude.Nothing,
      nextUpdateAvailabilityDate =
        Prelude.Nothing,
      gatewayTimezone = Prelude.Nothing,
      endpointType = Prelude.Nothing,
      gatewayType = Prelude.Nothing,
      gatewayNetworkInterfaces =
        Prelude.Nothing,
      ec2InstanceRegion = Prelude.Nothing,
      lastSoftwareUpdate = Prelude.Nothing,
      supportedGatewayCapacities =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      vPCEndpoint = Prelude.Nothing,
      cloudWatchLogGroupARN = Prelude.Nothing,
      ec2InstanceId = Prelude.Nothing,
      gatewayARN = Prelude.Nothing,
      gatewayId = Prelude.Nothing,
      hostEnvironment = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name you configured for your gateway.
describeGatewayInformationResponse_gatewayName :: Lens.Lens' DescribeGatewayInformationResponse (Prelude.Maybe Prelude.Text)
describeGatewayInformationResponse_gatewayName = Lens.lens (\DescribeGatewayInformationResponse' {gatewayName} -> gatewayName) (\s@DescribeGatewayInformationResponse' {} a -> s {gatewayName = a} :: DescribeGatewayInformationResponse)

-- | A value that indicates the operating state of the gateway.
describeGatewayInformationResponse_gatewayState :: Lens.Lens' DescribeGatewayInformationResponse (Prelude.Maybe Prelude.Text)
describeGatewayInformationResponse_gatewayState = Lens.lens (\DescribeGatewayInformationResponse' {gatewayState} -> gatewayState) (\s@DescribeGatewayInformationResponse' {} a -> s {gatewayState = a} :: DescribeGatewayInformationResponse)

-- | Date after which this gateway will not receive software updates for new
-- features and bug fixes.
describeGatewayInformationResponse_deprecationDate :: Lens.Lens' DescribeGatewayInformationResponse (Prelude.Maybe Prelude.Text)
describeGatewayInformationResponse_deprecationDate = Lens.lens (\DescribeGatewayInformationResponse' {deprecationDate} -> deprecationDate) (\s@DescribeGatewayInformationResponse' {} a -> s {deprecationDate = a} :: DescribeGatewayInformationResponse)

-- | Specifies the size of the gateway\'s metadata cache.
describeGatewayInformationResponse_gatewayCapacity :: Lens.Lens' DescribeGatewayInformationResponse (Prelude.Maybe GatewayCapacity)
describeGatewayInformationResponse_gatewayCapacity = Lens.lens (\DescribeGatewayInformationResponse' {gatewayCapacity} -> gatewayCapacity) (\s@DescribeGatewayInformationResponse' {} a -> s {gatewayCapacity = a} :: DescribeGatewayInformationResponse)

-- | Date after which this gateway will not receive software updates for new
-- features.
describeGatewayInformationResponse_softwareUpdatesEndDate :: Lens.Lens' DescribeGatewayInformationResponse (Prelude.Maybe Prelude.Text)
describeGatewayInformationResponse_softwareUpdatesEndDate = Lens.lens (\DescribeGatewayInformationResponse' {softwareUpdatesEndDate} -> softwareUpdatesEndDate) (\s@DescribeGatewayInformationResponse' {} a -> s {softwareUpdatesEndDate = a} :: DescribeGatewayInformationResponse)

-- | The date on which an update to the gateway is available. This date is in
-- the time zone of the gateway. If the gateway is not available for an
-- update this field is not returned in the response.
describeGatewayInformationResponse_nextUpdateAvailabilityDate :: Lens.Lens' DescribeGatewayInformationResponse (Prelude.Maybe Prelude.Text)
describeGatewayInformationResponse_nextUpdateAvailabilityDate = Lens.lens (\DescribeGatewayInformationResponse' {nextUpdateAvailabilityDate} -> nextUpdateAvailabilityDate) (\s@DescribeGatewayInformationResponse' {} a -> s {nextUpdateAvailabilityDate = a} :: DescribeGatewayInformationResponse)

-- | A value that indicates the time zone configured for the gateway.
describeGatewayInformationResponse_gatewayTimezone :: Lens.Lens' DescribeGatewayInformationResponse (Prelude.Maybe Prelude.Text)
describeGatewayInformationResponse_gatewayTimezone = Lens.lens (\DescribeGatewayInformationResponse' {gatewayTimezone} -> gatewayTimezone) (\s@DescribeGatewayInformationResponse' {} a -> s {gatewayTimezone = a} :: DescribeGatewayInformationResponse)

-- | The type of endpoint for your gateway.
--
-- Valid Values: @STANDARD@ | @FIPS@
describeGatewayInformationResponse_endpointType :: Lens.Lens' DescribeGatewayInformationResponse (Prelude.Maybe Prelude.Text)
describeGatewayInformationResponse_endpointType = Lens.lens (\DescribeGatewayInformationResponse' {endpointType} -> endpointType) (\s@DescribeGatewayInformationResponse' {} a -> s {endpointType = a} :: DescribeGatewayInformationResponse)

-- | The type of the gateway.
describeGatewayInformationResponse_gatewayType :: Lens.Lens' DescribeGatewayInformationResponse (Prelude.Maybe Prelude.Text)
describeGatewayInformationResponse_gatewayType = Lens.lens (\DescribeGatewayInformationResponse' {gatewayType} -> gatewayType) (\s@DescribeGatewayInformationResponse' {} a -> s {gatewayType = a} :: DescribeGatewayInformationResponse)

-- | A NetworkInterface array that contains descriptions of the gateway
-- network interfaces.
describeGatewayInformationResponse_gatewayNetworkInterfaces :: Lens.Lens' DescribeGatewayInformationResponse (Prelude.Maybe [NetworkInterface])
describeGatewayInformationResponse_gatewayNetworkInterfaces = Lens.lens (\DescribeGatewayInformationResponse' {gatewayNetworkInterfaces} -> gatewayNetworkInterfaces) (\s@DescribeGatewayInformationResponse' {} a -> s {gatewayNetworkInterfaces = a} :: DescribeGatewayInformationResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The Region where the Amazon EC2 instance is located.
describeGatewayInformationResponse_ec2InstanceRegion :: Lens.Lens' DescribeGatewayInformationResponse (Prelude.Maybe Prelude.Text)
describeGatewayInformationResponse_ec2InstanceRegion = Lens.lens (\DescribeGatewayInformationResponse' {ec2InstanceRegion} -> ec2InstanceRegion) (\s@DescribeGatewayInformationResponse' {} a -> s {ec2InstanceRegion = a} :: DescribeGatewayInformationResponse)

-- | The date on which the last software update was applied to the gateway.
-- If the gateway has never been updated, this field does not return a
-- value in the response.
describeGatewayInformationResponse_lastSoftwareUpdate :: Lens.Lens' DescribeGatewayInformationResponse (Prelude.Maybe Prelude.Text)
describeGatewayInformationResponse_lastSoftwareUpdate = Lens.lens (\DescribeGatewayInformationResponse' {lastSoftwareUpdate} -> lastSoftwareUpdate) (\s@DescribeGatewayInformationResponse' {} a -> s {lastSoftwareUpdate = a} :: DescribeGatewayInformationResponse)

-- | A list of the metadata cache sizes that the gateway can support based on
-- its current hardware specifications.
describeGatewayInformationResponse_supportedGatewayCapacities :: Lens.Lens' DescribeGatewayInformationResponse (Prelude.Maybe [GatewayCapacity])
describeGatewayInformationResponse_supportedGatewayCapacities = Lens.lens (\DescribeGatewayInformationResponse' {supportedGatewayCapacities} -> supportedGatewayCapacities) (\s@DescribeGatewayInformationResponse' {} a -> s {supportedGatewayCapacities = a} :: DescribeGatewayInformationResponse) Prelude.. Lens.mapping Lens._Coerce

-- | A list of up to 50 tags assigned to the gateway, sorted alphabetically
-- by key name. Each tag is a key-value pair. For a gateway with more than
-- 10 tags assigned, you can view all tags using the @ListTagsForResource@
-- API operation.
describeGatewayInformationResponse_tags :: Lens.Lens' DescribeGatewayInformationResponse (Prelude.Maybe [Tag])
describeGatewayInformationResponse_tags = Lens.lens (\DescribeGatewayInformationResponse' {tags} -> tags) (\s@DescribeGatewayInformationResponse' {} a -> s {tags = a} :: DescribeGatewayInformationResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The configuration settings for the virtual private cloud (VPC) endpoint
-- for your gateway.
describeGatewayInformationResponse_vPCEndpoint :: Lens.Lens' DescribeGatewayInformationResponse (Prelude.Maybe Prelude.Text)
describeGatewayInformationResponse_vPCEndpoint = Lens.lens (\DescribeGatewayInformationResponse' {vPCEndpoint} -> vPCEndpoint) (\s@DescribeGatewayInformationResponse' {} a -> s {vPCEndpoint = a} :: DescribeGatewayInformationResponse)

-- | The Amazon Resource Name (ARN) of the Amazon CloudWatch log group that
-- is used to monitor events in the gateway.
describeGatewayInformationResponse_cloudWatchLogGroupARN :: Lens.Lens' DescribeGatewayInformationResponse (Prelude.Maybe Prelude.Text)
describeGatewayInformationResponse_cloudWatchLogGroupARN = Lens.lens (\DescribeGatewayInformationResponse' {cloudWatchLogGroupARN} -> cloudWatchLogGroupARN) (\s@DescribeGatewayInformationResponse' {} a -> s {cloudWatchLogGroupARN = a} :: DescribeGatewayInformationResponse)

-- | The ID of the Amazon EC2 instance that was used to launch the gateway.
describeGatewayInformationResponse_ec2InstanceId :: Lens.Lens' DescribeGatewayInformationResponse (Prelude.Maybe Prelude.Text)
describeGatewayInformationResponse_ec2InstanceId = Lens.lens (\DescribeGatewayInformationResponse' {ec2InstanceId} -> ec2InstanceId) (\s@DescribeGatewayInformationResponse' {} a -> s {ec2InstanceId = a} :: DescribeGatewayInformationResponse)

-- | Undocumented member.
describeGatewayInformationResponse_gatewayARN :: Lens.Lens' DescribeGatewayInformationResponse (Prelude.Maybe Prelude.Text)
describeGatewayInformationResponse_gatewayARN = Lens.lens (\DescribeGatewayInformationResponse' {gatewayARN} -> gatewayARN) (\s@DescribeGatewayInformationResponse' {} a -> s {gatewayARN = a} :: DescribeGatewayInformationResponse)

-- | The unique identifier assigned to your gateway during activation. This
-- ID becomes part of the gateway Amazon Resource Name (ARN), which you use
-- as input for other operations.
describeGatewayInformationResponse_gatewayId :: Lens.Lens' DescribeGatewayInformationResponse (Prelude.Maybe Prelude.Text)
describeGatewayInformationResponse_gatewayId = Lens.lens (\DescribeGatewayInformationResponse' {gatewayId} -> gatewayId) (\s@DescribeGatewayInformationResponse' {} a -> s {gatewayId = a} :: DescribeGatewayInformationResponse)

-- | The type of hypervisor environment used by the host.
describeGatewayInformationResponse_hostEnvironment :: Lens.Lens' DescribeGatewayInformationResponse (Prelude.Maybe HostEnvironment)
describeGatewayInformationResponse_hostEnvironment = Lens.lens (\DescribeGatewayInformationResponse' {hostEnvironment} -> hostEnvironment) (\s@DescribeGatewayInformationResponse' {} a -> s {hostEnvironment = a} :: DescribeGatewayInformationResponse)

-- | The response's http status code.
describeGatewayInformationResponse_httpStatus :: Lens.Lens' DescribeGatewayInformationResponse Prelude.Int
describeGatewayInformationResponse_httpStatus = Lens.lens (\DescribeGatewayInformationResponse' {httpStatus} -> httpStatus) (\s@DescribeGatewayInformationResponse' {} a -> s {httpStatus = a} :: DescribeGatewayInformationResponse)

instance
  Prelude.NFData
    DescribeGatewayInformationResponse
