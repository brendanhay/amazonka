{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
  ( -- * Creating a request
    DescribeGatewayInformation (..),
    mkDescribeGatewayInformation,

    -- ** Request lenses
    dgiGatewayARN,

    -- * Destructuring the response
    DescribeGatewayInformationResponse (..),
    mkDescribeGatewayInformationResponse,

    -- ** Response lenses
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | A JSON object containing the ID of the gateway.
--
-- /See:/ 'mkDescribeGatewayInformation' smart constructor.
newtype DescribeGatewayInformation = DescribeGatewayInformation'
  { gatewayARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeGatewayInformation' with the minimum fields required to make a request.
--
-- * 'gatewayARN' -
mkDescribeGatewayInformation ::
  -- | 'gatewayARN'
  Lude.Text ->
  DescribeGatewayInformation
mkDescribeGatewayInformation pGatewayARN_ =
  DescribeGatewayInformation' {gatewayARN = pGatewayARN_}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiGatewayARN :: Lens.Lens' DescribeGatewayInformation Lude.Text
dgiGatewayARN = Lens.lens (gatewayARN :: DescribeGatewayInformation -> Lude.Text) (\s a -> s {gatewayARN = a} :: DescribeGatewayInformation)
{-# DEPRECATED dgiGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

instance Lude.AWSRequest DescribeGatewayInformation where
  type
    Rs DescribeGatewayInformation =
      DescribeGatewayInformationResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeGatewayInformationResponse'
            Lude.<$> (x Lude..?> "GatewayState")
            Lude.<*> (x Lude..?> "Ec2InstanceRegion")
            Lude.<*> (x Lude..?> "GatewayARN")
            Lude.<*> (x Lude..?> "GatewayNetworkInterfaces" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Ec2InstanceId")
            Lude.<*> (x Lude..?> "NextUpdateAvailabilityDate")
            Lude.<*> (x Lude..?> "EndpointType")
            Lude.<*> (x Lude..?> "DeprecationDate")
            Lude.<*> (x Lude..?> "LastSoftwareUpdate")
            Lude.<*> (x Lude..?> "GatewayName")
            Lude.<*> (x Lude..?> "GatewayId")
            Lude.<*> (x Lude..?> "HostEnvironment")
            Lude.<*> (x Lude..?> "GatewayType")
            Lude.<*> (x Lude..?> "GatewayTimezone")
            Lude.<*> (x Lude..?> "SoftwareUpdatesEndDate")
            Lude.<*> (x Lude..?> "CloudWatchLogGroupARN")
            Lude.<*> (x Lude..?> "VPCEndpoint")
            Lude.<*> (x Lude..?> "Tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeGatewayInformation where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StorageGateway_20130630.DescribeGatewayInformation" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeGatewayInformation where
  toJSON DescribeGatewayInformation' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("GatewayARN" Lude..= gatewayARN)])

instance Lude.ToPath DescribeGatewayInformation where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeGatewayInformation where
  toQuery = Lude.const Lude.mempty

-- | A JSON object containing the following fields:
--
-- /See:/ 'mkDescribeGatewayInformationResponse' smart constructor.
data DescribeGatewayInformationResponse = DescribeGatewayInformationResponse'
  { -- | A value that indicates the operating state of the gateway.
    gatewayState :: Lude.Maybe Lude.Text,
    -- | The AWS Region where the Amazon EC2 instance is located.
    ec2InstanceRegion :: Lude.Maybe Lude.Text,
    gatewayARN :: Lude.Maybe Lude.Text,
    -- | A 'NetworkInterface' array that contains descriptions of the gateway network interfaces.
    gatewayNetworkInterfaces :: Lude.Maybe [NetworkInterface],
    -- | The ID of the Amazon EC2 instance that was used to launch the gateway.
    ec2InstanceId :: Lude.Maybe Lude.Text,
    -- | The date on which an update to the gateway is available. This date is in the time zone of the gateway. If the gateway is not available for an update this field is not returned in the response.
    nextUpdateAvailabilityDate :: Lude.Maybe Lude.Text,
    -- | The type of endpoint for your gateway.
    --
    -- Valid Values: @STANDARD@ | @FIPS@
    endpointType :: Lude.Maybe Lude.Text,
    -- | Date after which this gateway will not receive software updates for new features and bug fixes.
    deprecationDate :: Lude.Maybe Lude.Text,
    -- | The date on which the last software update was applied to the gateway. If the gateway has never been updated, this field does not return a value in the response.
    lastSoftwareUpdate :: Lude.Maybe Lude.Text,
    -- | The name you configured for your gateway.
    gatewayName :: Lude.Maybe Lude.Text,
    -- | The unique identifier assigned to your gateway during activation. This ID becomes part of the gateway Amazon Resource Name (ARN), which you use as input for other operations.
    gatewayId :: Lude.Maybe Lude.Text,
    -- | The type of hypervisor environment used by the host.
    hostEnvironment :: Lude.Maybe HostEnvironment,
    -- | The type of the gateway.
    gatewayType :: Lude.Maybe Lude.Text,
    -- | A value that indicates the time zone configured for the gateway.
    gatewayTimezone :: Lude.Maybe Lude.Text,
    -- | Date after which this gateway will not receive software updates for new features.
    softwareUpdatesEndDate :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon CloudWatch log group that is used to monitor events in the gateway.
    cloudWatchLogGroupARN :: Lude.Maybe Lude.Text,
    -- | The configuration settings for the virtual private cloud (VPC) endpoint for your gateway.
    vpcEndpoint :: Lude.Maybe Lude.Text,
    -- | A list of up to 50 tags assigned to the gateway, sorted alphabetically by key name. Each tag is a key-value pair. For a gateway with more than 10 tags assigned, you can view all tags using the @ListTagsForResource@ API operation.
    tags :: Lude.Maybe [Tag],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeGatewayInformationResponse' with the minimum fields required to make a request.
--
-- * 'gatewayState' - A value that indicates the operating state of the gateway.
-- * 'ec2InstanceRegion' - The AWS Region where the Amazon EC2 instance is located.
-- * 'gatewayARN' -
-- * 'gatewayNetworkInterfaces' - A 'NetworkInterface' array that contains descriptions of the gateway network interfaces.
-- * 'ec2InstanceId' - The ID of the Amazon EC2 instance that was used to launch the gateway.
-- * 'nextUpdateAvailabilityDate' - The date on which an update to the gateway is available. This date is in the time zone of the gateway. If the gateway is not available for an update this field is not returned in the response.
-- * 'endpointType' - The type of endpoint for your gateway.
--
-- Valid Values: @STANDARD@ | @FIPS@
-- * 'deprecationDate' - Date after which this gateway will not receive software updates for new features and bug fixes.
-- * 'lastSoftwareUpdate' - The date on which the last software update was applied to the gateway. If the gateway has never been updated, this field does not return a value in the response.
-- * 'gatewayName' - The name you configured for your gateway.
-- * 'gatewayId' - The unique identifier assigned to your gateway during activation. This ID becomes part of the gateway Amazon Resource Name (ARN), which you use as input for other operations.
-- * 'hostEnvironment' - The type of hypervisor environment used by the host.
-- * 'gatewayType' - The type of the gateway.
-- * 'gatewayTimezone' - A value that indicates the time zone configured for the gateway.
-- * 'softwareUpdatesEndDate' - Date after which this gateway will not receive software updates for new features.
-- * 'cloudWatchLogGroupARN' - The Amazon Resource Name (ARN) of the Amazon CloudWatch log group that is used to monitor events in the gateway.
-- * 'vpcEndpoint' - The configuration settings for the virtual private cloud (VPC) endpoint for your gateway.
-- * 'tags' - A list of up to 50 tags assigned to the gateway, sorted alphabetically by key name. Each tag is a key-value pair. For a gateway with more than 10 tags assigned, you can view all tags using the @ListTagsForResource@ API operation.
-- * 'responseStatus' - The response status code.
mkDescribeGatewayInformationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeGatewayInformationResponse
mkDescribeGatewayInformationResponse pResponseStatus_ =
  DescribeGatewayInformationResponse'
    { gatewayState = Lude.Nothing,
      ec2InstanceRegion = Lude.Nothing,
      gatewayARN = Lude.Nothing,
      gatewayNetworkInterfaces = Lude.Nothing,
      ec2InstanceId = Lude.Nothing,
      nextUpdateAvailabilityDate = Lude.Nothing,
      endpointType = Lude.Nothing,
      deprecationDate = Lude.Nothing,
      lastSoftwareUpdate = Lude.Nothing,
      gatewayName = Lude.Nothing,
      gatewayId = Lude.Nothing,
      hostEnvironment = Lude.Nothing,
      gatewayType = Lude.Nothing,
      gatewayTimezone = Lude.Nothing,
      softwareUpdatesEndDate = Lude.Nothing,
      cloudWatchLogGroupARN = Lude.Nothing,
      vpcEndpoint = Lude.Nothing,
      tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A value that indicates the operating state of the gateway.
--
-- /Note:/ Consider using 'gatewayState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgirsGatewayState :: Lens.Lens' DescribeGatewayInformationResponse (Lude.Maybe Lude.Text)
dgirsGatewayState = Lens.lens (gatewayState :: DescribeGatewayInformationResponse -> Lude.Maybe Lude.Text) (\s a -> s {gatewayState = a} :: DescribeGatewayInformationResponse)
{-# DEPRECATED dgirsGatewayState "Use generic-lens or generic-optics with 'gatewayState' instead." #-}

-- | The AWS Region where the Amazon EC2 instance is located.
--
-- /Note:/ Consider using 'ec2InstanceRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgirsEC2InstanceRegion :: Lens.Lens' DescribeGatewayInformationResponse (Lude.Maybe Lude.Text)
dgirsEC2InstanceRegion = Lens.lens (ec2InstanceRegion :: DescribeGatewayInformationResponse -> Lude.Maybe Lude.Text) (\s a -> s {ec2InstanceRegion = a} :: DescribeGatewayInformationResponse)
{-# DEPRECATED dgirsEC2InstanceRegion "Use generic-lens or generic-optics with 'ec2InstanceRegion' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgirsGatewayARN :: Lens.Lens' DescribeGatewayInformationResponse (Lude.Maybe Lude.Text)
dgirsGatewayARN = Lens.lens (gatewayARN :: DescribeGatewayInformationResponse -> Lude.Maybe Lude.Text) (\s a -> s {gatewayARN = a} :: DescribeGatewayInformationResponse)
{-# DEPRECATED dgirsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | A 'NetworkInterface' array that contains descriptions of the gateway network interfaces.
--
-- /Note:/ Consider using 'gatewayNetworkInterfaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgirsGatewayNetworkInterfaces :: Lens.Lens' DescribeGatewayInformationResponse (Lude.Maybe [NetworkInterface])
dgirsGatewayNetworkInterfaces = Lens.lens (gatewayNetworkInterfaces :: DescribeGatewayInformationResponse -> Lude.Maybe [NetworkInterface]) (\s a -> s {gatewayNetworkInterfaces = a} :: DescribeGatewayInformationResponse)
{-# DEPRECATED dgirsGatewayNetworkInterfaces "Use generic-lens or generic-optics with 'gatewayNetworkInterfaces' instead." #-}

-- | The ID of the Amazon EC2 instance that was used to launch the gateway.
--
-- /Note:/ Consider using 'ec2InstanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgirsEC2InstanceId :: Lens.Lens' DescribeGatewayInformationResponse (Lude.Maybe Lude.Text)
dgirsEC2InstanceId = Lens.lens (ec2InstanceId :: DescribeGatewayInformationResponse -> Lude.Maybe Lude.Text) (\s a -> s {ec2InstanceId = a} :: DescribeGatewayInformationResponse)
{-# DEPRECATED dgirsEC2InstanceId "Use generic-lens or generic-optics with 'ec2InstanceId' instead." #-}

-- | The date on which an update to the gateway is available. This date is in the time zone of the gateway. If the gateway is not available for an update this field is not returned in the response.
--
-- /Note:/ Consider using 'nextUpdateAvailabilityDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgirsNextUpdateAvailabilityDate :: Lens.Lens' DescribeGatewayInformationResponse (Lude.Maybe Lude.Text)
dgirsNextUpdateAvailabilityDate = Lens.lens (nextUpdateAvailabilityDate :: DescribeGatewayInformationResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextUpdateAvailabilityDate = a} :: DescribeGatewayInformationResponse)
{-# DEPRECATED dgirsNextUpdateAvailabilityDate "Use generic-lens or generic-optics with 'nextUpdateAvailabilityDate' instead." #-}

-- | The type of endpoint for your gateway.
--
-- Valid Values: @STANDARD@ | @FIPS@
--
-- /Note:/ Consider using 'endpointType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgirsEndpointType :: Lens.Lens' DescribeGatewayInformationResponse (Lude.Maybe Lude.Text)
dgirsEndpointType = Lens.lens (endpointType :: DescribeGatewayInformationResponse -> Lude.Maybe Lude.Text) (\s a -> s {endpointType = a} :: DescribeGatewayInformationResponse)
{-# DEPRECATED dgirsEndpointType "Use generic-lens or generic-optics with 'endpointType' instead." #-}

-- | Date after which this gateway will not receive software updates for new features and bug fixes.
--
-- /Note:/ Consider using 'deprecationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgirsDeprecationDate :: Lens.Lens' DescribeGatewayInformationResponse (Lude.Maybe Lude.Text)
dgirsDeprecationDate = Lens.lens (deprecationDate :: DescribeGatewayInformationResponse -> Lude.Maybe Lude.Text) (\s a -> s {deprecationDate = a} :: DescribeGatewayInformationResponse)
{-# DEPRECATED dgirsDeprecationDate "Use generic-lens or generic-optics with 'deprecationDate' instead." #-}

-- | The date on which the last software update was applied to the gateway. If the gateway has never been updated, this field does not return a value in the response.
--
-- /Note:/ Consider using 'lastSoftwareUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgirsLastSoftwareUpdate :: Lens.Lens' DescribeGatewayInformationResponse (Lude.Maybe Lude.Text)
dgirsLastSoftwareUpdate = Lens.lens (lastSoftwareUpdate :: DescribeGatewayInformationResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastSoftwareUpdate = a} :: DescribeGatewayInformationResponse)
{-# DEPRECATED dgirsLastSoftwareUpdate "Use generic-lens or generic-optics with 'lastSoftwareUpdate' instead." #-}

-- | The name you configured for your gateway.
--
-- /Note:/ Consider using 'gatewayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgirsGatewayName :: Lens.Lens' DescribeGatewayInformationResponse (Lude.Maybe Lude.Text)
dgirsGatewayName = Lens.lens (gatewayName :: DescribeGatewayInformationResponse -> Lude.Maybe Lude.Text) (\s a -> s {gatewayName = a} :: DescribeGatewayInformationResponse)
{-# DEPRECATED dgirsGatewayName "Use generic-lens or generic-optics with 'gatewayName' instead." #-}

-- | The unique identifier assigned to your gateway during activation. This ID becomes part of the gateway Amazon Resource Name (ARN), which you use as input for other operations.
--
-- /Note:/ Consider using 'gatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgirsGatewayId :: Lens.Lens' DescribeGatewayInformationResponse (Lude.Maybe Lude.Text)
dgirsGatewayId = Lens.lens (gatewayId :: DescribeGatewayInformationResponse -> Lude.Maybe Lude.Text) (\s a -> s {gatewayId = a} :: DescribeGatewayInformationResponse)
{-# DEPRECATED dgirsGatewayId "Use generic-lens or generic-optics with 'gatewayId' instead." #-}

-- | The type of hypervisor environment used by the host.
--
-- /Note:/ Consider using 'hostEnvironment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgirsHostEnvironment :: Lens.Lens' DescribeGatewayInformationResponse (Lude.Maybe HostEnvironment)
dgirsHostEnvironment = Lens.lens (hostEnvironment :: DescribeGatewayInformationResponse -> Lude.Maybe HostEnvironment) (\s a -> s {hostEnvironment = a} :: DescribeGatewayInformationResponse)
{-# DEPRECATED dgirsHostEnvironment "Use generic-lens or generic-optics with 'hostEnvironment' instead." #-}

-- | The type of the gateway.
--
-- /Note:/ Consider using 'gatewayType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgirsGatewayType :: Lens.Lens' DescribeGatewayInformationResponse (Lude.Maybe Lude.Text)
dgirsGatewayType = Lens.lens (gatewayType :: DescribeGatewayInformationResponse -> Lude.Maybe Lude.Text) (\s a -> s {gatewayType = a} :: DescribeGatewayInformationResponse)
{-# DEPRECATED dgirsGatewayType "Use generic-lens or generic-optics with 'gatewayType' instead." #-}

-- | A value that indicates the time zone configured for the gateway.
--
-- /Note:/ Consider using 'gatewayTimezone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgirsGatewayTimezone :: Lens.Lens' DescribeGatewayInformationResponse (Lude.Maybe Lude.Text)
dgirsGatewayTimezone = Lens.lens (gatewayTimezone :: DescribeGatewayInformationResponse -> Lude.Maybe Lude.Text) (\s a -> s {gatewayTimezone = a} :: DescribeGatewayInformationResponse)
{-# DEPRECATED dgirsGatewayTimezone "Use generic-lens or generic-optics with 'gatewayTimezone' instead." #-}

-- | Date after which this gateway will not receive software updates for new features.
--
-- /Note:/ Consider using 'softwareUpdatesEndDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgirsSoftwareUpdatesEndDate :: Lens.Lens' DescribeGatewayInformationResponse (Lude.Maybe Lude.Text)
dgirsSoftwareUpdatesEndDate = Lens.lens (softwareUpdatesEndDate :: DescribeGatewayInformationResponse -> Lude.Maybe Lude.Text) (\s a -> s {softwareUpdatesEndDate = a} :: DescribeGatewayInformationResponse)
{-# DEPRECATED dgirsSoftwareUpdatesEndDate "Use generic-lens or generic-optics with 'softwareUpdatesEndDate' instead." #-}

-- | The Amazon Resource Name (ARN) of the Amazon CloudWatch log group that is used to monitor events in the gateway.
--
-- /Note:/ Consider using 'cloudWatchLogGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgirsCloudWatchLogGroupARN :: Lens.Lens' DescribeGatewayInformationResponse (Lude.Maybe Lude.Text)
dgirsCloudWatchLogGroupARN = Lens.lens (cloudWatchLogGroupARN :: DescribeGatewayInformationResponse -> Lude.Maybe Lude.Text) (\s a -> s {cloudWatchLogGroupARN = a} :: DescribeGatewayInformationResponse)
{-# DEPRECATED dgirsCloudWatchLogGroupARN "Use generic-lens or generic-optics with 'cloudWatchLogGroupARN' instead." #-}

-- | The configuration settings for the virtual private cloud (VPC) endpoint for your gateway.
--
-- /Note:/ Consider using 'vpcEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgirsVPCEndpoint :: Lens.Lens' DescribeGatewayInformationResponse (Lude.Maybe Lude.Text)
dgirsVPCEndpoint = Lens.lens (vpcEndpoint :: DescribeGatewayInformationResponse -> Lude.Maybe Lude.Text) (\s a -> s {vpcEndpoint = a} :: DescribeGatewayInformationResponse)
{-# DEPRECATED dgirsVPCEndpoint "Use generic-lens or generic-optics with 'vpcEndpoint' instead." #-}

-- | A list of up to 50 tags assigned to the gateway, sorted alphabetically by key name. Each tag is a key-value pair. For a gateway with more than 10 tags assigned, you can view all tags using the @ListTagsForResource@ API operation.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgirsTags :: Lens.Lens' DescribeGatewayInformationResponse (Lude.Maybe [Tag])
dgirsTags = Lens.lens (tags :: DescribeGatewayInformationResponse -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: DescribeGatewayInformationResponse)
{-# DEPRECATED dgirsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgirsResponseStatus :: Lens.Lens' DescribeGatewayInformationResponse Lude.Int
dgirsResponseStatus = Lens.lens (responseStatus :: DescribeGatewayInformationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeGatewayInformationResponse)
{-# DEPRECATED dgirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
