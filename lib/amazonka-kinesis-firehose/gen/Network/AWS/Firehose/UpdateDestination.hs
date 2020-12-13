{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.UpdateDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified destination of the specified delivery stream.
--
-- Use this operation to change the destination type (for example, to replace the Amazon S3 destination with Amazon Redshift) or change the parameters associated with a destination (for example, to change the bucket name of the Amazon S3 destination). The update might not occur immediately. The target delivery stream remains active while the configurations are updated, so data writes to the delivery stream can continue during this process. The updated configurations are usually effective within a few minutes.
-- Switching between Amazon ES and other services is not supported. For an Amazon ES destination, you can only update to another Amazon ES destination.
-- If the destination type is the same, Kinesis Data Firehose merges the configuration parameters specified with the destination configuration that already exists on the delivery stream. If any of the parameters are not specified in the call, the existing values are retained. For example, in the Amazon S3 destination, if 'EncryptionConfiguration' is not specified, then the existing @EncryptionConfiguration@ is maintained on the destination.
-- If the destination type is not the same, for example, changing the destination from Amazon S3 to Amazon Redshift, Kinesis Data Firehose does not merge any parameters. In this case, all parameters must be specified.
-- Kinesis Data Firehose uses @CurrentDeliveryStreamVersionId@ to avoid race conditions and conflicting merges. This is a required field, and the service updates the configuration only if the existing configuration has a version ID that matches. After the update is applied successfully, the version ID is updated, and can be retrieved using 'DescribeDeliveryStream' . Use the new version ID to set @CurrentDeliveryStreamVersionId@ in the next call.
module Network.AWS.Firehose.UpdateDestination
  ( -- * Creating a request
    UpdateDestination (..),
    mkUpdateDestination,

    -- ** Request lenses
    udSplunkDestinationUpdate,
    udS3DestinationUpdate,
    udRedshiftDestinationUpdate,
    udElasticsearchDestinationUpdate,
    udCurrentDeliveryStreamVersionId,
    udDeliveryStreamName,
    udExtendedS3DestinationUpdate,
    udHTTPEndpointDestinationUpdate,
    udDestinationId,

    -- * Destructuring the response
    UpdateDestinationResponse (..),
    mkUpdateDestinationResponse,

    -- ** Response lenses
    udrsResponseStatus,
  )
where

import Network.AWS.Firehose.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateDestination' smart constructor.
data UpdateDestination = UpdateDestination'
  { -- | Describes an update for a destination in Splunk.
    splunkDestinationUpdate :: Lude.Maybe SplunkDestinationUpdate,
    -- | [Deprecated] Describes an update for a destination in Amazon S3.
    s3DestinationUpdate :: Lude.Maybe S3DestinationUpdate,
    -- | Describes an update for a destination in Amazon Redshift.
    redshiftDestinationUpdate :: Lude.Maybe RedshiftDestinationUpdate,
    -- | Describes an update for a destination in Amazon ES.
    elasticsearchDestinationUpdate :: Lude.Maybe ElasticsearchDestinationUpdate,
    -- | Obtain this value from the @VersionId@ result of 'DeliveryStreamDescription' . This value is required, and helps the service perform conditional operations. For example, if there is an interleaving update and this value is null, then the update destination fails. After the update is successful, the @VersionId@ value is updated. The service then performs a merge of the old configuration with the new configuration.
    currentDeliveryStreamVersionId :: Lude.Text,
    -- | The name of the delivery stream.
    deliveryStreamName :: Lude.Text,
    -- | Describes an update for a destination in Amazon S3.
    extendedS3DestinationUpdate :: Lude.Maybe ExtendedS3DestinationUpdate,
    -- | Describes an update to the specified HTTP endpoint destination.
    hTTPEndpointDestinationUpdate :: Lude.Maybe HTTPEndpointDestinationUpdate,
    -- | The ID of the destination.
    destinationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDestination' with the minimum fields required to make a request.
--
-- * 'splunkDestinationUpdate' - Describes an update for a destination in Splunk.
-- * 's3DestinationUpdate' - [Deprecated] Describes an update for a destination in Amazon S3.
-- * 'redshiftDestinationUpdate' - Describes an update for a destination in Amazon Redshift.
-- * 'elasticsearchDestinationUpdate' - Describes an update for a destination in Amazon ES.
-- * 'currentDeliveryStreamVersionId' - Obtain this value from the @VersionId@ result of 'DeliveryStreamDescription' . This value is required, and helps the service perform conditional operations. For example, if there is an interleaving update and this value is null, then the update destination fails. After the update is successful, the @VersionId@ value is updated. The service then performs a merge of the old configuration with the new configuration.
-- * 'deliveryStreamName' - The name of the delivery stream.
-- * 'extendedS3DestinationUpdate' - Describes an update for a destination in Amazon S3.
-- * 'hTTPEndpointDestinationUpdate' - Describes an update to the specified HTTP endpoint destination.
-- * 'destinationId' - The ID of the destination.
mkUpdateDestination ::
  -- | 'currentDeliveryStreamVersionId'
  Lude.Text ->
  -- | 'deliveryStreamName'
  Lude.Text ->
  -- | 'destinationId'
  Lude.Text ->
  UpdateDestination
mkUpdateDestination
  pCurrentDeliveryStreamVersionId_
  pDeliveryStreamName_
  pDestinationId_ =
    UpdateDestination'
      { splunkDestinationUpdate = Lude.Nothing,
        s3DestinationUpdate = Lude.Nothing,
        redshiftDestinationUpdate = Lude.Nothing,
        elasticsearchDestinationUpdate = Lude.Nothing,
        currentDeliveryStreamVersionId = pCurrentDeliveryStreamVersionId_,
        deliveryStreamName = pDeliveryStreamName_,
        extendedS3DestinationUpdate = Lude.Nothing,
        hTTPEndpointDestinationUpdate = Lude.Nothing,
        destinationId = pDestinationId_
      }

-- | Describes an update for a destination in Splunk.
--
-- /Note:/ Consider using 'splunkDestinationUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udSplunkDestinationUpdate :: Lens.Lens' UpdateDestination (Lude.Maybe SplunkDestinationUpdate)
udSplunkDestinationUpdate = Lens.lens (splunkDestinationUpdate :: UpdateDestination -> Lude.Maybe SplunkDestinationUpdate) (\s a -> s {splunkDestinationUpdate = a} :: UpdateDestination)
{-# DEPRECATED udSplunkDestinationUpdate "Use generic-lens or generic-optics with 'splunkDestinationUpdate' instead." #-}

-- | [Deprecated] Describes an update for a destination in Amazon S3.
--
-- /Note:/ Consider using 's3DestinationUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udS3DestinationUpdate :: Lens.Lens' UpdateDestination (Lude.Maybe S3DestinationUpdate)
udS3DestinationUpdate = Lens.lens (s3DestinationUpdate :: UpdateDestination -> Lude.Maybe S3DestinationUpdate) (\s a -> s {s3DestinationUpdate = a} :: UpdateDestination)
{-# DEPRECATED udS3DestinationUpdate "Use generic-lens or generic-optics with 's3DestinationUpdate' instead." #-}

-- | Describes an update for a destination in Amazon Redshift.
--
-- /Note:/ Consider using 'redshiftDestinationUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udRedshiftDestinationUpdate :: Lens.Lens' UpdateDestination (Lude.Maybe RedshiftDestinationUpdate)
udRedshiftDestinationUpdate = Lens.lens (redshiftDestinationUpdate :: UpdateDestination -> Lude.Maybe RedshiftDestinationUpdate) (\s a -> s {redshiftDestinationUpdate = a} :: UpdateDestination)
{-# DEPRECATED udRedshiftDestinationUpdate "Use generic-lens or generic-optics with 'redshiftDestinationUpdate' instead." #-}

-- | Describes an update for a destination in Amazon ES.
--
-- /Note:/ Consider using 'elasticsearchDestinationUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udElasticsearchDestinationUpdate :: Lens.Lens' UpdateDestination (Lude.Maybe ElasticsearchDestinationUpdate)
udElasticsearchDestinationUpdate = Lens.lens (elasticsearchDestinationUpdate :: UpdateDestination -> Lude.Maybe ElasticsearchDestinationUpdate) (\s a -> s {elasticsearchDestinationUpdate = a} :: UpdateDestination)
{-# DEPRECATED udElasticsearchDestinationUpdate "Use generic-lens or generic-optics with 'elasticsearchDestinationUpdate' instead." #-}

-- | Obtain this value from the @VersionId@ result of 'DeliveryStreamDescription' . This value is required, and helps the service perform conditional operations. For example, if there is an interleaving update and this value is null, then the update destination fails. After the update is successful, the @VersionId@ value is updated. The service then performs a merge of the old configuration with the new configuration.
--
-- /Note:/ Consider using 'currentDeliveryStreamVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udCurrentDeliveryStreamVersionId :: Lens.Lens' UpdateDestination Lude.Text
udCurrentDeliveryStreamVersionId = Lens.lens (currentDeliveryStreamVersionId :: UpdateDestination -> Lude.Text) (\s a -> s {currentDeliveryStreamVersionId = a} :: UpdateDestination)
{-# DEPRECATED udCurrentDeliveryStreamVersionId "Use generic-lens or generic-optics with 'currentDeliveryStreamVersionId' instead." #-}

-- | The name of the delivery stream.
--
-- /Note:/ Consider using 'deliveryStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udDeliveryStreamName :: Lens.Lens' UpdateDestination Lude.Text
udDeliveryStreamName = Lens.lens (deliveryStreamName :: UpdateDestination -> Lude.Text) (\s a -> s {deliveryStreamName = a} :: UpdateDestination)
{-# DEPRECATED udDeliveryStreamName "Use generic-lens or generic-optics with 'deliveryStreamName' instead." #-}

-- | Describes an update for a destination in Amazon S3.
--
-- /Note:/ Consider using 'extendedS3DestinationUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udExtendedS3DestinationUpdate :: Lens.Lens' UpdateDestination (Lude.Maybe ExtendedS3DestinationUpdate)
udExtendedS3DestinationUpdate = Lens.lens (extendedS3DestinationUpdate :: UpdateDestination -> Lude.Maybe ExtendedS3DestinationUpdate) (\s a -> s {extendedS3DestinationUpdate = a} :: UpdateDestination)
{-# DEPRECATED udExtendedS3DestinationUpdate "Use generic-lens or generic-optics with 'extendedS3DestinationUpdate' instead." #-}

-- | Describes an update to the specified HTTP endpoint destination.
--
-- /Note:/ Consider using 'hTTPEndpointDestinationUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udHTTPEndpointDestinationUpdate :: Lens.Lens' UpdateDestination (Lude.Maybe HTTPEndpointDestinationUpdate)
udHTTPEndpointDestinationUpdate = Lens.lens (hTTPEndpointDestinationUpdate :: UpdateDestination -> Lude.Maybe HTTPEndpointDestinationUpdate) (\s a -> s {hTTPEndpointDestinationUpdate = a} :: UpdateDestination)
{-# DEPRECATED udHTTPEndpointDestinationUpdate "Use generic-lens or generic-optics with 'hTTPEndpointDestinationUpdate' instead." #-}

-- | The ID of the destination.
--
-- /Note:/ Consider using 'destinationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udDestinationId :: Lens.Lens' UpdateDestination Lude.Text
udDestinationId = Lens.lens (destinationId :: UpdateDestination -> Lude.Text) (\s a -> s {destinationId = a} :: UpdateDestination)
{-# DEPRECATED udDestinationId "Use generic-lens or generic-optics with 'destinationId' instead." #-}

instance Lude.AWSRequest UpdateDestination where
  type Rs UpdateDestination = UpdateDestinationResponse
  request = Req.postJSON firehoseService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateDestinationResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateDestination where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Firehose_20150804.UpdateDestination" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateDestination where
  toJSON UpdateDestination' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SplunkDestinationUpdate" Lude..=)
              Lude.<$> splunkDestinationUpdate,
            ("S3DestinationUpdate" Lude..=) Lude.<$> s3DestinationUpdate,
            ("RedshiftDestinationUpdate" Lude..=)
              Lude.<$> redshiftDestinationUpdate,
            ("ElasticsearchDestinationUpdate" Lude..=)
              Lude.<$> elasticsearchDestinationUpdate,
            Lude.Just
              ( "CurrentDeliveryStreamVersionId"
                  Lude..= currentDeliveryStreamVersionId
              ),
            Lude.Just ("DeliveryStreamName" Lude..= deliveryStreamName),
            ("ExtendedS3DestinationUpdate" Lude..=)
              Lude.<$> extendedS3DestinationUpdate,
            ("HttpEndpointDestinationUpdate" Lude..=)
              Lude.<$> hTTPEndpointDestinationUpdate,
            Lude.Just ("DestinationId" Lude..= destinationId)
          ]
      )

instance Lude.ToPath UpdateDestination where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateDestination where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateDestinationResponse' smart constructor.
newtype UpdateDestinationResponse = UpdateDestinationResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDestinationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateDestinationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateDestinationResponse
mkUpdateDestinationResponse pResponseStatus_ =
  UpdateDestinationResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrsResponseStatus :: Lens.Lens' UpdateDestinationResponse Lude.Int
udrsResponseStatus = Lens.lens (responseStatus :: UpdateDestinationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateDestinationResponse)
{-# DEPRECATED udrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
