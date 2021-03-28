{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      UpdateDestination (..)
    , mkUpdateDestination
    -- ** Request lenses
    , udDeliveryStreamName
    , udCurrentDeliveryStreamVersionId
    , udDestinationId
    , udElasticsearchDestinationUpdate
    , udExtendedS3DestinationUpdate
    , udHttpEndpointDestinationUpdate
    , udRedshiftDestinationUpdate
    , udS3DestinationUpdate
    , udSplunkDestinationUpdate

    -- * Destructuring the response
    , UpdateDestinationResponse (..)
    , mkUpdateDestinationResponse
    -- ** Response lenses
    , udrrsResponseStatus
    ) where

import qualified Network.AWS.Firehose.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateDestination' smart constructor.
data UpdateDestination = UpdateDestination'
  { deliveryStreamName :: Types.DeliveryStreamName
    -- ^ The name of the delivery stream.
  , currentDeliveryStreamVersionId :: Types.DeliveryStreamVersionId
    -- ^ Obtain this value from the @VersionId@ result of 'DeliveryStreamDescription' . This value is required, and helps the service perform conditional operations. For example, if there is an interleaving update and this value is null, then the update destination fails. After the update is successful, the @VersionId@ value is updated. The service then performs a merge of the old configuration with the new configuration.
  , destinationId :: Types.DestinationId
    -- ^ The ID of the destination.
  , elasticsearchDestinationUpdate :: Core.Maybe Types.ElasticsearchDestinationUpdate
    -- ^ Describes an update for a destination in Amazon ES.
  , extendedS3DestinationUpdate :: Core.Maybe Types.ExtendedS3DestinationUpdate
    -- ^ Describes an update for a destination in Amazon S3.
  , httpEndpointDestinationUpdate :: Core.Maybe Types.HttpEndpointDestinationUpdate
    -- ^ Describes an update to the specified HTTP endpoint destination.
  , redshiftDestinationUpdate :: Core.Maybe Types.RedshiftDestinationUpdate
    -- ^ Describes an update for a destination in Amazon Redshift.
  , s3DestinationUpdate :: Core.Maybe Types.S3DestinationUpdate
    -- ^ [Deprecated] Describes an update for a destination in Amazon S3.
  , splunkDestinationUpdate :: Core.Maybe Types.SplunkDestinationUpdate
    -- ^ Describes an update for a destination in Splunk.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDestination' value with any optional fields omitted.
mkUpdateDestination
    :: Types.DeliveryStreamName -- ^ 'deliveryStreamName'
    -> Types.DeliveryStreamVersionId -- ^ 'currentDeliveryStreamVersionId'
    -> Types.DestinationId -- ^ 'destinationId'
    -> UpdateDestination
mkUpdateDestination deliveryStreamName
  currentDeliveryStreamVersionId destinationId
  = UpdateDestination'{deliveryStreamName,
                       currentDeliveryStreamVersionId, destinationId,
                       elasticsearchDestinationUpdate = Core.Nothing,
                       extendedS3DestinationUpdate = Core.Nothing,
                       httpEndpointDestinationUpdate = Core.Nothing,
                       redshiftDestinationUpdate = Core.Nothing,
                       s3DestinationUpdate = Core.Nothing,
                       splunkDestinationUpdate = Core.Nothing}

-- | The name of the delivery stream.
--
-- /Note:/ Consider using 'deliveryStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udDeliveryStreamName :: Lens.Lens' UpdateDestination Types.DeliveryStreamName
udDeliveryStreamName = Lens.field @"deliveryStreamName"
{-# INLINEABLE udDeliveryStreamName #-}
{-# DEPRECATED deliveryStreamName "Use generic-lens or generic-optics with 'deliveryStreamName' instead"  #-}

-- | Obtain this value from the @VersionId@ result of 'DeliveryStreamDescription' . This value is required, and helps the service perform conditional operations. For example, if there is an interleaving update and this value is null, then the update destination fails. After the update is successful, the @VersionId@ value is updated. The service then performs a merge of the old configuration with the new configuration.
--
-- /Note:/ Consider using 'currentDeliveryStreamVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udCurrentDeliveryStreamVersionId :: Lens.Lens' UpdateDestination Types.DeliveryStreamVersionId
udCurrentDeliveryStreamVersionId = Lens.field @"currentDeliveryStreamVersionId"
{-# INLINEABLE udCurrentDeliveryStreamVersionId #-}
{-# DEPRECATED currentDeliveryStreamVersionId "Use generic-lens or generic-optics with 'currentDeliveryStreamVersionId' instead"  #-}

-- | The ID of the destination.
--
-- /Note:/ Consider using 'destinationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udDestinationId :: Lens.Lens' UpdateDestination Types.DestinationId
udDestinationId = Lens.field @"destinationId"
{-# INLINEABLE udDestinationId #-}
{-# DEPRECATED destinationId "Use generic-lens or generic-optics with 'destinationId' instead"  #-}

-- | Describes an update for a destination in Amazon ES.
--
-- /Note:/ Consider using 'elasticsearchDestinationUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udElasticsearchDestinationUpdate :: Lens.Lens' UpdateDestination (Core.Maybe Types.ElasticsearchDestinationUpdate)
udElasticsearchDestinationUpdate = Lens.field @"elasticsearchDestinationUpdate"
{-# INLINEABLE udElasticsearchDestinationUpdate #-}
{-# DEPRECATED elasticsearchDestinationUpdate "Use generic-lens or generic-optics with 'elasticsearchDestinationUpdate' instead"  #-}

-- | Describes an update for a destination in Amazon S3.
--
-- /Note:/ Consider using 'extendedS3DestinationUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udExtendedS3DestinationUpdate :: Lens.Lens' UpdateDestination (Core.Maybe Types.ExtendedS3DestinationUpdate)
udExtendedS3DestinationUpdate = Lens.field @"extendedS3DestinationUpdate"
{-# INLINEABLE udExtendedS3DestinationUpdate #-}
{-# DEPRECATED extendedS3DestinationUpdate "Use generic-lens or generic-optics with 'extendedS3DestinationUpdate' instead"  #-}

-- | Describes an update to the specified HTTP endpoint destination.
--
-- /Note:/ Consider using 'httpEndpointDestinationUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udHttpEndpointDestinationUpdate :: Lens.Lens' UpdateDestination (Core.Maybe Types.HttpEndpointDestinationUpdate)
udHttpEndpointDestinationUpdate = Lens.field @"httpEndpointDestinationUpdate"
{-# INLINEABLE udHttpEndpointDestinationUpdate #-}
{-# DEPRECATED httpEndpointDestinationUpdate "Use generic-lens or generic-optics with 'httpEndpointDestinationUpdate' instead"  #-}

-- | Describes an update for a destination in Amazon Redshift.
--
-- /Note:/ Consider using 'redshiftDestinationUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udRedshiftDestinationUpdate :: Lens.Lens' UpdateDestination (Core.Maybe Types.RedshiftDestinationUpdate)
udRedshiftDestinationUpdate = Lens.field @"redshiftDestinationUpdate"
{-# INLINEABLE udRedshiftDestinationUpdate #-}
{-# DEPRECATED redshiftDestinationUpdate "Use generic-lens or generic-optics with 'redshiftDestinationUpdate' instead"  #-}

-- | [Deprecated] Describes an update for a destination in Amazon S3.
--
-- /Note:/ Consider using 's3DestinationUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udS3DestinationUpdate :: Lens.Lens' UpdateDestination (Core.Maybe Types.S3DestinationUpdate)
udS3DestinationUpdate = Lens.field @"s3DestinationUpdate"
{-# INLINEABLE udS3DestinationUpdate #-}
{-# DEPRECATED s3DestinationUpdate "Use generic-lens or generic-optics with 's3DestinationUpdate' instead"  #-}

-- | Describes an update for a destination in Splunk.
--
-- /Note:/ Consider using 'splunkDestinationUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udSplunkDestinationUpdate :: Lens.Lens' UpdateDestination (Core.Maybe Types.SplunkDestinationUpdate)
udSplunkDestinationUpdate = Lens.field @"splunkDestinationUpdate"
{-# INLINEABLE udSplunkDestinationUpdate #-}
{-# DEPRECATED splunkDestinationUpdate "Use generic-lens or generic-optics with 'splunkDestinationUpdate' instead"  #-}

instance Core.ToQuery UpdateDestination where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateDestination where
        toHeaders UpdateDestination{..}
          = Core.pure ("X-Amz-Target", "Firehose_20150804.UpdateDestination")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateDestination where
        toJSON UpdateDestination{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DeliveryStreamName" Core..= deliveryStreamName),
                  Core.Just
                    ("CurrentDeliveryStreamVersionId" Core..=
                       currentDeliveryStreamVersionId),
                  Core.Just ("DestinationId" Core..= destinationId),
                  ("ElasticsearchDestinationUpdate" Core..=) Core.<$>
                    elasticsearchDestinationUpdate,
                  ("ExtendedS3DestinationUpdate" Core..=) Core.<$>
                    extendedS3DestinationUpdate,
                  ("HttpEndpointDestinationUpdate" Core..=) Core.<$>
                    httpEndpointDestinationUpdate,
                  ("RedshiftDestinationUpdate" Core..=) Core.<$>
                    redshiftDestinationUpdate,
                  ("S3DestinationUpdate" Core..=) Core.<$> s3DestinationUpdate,
                  ("SplunkDestinationUpdate" Core..=) Core.<$>
                    splunkDestinationUpdate])

instance Core.AWSRequest UpdateDestination where
        type Rs UpdateDestination = UpdateDestinationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateDestinationResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateDestinationResponse' smart constructor.
newtype UpdateDestinationResponse = UpdateDestinationResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDestinationResponse' value with any optional fields omitted.
mkUpdateDestinationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateDestinationResponse
mkUpdateDestinationResponse responseStatus
  = UpdateDestinationResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrrsResponseStatus :: Lens.Lens' UpdateDestinationResponse Core.Int
udrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE udrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
