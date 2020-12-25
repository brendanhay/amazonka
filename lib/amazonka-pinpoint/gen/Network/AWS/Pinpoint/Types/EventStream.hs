{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EventStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EventStream
  ( EventStream (..),

    -- * Smart constructor
    mkEventStream,

    -- * Lenses
    esApplicationId,
    esRoleArn,
    esDestinationStreamArn,
    esExternalId,
    esLastModifiedDate,
    esLastUpdatedBy,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies settings for publishing event data to an Amazon Kinesis data stream or an Amazon Kinesis Data Firehose delivery stream.
--
-- /See:/ 'mkEventStream' smart constructor.
data EventStream = EventStream'
  { -- | The unique identifier for the application to publish event data for.
    applicationId :: Core.Text,
    -- | The AWS Identity and Access Management (IAM) role that authorizes Amazon Pinpoint to publish event data to the stream in your AWS account.
    roleArn :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon Kinesis data stream or Amazon Kinesis Data Firehose delivery stream to publish event data to.
    --
    -- For a Kinesis data stream, the ARN format is: arn:aws:kinesis:<replaceable>region:<replaceable>account-id:stream/<replaceable>stream_name
    -- For a Kinesis Data Firehose delivery stream, the ARN format is: arn:aws:firehose:<replaceable>region:<replaceable>account-id:deliverystream/<replaceable>stream_name
    destinationStreamArn :: Core.Text,
    -- | (Deprecated) Your AWS account ID, which you assigned to an external ID key in an IAM trust policy. Amazon Pinpoint previously used this value to assume an IAM role when publishing event data, but we removed this requirement. We don't recommend use of external IDs for IAM roles that are assumed by Amazon Pinpoint.
    externalId :: Core.Maybe Core.Text,
    -- | The date, in ISO 8601 format, when the event stream was last modified.
    lastModifiedDate :: Core.Maybe Core.Text,
    -- | The IAM user who last modified the event stream.
    lastUpdatedBy :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EventStream' value with any optional fields omitted.
mkEventStream ::
  -- | 'applicationId'
  Core.Text ->
  -- | 'roleArn'
  Core.Text ->
  -- | 'destinationStreamArn'
  Core.Text ->
  EventStream
mkEventStream applicationId roleArn destinationStreamArn =
  EventStream'
    { applicationId,
      roleArn,
      destinationStreamArn,
      externalId = Core.Nothing,
      lastModifiedDate = Core.Nothing,
      lastUpdatedBy = Core.Nothing
    }

-- | The unique identifier for the application to publish event data for.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esApplicationId :: Lens.Lens' EventStream Core.Text
esApplicationId = Lens.field @"applicationId"
{-# DEPRECATED esApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The AWS Identity and Access Management (IAM) role that authorizes Amazon Pinpoint to publish event data to the stream in your AWS account.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esRoleArn :: Lens.Lens' EventStream Core.Text
esRoleArn = Lens.field @"roleArn"
{-# DEPRECATED esRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | The Amazon Resource Name (ARN) of the Amazon Kinesis data stream or Amazon Kinesis Data Firehose delivery stream to publish event data to.
--
-- For a Kinesis data stream, the ARN format is: arn:aws:kinesis:<replaceable>region:<replaceable>account-id:stream/<replaceable>stream_name
-- For a Kinesis Data Firehose delivery stream, the ARN format is: arn:aws:firehose:<replaceable>region:<replaceable>account-id:deliverystream/<replaceable>stream_name
--
-- /Note:/ Consider using 'destinationStreamArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esDestinationStreamArn :: Lens.Lens' EventStream Core.Text
esDestinationStreamArn = Lens.field @"destinationStreamArn"
{-# DEPRECATED esDestinationStreamArn "Use generic-lens or generic-optics with 'destinationStreamArn' instead." #-}

-- | (Deprecated) Your AWS account ID, which you assigned to an external ID key in an IAM trust policy. Amazon Pinpoint previously used this value to assume an IAM role when publishing event data, but we removed this requirement. We don't recommend use of external IDs for IAM roles that are assumed by Amazon Pinpoint.
--
-- /Note:/ Consider using 'externalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esExternalId :: Lens.Lens' EventStream (Core.Maybe Core.Text)
esExternalId = Lens.field @"externalId"
{-# DEPRECATED esExternalId "Use generic-lens or generic-optics with 'externalId' instead." #-}

-- | The date, in ISO 8601 format, when the event stream was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esLastModifiedDate :: Lens.Lens' EventStream (Core.Maybe Core.Text)
esLastModifiedDate = Lens.field @"lastModifiedDate"
{-# DEPRECATED esLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The IAM user who last modified the event stream.
--
-- /Note:/ Consider using 'lastUpdatedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esLastUpdatedBy :: Lens.Lens' EventStream (Core.Maybe Core.Text)
esLastUpdatedBy = Lens.field @"lastUpdatedBy"
{-# DEPRECATED esLastUpdatedBy "Use generic-lens or generic-optics with 'lastUpdatedBy' instead." #-}

instance Core.FromJSON EventStream where
  parseJSON =
    Core.withObject "EventStream" Core.$
      \x ->
        EventStream'
          Core.<$> (x Core..: "ApplicationId")
          Core.<*> (x Core..: "RoleArn")
          Core.<*> (x Core..: "DestinationStreamArn")
          Core.<*> (x Core..:? "ExternalId")
          Core.<*> (x Core..:? "LastModifiedDate")
          Core.<*> (x Core..:? "LastUpdatedBy")
