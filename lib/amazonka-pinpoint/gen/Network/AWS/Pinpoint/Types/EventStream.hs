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
    esLastUpdatedBy,
    esLastModifiedDate,
    esExternalId,
    esApplicationId,
    esRoleARN,
    esDestinationStreamARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies settings for publishing event data to an Amazon Kinesis data stream or an Amazon Kinesis Data Firehose delivery stream.
--
-- /See:/ 'mkEventStream' smart constructor.
data EventStream = EventStream'
  { lastUpdatedBy ::
      Lude.Maybe Lude.Text,
    lastModifiedDate :: Lude.Maybe Lude.Text,
    externalId :: Lude.Maybe Lude.Text,
    applicationId :: Lude.Text,
    roleARN :: Lude.Text,
    destinationStreamARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EventStream' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application to publish event data for.
-- * 'destinationStreamARN' - The Amazon Resource Name (ARN) of the Amazon Kinesis data stream or Amazon Kinesis Data Firehose delivery stream to publish event data to.
--
-- For a Kinesis data stream, the ARN format is: arn:aws:kinesis:<replaceable>region:<replaceable>account-id:stream/<replaceable>stream_name
-- For a Kinesis Data Firehose delivery stream, the ARN format is: arn:aws:firehose:<replaceable>region:<replaceable>account-id:deliverystream/<replaceable>stream_name
-- * 'externalId' - (Deprecated) Your AWS account ID, which you assigned to an external ID key in an IAM trust policy. Amazon Pinpoint previously used this value to assume an IAM role when publishing event data, but we removed this requirement. We don't recommend use of external IDs for IAM roles that are assumed by Amazon Pinpoint.
-- * 'lastModifiedDate' - The date, in ISO 8601 format, when the event stream was last modified.
-- * 'lastUpdatedBy' - The IAM user who last modified the event stream.
-- * 'roleARN' - The AWS Identity and Access Management (IAM) role that authorizes Amazon Pinpoint to publish event data to the stream in your AWS account.
mkEventStream ::
  -- | 'applicationId'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  -- | 'destinationStreamARN'
  Lude.Text ->
  EventStream
mkEventStream pApplicationId_ pRoleARN_ pDestinationStreamARN_ =
  EventStream'
    { lastUpdatedBy = Lude.Nothing,
      lastModifiedDate = Lude.Nothing,
      externalId = Lude.Nothing,
      applicationId = pApplicationId_,
      roleARN = pRoleARN_,
      destinationStreamARN = pDestinationStreamARN_
    }

-- | The IAM user who last modified the event stream.
--
-- /Note:/ Consider using 'lastUpdatedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esLastUpdatedBy :: Lens.Lens' EventStream (Lude.Maybe Lude.Text)
esLastUpdatedBy = Lens.lens (lastUpdatedBy :: EventStream -> Lude.Maybe Lude.Text) (\s a -> s {lastUpdatedBy = a} :: EventStream)
{-# DEPRECATED esLastUpdatedBy "Use generic-lens or generic-optics with 'lastUpdatedBy' instead." #-}

-- | The date, in ISO 8601 format, when the event stream was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esLastModifiedDate :: Lens.Lens' EventStream (Lude.Maybe Lude.Text)
esLastModifiedDate = Lens.lens (lastModifiedDate :: EventStream -> Lude.Maybe Lude.Text) (\s a -> s {lastModifiedDate = a} :: EventStream)
{-# DEPRECATED esLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | (Deprecated) Your AWS account ID, which you assigned to an external ID key in an IAM trust policy. Amazon Pinpoint previously used this value to assume an IAM role when publishing event data, but we removed this requirement. We don't recommend use of external IDs for IAM roles that are assumed by Amazon Pinpoint.
--
-- /Note:/ Consider using 'externalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esExternalId :: Lens.Lens' EventStream (Lude.Maybe Lude.Text)
esExternalId = Lens.lens (externalId :: EventStream -> Lude.Maybe Lude.Text) (\s a -> s {externalId = a} :: EventStream)
{-# DEPRECATED esExternalId "Use generic-lens or generic-optics with 'externalId' instead." #-}

-- | The unique identifier for the application to publish event data for.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esApplicationId :: Lens.Lens' EventStream Lude.Text
esApplicationId = Lens.lens (applicationId :: EventStream -> Lude.Text) (\s a -> s {applicationId = a} :: EventStream)
{-# DEPRECATED esApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The AWS Identity and Access Management (IAM) role that authorizes Amazon Pinpoint to publish event data to the stream in your AWS account.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esRoleARN :: Lens.Lens' EventStream Lude.Text
esRoleARN = Lens.lens (roleARN :: EventStream -> Lude.Text) (\s a -> s {roleARN = a} :: EventStream)
{-# DEPRECATED esRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The Amazon Resource Name (ARN) of the Amazon Kinesis data stream or Amazon Kinesis Data Firehose delivery stream to publish event data to.
--
-- For a Kinesis data stream, the ARN format is: arn:aws:kinesis:<replaceable>region:<replaceable>account-id:stream/<replaceable>stream_name
-- For a Kinesis Data Firehose delivery stream, the ARN format is: arn:aws:firehose:<replaceable>region:<replaceable>account-id:deliverystream/<replaceable>stream_name
--
-- /Note:/ Consider using 'destinationStreamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esDestinationStreamARN :: Lens.Lens' EventStream Lude.Text
esDestinationStreamARN = Lens.lens (destinationStreamARN :: EventStream -> Lude.Text) (\s a -> s {destinationStreamARN = a} :: EventStream)
{-# DEPRECATED esDestinationStreamARN "Use generic-lens or generic-optics with 'destinationStreamARN' instead." #-}

instance Lude.FromJSON EventStream where
  parseJSON =
    Lude.withObject
      "EventStream"
      ( \x ->
          EventStream'
            Lude.<$> (x Lude..:? "LastUpdatedBy")
            Lude.<*> (x Lude..:? "LastModifiedDate")
            Lude.<*> (x Lude..:? "ExternalId")
            Lude.<*> (x Lude..: "ApplicationId")
            Lude.<*> (x Lude..: "RoleArn")
            Lude.<*> (x Lude..: "DestinationStreamArn")
      )
