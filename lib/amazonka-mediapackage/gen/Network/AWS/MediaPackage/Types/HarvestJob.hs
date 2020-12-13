{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.HarvestJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.HarvestJob
  ( HarvestJob (..),

    -- * Smart constructor
    mkHarvestJob,

    -- * Lenses
    hjStatus,
    hjOriginEndpointId,
    hjStartTime,
    hjARN,
    hjCreatedAt,
    hjChannelId,
    hjS3Destination,
    hjEndTime,
    hjId,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types.S3Destination
import Network.AWS.MediaPackage.Types.Status
import qualified Network.AWS.Prelude as Lude

-- | A HarvestJob resource configuration
--
-- /See:/ 'mkHarvestJob' smart constructor.
data HarvestJob = HarvestJob'
  { -- | The current status of the HarvestJob. Consider setting up a CloudWatch Event to listen for
    --
    -- HarvestJobs as they succeed or fail. In the event of failure, the CloudWatch Event will
    -- include an explanation of why the HarvestJob failed.
    status :: Lude.Maybe Status,
    -- | The ID of the OriginEndpoint that the HarvestJob will harvest from.
    --
    -- This cannot be changed after the HarvestJob is submitted.
    originEndpointId :: Lude.Maybe Lude.Text,
    -- | The start of the time-window which will be harvested.
    startTime :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) assigned to the HarvestJob.
    arn :: Lude.Maybe Lude.Text,
    -- | The time the HarvestJob was submitted
    createdAt :: Lude.Maybe Lude.Text,
    -- | The ID of the Channel that the HarvestJob will harvest from.
    channelId :: Lude.Maybe Lude.Text,
    s3Destination :: Lude.Maybe S3Destination,
    -- | The end of the time-window which will be harvested.
    endTime :: Lude.Maybe Lude.Text,
    -- | The ID of the HarvestJob. The ID must be unique within the region
    --
    -- and it cannot be changed after the HarvestJob is submitted.
    id :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HarvestJob' with the minimum fields required to make a request.
--
-- * 'status' - The current status of the HarvestJob. Consider setting up a CloudWatch Event to listen for
--
-- HarvestJobs as they succeed or fail. In the event of failure, the CloudWatch Event will
-- include an explanation of why the HarvestJob failed.
-- * 'originEndpointId' - The ID of the OriginEndpoint that the HarvestJob will harvest from.
--
-- This cannot be changed after the HarvestJob is submitted.
-- * 'startTime' - The start of the time-window which will be harvested.
-- * 'arn' - The Amazon Resource Name (ARN) assigned to the HarvestJob.
-- * 'createdAt' - The time the HarvestJob was submitted
-- * 'channelId' - The ID of the Channel that the HarvestJob will harvest from.
-- * 's3Destination' -
-- * 'endTime' - The end of the time-window which will be harvested.
-- * 'id' - The ID of the HarvestJob. The ID must be unique within the region
--
-- and it cannot be changed after the HarvestJob is submitted.
mkHarvestJob ::
  HarvestJob
mkHarvestJob =
  HarvestJob'
    { status = Lude.Nothing,
      originEndpointId = Lude.Nothing,
      startTime = Lude.Nothing,
      arn = Lude.Nothing,
      createdAt = Lude.Nothing,
      channelId = Lude.Nothing,
      s3Destination = Lude.Nothing,
      endTime = Lude.Nothing,
      id = Lude.Nothing
    }

-- | The current status of the HarvestJob. Consider setting up a CloudWatch Event to listen for
--
-- HarvestJobs as they succeed or fail. In the event of failure, the CloudWatch Event will
-- include an explanation of why the HarvestJob failed.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hjStatus :: Lens.Lens' HarvestJob (Lude.Maybe Status)
hjStatus = Lens.lens (status :: HarvestJob -> Lude.Maybe Status) (\s a -> s {status = a} :: HarvestJob)
{-# DEPRECATED hjStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The ID of the OriginEndpoint that the HarvestJob will harvest from.
--
-- This cannot be changed after the HarvestJob is submitted.
--
-- /Note:/ Consider using 'originEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hjOriginEndpointId :: Lens.Lens' HarvestJob (Lude.Maybe Lude.Text)
hjOriginEndpointId = Lens.lens (originEndpointId :: HarvestJob -> Lude.Maybe Lude.Text) (\s a -> s {originEndpointId = a} :: HarvestJob)
{-# DEPRECATED hjOriginEndpointId "Use generic-lens or generic-optics with 'originEndpointId' instead." #-}

-- | The start of the time-window which will be harvested.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hjStartTime :: Lens.Lens' HarvestJob (Lude.Maybe Lude.Text)
hjStartTime = Lens.lens (startTime :: HarvestJob -> Lude.Maybe Lude.Text) (\s a -> s {startTime = a} :: HarvestJob)
{-# DEPRECATED hjStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The Amazon Resource Name (ARN) assigned to the HarvestJob.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hjARN :: Lens.Lens' HarvestJob (Lude.Maybe Lude.Text)
hjARN = Lens.lens (arn :: HarvestJob -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: HarvestJob)
{-# DEPRECATED hjARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time the HarvestJob was submitted
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hjCreatedAt :: Lens.Lens' HarvestJob (Lude.Maybe Lude.Text)
hjCreatedAt = Lens.lens (createdAt :: HarvestJob -> Lude.Maybe Lude.Text) (\s a -> s {createdAt = a} :: HarvestJob)
{-# DEPRECATED hjCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The ID of the Channel that the HarvestJob will harvest from.
--
-- /Note:/ Consider using 'channelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hjChannelId :: Lens.Lens' HarvestJob (Lude.Maybe Lude.Text)
hjChannelId = Lens.lens (channelId :: HarvestJob -> Lude.Maybe Lude.Text) (\s a -> s {channelId = a} :: HarvestJob)
{-# DEPRECATED hjChannelId "Use generic-lens or generic-optics with 'channelId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 's3Destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hjS3Destination :: Lens.Lens' HarvestJob (Lude.Maybe S3Destination)
hjS3Destination = Lens.lens (s3Destination :: HarvestJob -> Lude.Maybe S3Destination) (\s a -> s {s3Destination = a} :: HarvestJob)
{-# DEPRECATED hjS3Destination "Use generic-lens or generic-optics with 's3Destination' instead." #-}

-- | The end of the time-window which will be harvested.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hjEndTime :: Lens.Lens' HarvestJob (Lude.Maybe Lude.Text)
hjEndTime = Lens.lens (endTime :: HarvestJob -> Lude.Maybe Lude.Text) (\s a -> s {endTime = a} :: HarvestJob)
{-# DEPRECATED hjEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The ID of the HarvestJob. The ID must be unique within the region
--
-- and it cannot be changed after the HarvestJob is submitted.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hjId :: Lens.Lens' HarvestJob (Lude.Maybe Lude.Text)
hjId = Lens.lens (id :: HarvestJob -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: HarvestJob)
{-# DEPRECATED hjId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromJSON HarvestJob where
  parseJSON =
    Lude.withObject
      "HarvestJob"
      ( \x ->
          HarvestJob'
            Lude.<$> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "originEndpointId")
            Lude.<*> (x Lude..:? "startTime")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "createdAt")
            Lude.<*> (x Lude..:? "channelId")
            Lude.<*> (x Lude..:? "s3Destination")
            Lude.<*> (x Lude..:? "endTime")
            Lude.<*> (x Lude..:? "id")
      )
