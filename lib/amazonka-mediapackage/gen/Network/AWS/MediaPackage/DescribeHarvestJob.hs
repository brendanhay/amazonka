{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.DescribeHarvestJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details about an existing HarvestJob.
module Network.AWS.MediaPackage.DescribeHarvestJob
  ( -- * Creating a request
    DescribeHarvestJob (..),
    mkDescribeHarvestJob,

    -- ** Request lenses
    dhjId,

    -- * Destructuring the response
    DescribeHarvestJobResponse (..),
    mkDescribeHarvestJobResponse,

    -- ** Response lenses
    dhjrsStatus,
    dhjrsOriginEndpointId,
    dhjrsStartTime,
    dhjrsARN,
    dhjrsCreatedAt,
    dhjrsChannelId,
    dhjrsS3Destination,
    dhjrsEndTime,
    dhjrsId,
    dhjrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeHarvestJob' smart constructor.
newtype DescribeHarvestJob = DescribeHarvestJob'
  { -- | The ID of the HarvestJob.
    id :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeHarvestJob' with the minimum fields required to make a request.
--
-- * 'id' - The ID of the HarvestJob.
mkDescribeHarvestJob ::
  -- | 'id'
  Lude.Text ->
  DescribeHarvestJob
mkDescribeHarvestJob pId_ = DescribeHarvestJob' {id = pId_}

-- | The ID of the HarvestJob.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhjId :: Lens.Lens' DescribeHarvestJob Lude.Text
dhjId = Lens.lens (id :: DescribeHarvestJob -> Lude.Text) (\s a -> s {id = a} :: DescribeHarvestJob)
{-# DEPRECATED dhjId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest DescribeHarvestJob where
  type Rs DescribeHarvestJob = DescribeHarvestJobResponse
  request = Req.get mediaPackageService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeHarvestJobResponse'
            Lude.<$> (x Lude..?> "status")
            Lude.<*> (x Lude..?> "originEndpointId")
            Lude.<*> (x Lude..?> "startTime")
            Lude.<*> (x Lude..?> "arn")
            Lude.<*> (x Lude..?> "createdAt")
            Lude.<*> (x Lude..?> "channelId")
            Lude.<*> (x Lude..?> "s3Destination")
            Lude.<*> (x Lude..?> "endTime")
            Lude.<*> (x Lude..?> "id")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeHarvestJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DescribeHarvestJob where
  toPath DescribeHarvestJob' {..} =
    Lude.mconcat ["/harvest_jobs/", Lude.toBS id]

instance Lude.ToQuery DescribeHarvestJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeHarvestJobResponse' smart constructor.
data DescribeHarvestJobResponse = DescribeHarvestJobResponse'
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
    id :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeHarvestJobResponse' with the minimum fields required to make a request.
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
-- * 'responseStatus' - The response status code.
mkDescribeHarvestJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeHarvestJobResponse
mkDescribeHarvestJobResponse pResponseStatus_ =
  DescribeHarvestJobResponse'
    { status = Lude.Nothing,
      originEndpointId = Lude.Nothing,
      startTime = Lude.Nothing,
      arn = Lude.Nothing,
      createdAt = Lude.Nothing,
      channelId = Lude.Nothing,
      s3Destination = Lude.Nothing,
      endTime = Lude.Nothing,
      id = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current status of the HarvestJob. Consider setting up a CloudWatch Event to listen for
--
-- HarvestJobs as they succeed or fail. In the event of failure, the CloudWatch Event will
-- include an explanation of why the HarvestJob failed.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhjrsStatus :: Lens.Lens' DescribeHarvestJobResponse (Lude.Maybe Status)
dhjrsStatus = Lens.lens (status :: DescribeHarvestJobResponse -> Lude.Maybe Status) (\s a -> s {status = a} :: DescribeHarvestJobResponse)
{-# DEPRECATED dhjrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The ID of the OriginEndpoint that the HarvestJob will harvest from.
--
-- This cannot be changed after the HarvestJob is submitted.
--
-- /Note:/ Consider using 'originEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhjrsOriginEndpointId :: Lens.Lens' DescribeHarvestJobResponse (Lude.Maybe Lude.Text)
dhjrsOriginEndpointId = Lens.lens (originEndpointId :: DescribeHarvestJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {originEndpointId = a} :: DescribeHarvestJobResponse)
{-# DEPRECATED dhjrsOriginEndpointId "Use generic-lens or generic-optics with 'originEndpointId' instead." #-}

-- | The start of the time-window which will be harvested.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhjrsStartTime :: Lens.Lens' DescribeHarvestJobResponse (Lude.Maybe Lude.Text)
dhjrsStartTime = Lens.lens (startTime :: DescribeHarvestJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {startTime = a} :: DescribeHarvestJobResponse)
{-# DEPRECATED dhjrsStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The Amazon Resource Name (ARN) assigned to the HarvestJob.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhjrsARN :: Lens.Lens' DescribeHarvestJobResponse (Lude.Maybe Lude.Text)
dhjrsARN = Lens.lens (arn :: DescribeHarvestJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: DescribeHarvestJobResponse)
{-# DEPRECATED dhjrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time the HarvestJob was submitted
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhjrsCreatedAt :: Lens.Lens' DescribeHarvestJobResponse (Lude.Maybe Lude.Text)
dhjrsCreatedAt = Lens.lens (createdAt :: DescribeHarvestJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {createdAt = a} :: DescribeHarvestJobResponse)
{-# DEPRECATED dhjrsCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The ID of the Channel that the HarvestJob will harvest from.
--
-- /Note:/ Consider using 'channelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhjrsChannelId :: Lens.Lens' DescribeHarvestJobResponse (Lude.Maybe Lude.Text)
dhjrsChannelId = Lens.lens (channelId :: DescribeHarvestJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {channelId = a} :: DescribeHarvestJobResponse)
{-# DEPRECATED dhjrsChannelId "Use generic-lens or generic-optics with 'channelId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 's3Destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhjrsS3Destination :: Lens.Lens' DescribeHarvestJobResponse (Lude.Maybe S3Destination)
dhjrsS3Destination = Lens.lens (s3Destination :: DescribeHarvestJobResponse -> Lude.Maybe S3Destination) (\s a -> s {s3Destination = a} :: DescribeHarvestJobResponse)
{-# DEPRECATED dhjrsS3Destination "Use generic-lens or generic-optics with 's3Destination' instead." #-}

-- | The end of the time-window which will be harvested.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhjrsEndTime :: Lens.Lens' DescribeHarvestJobResponse (Lude.Maybe Lude.Text)
dhjrsEndTime = Lens.lens (endTime :: DescribeHarvestJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {endTime = a} :: DescribeHarvestJobResponse)
{-# DEPRECATED dhjrsEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The ID of the HarvestJob. The ID must be unique within the region
--
-- and it cannot be changed after the HarvestJob is submitted.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhjrsId :: Lens.Lens' DescribeHarvestJobResponse (Lude.Maybe Lude.Text)
dhjrsId = Lens.lens (id :: DescribeHarvestJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: DescribeHarvestJobResponse)
{-# DEPRECATED dhjrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhjrsResponseStatus :: Lens.Lens' DescribeHarvestJobResponse Lude.Int
dhjrsResponseStatus = Lens.lens (responseStatus :: DescribeHarvestJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeHarvestJobResponse)
{-# DEPRECATED dhjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
