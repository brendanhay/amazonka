{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.CreateHarvestJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new HarvestJob record.
module Network.AWS.MediaPackage.CreateHarvestJob
  ( -- * Creating a request
    CreateHarvestJob (..),
    mkCreateHarvestJob,

    -- ** Request lenses
    chjS3Destination,
    chjEndTime,
    chjOriginEndpointId,
    chjStartTime,
    chjId,

    -- * Destructuring the response
    CreateHarvestJobResponse (..),
    mkCreateHarvestJobResponse,

    -- ** Response lenses
    chjrsStatus,
    chjrsOriginEndpointId,
    chjrsStartTime,
    chjrsARN,
    chjrsCreatedAt,
    chjrsChannelId,
    chjrsS3Destination,
    chjrsEndTime,
    chjrsId,
    chjrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Configuration parameters used to create a new HarvestJob.
--
-- /See:/ 'mkCreateHarvestJob' smart constructor.
data CreateHarvestJob = CreateHarvestJob'
  { s3Destination ::
      S3Destination,
    endTime :: Lude.Text,
    originEndpointId :: Lude.Text,
    startTime :: Lude.Text,
    id :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateHarvestJob' with the minimum fields required to make a request.
--
-- * 'endTime' - The end of the time-window which will be harvested
-- * 'id' - The ID of the HarvestJob. The ID must be unique within the region
--
-- and it cannot be changed after the HarvestJob is submitted
-- * 'originEndpointId' - The ID of the OriginEndpoint that the HarvestJob will harvest from.
--
-- This cannot be changed after the HarvestJob is submitted.
-- * 's3Destination' - Undocumented field.
-- * 'startTime' - The start of the time-window which will be harvested
mkCreateHarvestJob ::
  -- | 's3Destination'
  S3Destination ->
  -- | 'endTime'
  Lude.Text ->
  -- | 'originEndpointId'
  Lude.Text ->
  -- | 'startTime'
  Lude.Text ->
  -- | 'id'
  Lude.Text ->
  CreateHarvestJob
mkCreateHarvestJob
  pS3Destination_
  pEndTime_
  pOriginEndpointId_
  pStartTime_
  pId_ =
    CreateHarvestJob'
      { s3Destination = pS3Destination_,
        endTime = pEndTime_,
        originEndpointId = pOriginEndpointId_,
        startTime = pStartTime_,
        id = pId_
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 's3Destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chjS3Destination :: Lens.Lens' CreateHarvestJob S3Destination
chjS3Destination = Lens.lens (s3Destination :: CreateHarvestJob -> S3Destination) (\s a -> s {s3Destination = a} :: CreateHarvestJob)
{-# DEPRECATED chjS3Destination "Use generic-lens or generic-optics with 's3Destination' instead." #-}

-- | The end of the time-window which will be harvested
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chjEndTime :: Lens.Lens' CreateHarvestJob Lude.Text
chjEndTime = Lens.lens (endTime :: CreateHarvestJob -> Lude.Text) (\s a -> s {endTime = a} :: CreateHarvestJob)
{-# DEPRECATED chjEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The ID of the OriginEndpoint that the HarvestJob will harvest from.
--
-- This cannot be changed after the HarvestJob is submitted.
--
-- /Note:/ Consider using 'originEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chjOriginEndpointId :: Lens.Lens' CreateHarvestJob Lude.Text
chjOriginEndpointId = Lens.lens (originEndpointId :: CreateHarvestJob -> Lude.Text) (\s a -> s {originEndpointId = a} :: CreateHarvestJob)
{-# DEPRECATED chjOriginEndpointId "Use generic-lens or generic-optics with 'originEndpointId' instead." #-}

-- | The start of the time-window which will be harvested
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chjStartTime :: Lens.Lens' CreateHarvestJob Lude.Text
chjStartTime = Lens.lens (startTime :: CreateHarvestJob -> Lude.Text) (\s a -> s {startTime = a} :: CreateHarvestJob)
{-# DEPRECATED chjStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The ID of the HarvestJob. The ID must be unique within the region
--
-- and it cannot be changed after the HarvestJob is submitted
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chjId :: Lens.Lens' CreateHarvestJob Lude.Text
chjId = Lens.lens (id :: CreateHarvestJob -> Lude.Text) (\s a -> s {id = a} :: CreateHarvestJob)
{-# DEPRECATED chjId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest CreateHarvestJob where
  type Rs CreateHarvestJob = CreateHarvestJobResponse
  request = Req.postJSON mediaPackageService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateHarvestJobResponse'
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

instance Lude.ToHeaders CreateHarvestJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateHarvestJob where
  toJSON CreateHarvestJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("s3Destination" Lude..= s3Destination),
            Lude.Just ("endTime" Lude..= endTime),
            Lude.Just ("originEndpointId" Lude..= originEndpointId),
            Lude.Just ("startTime" Lude..= startTime),
            Lude.Just ("id" Lude..= id)
          ]
      )

instance Lude.ToPath CreateHarvestJob where
  toPath = Lude.const "/harvest_jobs"

instance Lude.ToQuery CreateHarvestJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateHarvestJobResponse' smart constructor.
data CreateHarvestJobResponse = CreateHarvestJobResponse'
  { status ::
      Lude.Maybe Status,
    originEndpointId :: Lude.Maybe Lude.Text,
    startTime :: Lude.Maybe Lude.Text,
    arn :: Lude.Maybe Lude.Text,
    createdAt :: Lude.Maybe Lude.Text,
    channelId :: Lude.Maybe Lude.Text,
    s3Destination :: Lude.Maybe S3Destination,
    endTime :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateHarvestJobResponse' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) assigned to the HarvestJob.
-- * 'channelId' - The ID of the Channel that the HarvestJob will harvest from.
-- * 'createdAt' - The time the HarvestJob was submitted
-- * 'endTime' - The end of the time-window which will be harvested.
-- * 'id' - The ID of the HarvestJob. The ID must be unique within the region
--
-- and it cannot be changed after the HarvestJob is submitted.
-- * 'originEndpointId' - The ID of the OriginEndpoint that the HarvestJob will harvest from.
--
-- This cannot be changed after the HarvestJob is submitted.
-- * 'responseStatus' - The response status code.
-- * 's3Destination' - Undocumented field.
-- * 'startTime' - The start of the time-window which will be harvested.
-- * 'status' - The current status of the HarvestJob. Consider setting up a CloudWatch Event to listen for
--
-- HarvestJobs as they succeed or fail. In the event of failure, the CloudWatch Event will
-- include an explanation of why the HarvestJob failed.
mkCreateHarvestJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateHarvestJobResponse
mkCreateHarvestJobResponse pResponseStatus_ =
  CreateHarvestJobResponse'
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
chjrsStatus :: Lens.Lens' CreateHarvestJobResponse (Lude.Maybe Status)
chjrsStatus = Lens.lens (status :: CreateHarvestJobResponse -> Lude.Maybe Status) (\s a -> s {status = a} :: CreateHarvestJobResponse)
{-# DEPRECATED chjrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The ID of the OriginEndpoint that the HarvestJob will harvest from.
--
-- This cannot be changed after the HarvestJob is submitted.
--
-- /Note:/ Consider using 'originEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chjrsOriginEndpointId :: Lens.Lens' CreateHarvestJobResponse (Lude.Maybe Lude.Text)
chjrsOriginEndpointId = Lens.lens (originEndpointId :: CreateHarvestJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {originEndpointId = a} :: CreateHarvestJobResponse)
{-# DEPRECATED chjrsOriginEndpointId "Use generic-lens or generic-optics with 'originEndpointId' instead." #-}

-- | The start of the time-window which will be harvested.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chjrsStartTime :: Lens.Lens' CreateHarvestJobResponse (Lude.Maybe Lude.Text)
chjrsStartTime = Lens.lens (startTime :: CreateHarvestJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {startTime = a} :: CreateHarvestJobResponse)
{-# DEPRECATED chjrsStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The Amazon Resource Name (ARN) assigned to the HarvestJob.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chjrsARN :: Lens.Lens' CreateHarvestJobResponse (Lude.Maybe Lude.Text)
chjrsARN = Lens.lens (arn :: CreateHarvestJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: CreateHarvestJobResponse)
{-# DEPRECATED chjrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time the HarvestJob was submitted
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chjrsCreatedAt :: Lens.Lens' CreateHarvestJobResponse (Lude.Maybe Lude.Text)
chjrsCreatedAt = Lens.lens (createdAt :: CreateHarvestJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {createdAt = a} :: CreateHarvestJobResponse)
{-# DEPRECATED chjrsCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The ID of the Channel that the HarvestJob will harvest from.
--
-- /Note:/ Consider using 'channelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chjrsChannelId :: Lens.Lens' CreateHarvestJobResponse (Lude.Maybe Lude.Text)
chjrsChannelId = Lens.lens (channelId :: CreateHarvestJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {channelId = a} :: CreateHarvestJobResponse)
{-# DEPRECATED chjrsChannelId "Use generic-lens or generic-optics with 'channelId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 's3Destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chjrsS3Destination :: Lens.Lens' CreateHarvestJobResponse (Lude.Maybe S3Destination)
chjrsS3Destination = Lens.lens (s3Destination :: CreateHarvestJobResponse -> Lude.Maybe S3Destination) (\s a -> s {s3Destination = a} :: CreateHarvestJobResponse)
{-# DEPRECATED chjrsS3Destination "Use generic-lens or generic-optics with 's3Destination' instead." #-}

-- | The end of the time-window which will be harvested.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chjrsEndTime :: Lens.Lens' CreateHarvestJobResponse (Lude.Maybe Lude.Text)
chjrsEndTime = Lens.lens (endTime :: CreateHarvestJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {endTime = a} :: CreateHarvestJobResponse)
{-# DEPRECATED chjrsEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The ID of the HarvestJob. The ID must be unique within the region
--
-- and it cannot be changed after the HarvestJob is submitted.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chjrsId :: Lens.Lens' CreateHarvestJobResponse (Lude.Maybe Lude.Text)
chjrsId = Lens.lens (id :: CreateHarvestJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: CreateHarvestJobResponse)
{-# DEPRECATED chjrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chjrsResponseStatus :: Lens.Lens' CreateHarvestJobResponse Lude.Int
chjrsResponseStatus = Lens.lens (responseStatus :: CreateHarvestJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateHarvestJobResponse)
{-# DEPRECATED chjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
