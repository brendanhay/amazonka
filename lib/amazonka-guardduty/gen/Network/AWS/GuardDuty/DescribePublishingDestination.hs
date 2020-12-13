{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.DescribePublishingDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the publishing destination specified by the provided @destinationId@ .
module Network.AWS.GuardDuty.DescribePublishingDestination
  ( -- * Creating a request
    DescribePublishingDestination (..),
    mkDescribePublishingDestination,

    -- ** Request lenses
    dpdfDetectorId,
    dpdfDestinationId,

    -- * Destructuring the response
    DescribePublishingDestinationResponse (..),
    mkDescribePublishingDestinationResponse,

    -- ** Response lenses
    dpdfrsStatus,
    dpdfrsPublishingFailureStartTimestamp,
    dpdfrsDestinationType,
    dpdfrsDestinationProperties,
    dpdfrsDestinationId,
    dpdfrsResponseStatus,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribePublishingDestination' smart constructor.
data DescribePublishingDestination = DescribePublishingDestination'
  { -- | The unique ID of the detector associated with the publishing destination to retrieve.
    detectorId :: Lude.Text,
    -- | The ID of the publishing destination to retrieve.
    destinationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribePublishingDestination' with the minimum fields required to make a request.
--
-- * 'detectorId' - The unique ID of the detector associated with the publishing destination to retrieve.
-- * 'destinationId' - The ID of the publishing destination to retrieve.
mkDescribePublishingDestination ::
  -- | 'detectorId'
  Lude.Text ->
  -- | 'destinationId'
  Lude.Text ->
  DescribePublishingDestination
mkDescribePublishingDestination pDetectorId_ pDestinationId_ =
  DescribePublishingDestination'
    { detectorId = pDetectorId_,
      destinationId = pDestinationId_
    }

-- | The unique ID of the detector associated with the publishing destination to retrieve.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpdfDetectorId :: Lens.Lens' DescribePublishingDestination Lude.Text
dpdfDetectorId = Lens.lens (detectorId :: DescribePublishingDestination -> Lude.Text) (\s a -> s {detectorId = a} :: DescribePublishingDestination)
{-# DEPRECATED dpdfDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | The ID of the publishing destination to retrieve.
--
-- /Note:/ Consider using 'destinationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpdfDestinationId :: Lens.Lens' DescribePublishingDestination Lude.Text
dpdfDestinationId = Lens.lens (destinationId :: DescribePublishingDestination -> Lude.Text) (\s a -> s {destinationId = a} :: DescribePublishingDestination)
{-# DEPRECATED dpdfDestinationId "Use generic-lens or generic-optics with 'destinationId' instead." #-}

instance Lude.AWSRequest DescribePublishingDestination where
  type
    Rs DescribePublishingDestination =
      DescribePublishingDestinationResponse
  request = Req.get guardDutyService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribePublishingDestinationResponse'
            Lude.<$> (x Lude..:> "status")
            Lude.<*> (x Lude..:> "publishingFailureStartTimestamp")
            Lude.<*> (x Lude..:> "destinationType")
            Lude.<*> (x Lude..:> "destinationProperties")
            Lude.<*> (x Lude..:> "destinationId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribePublishingDestination where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DescribePublishingDestination where
  toPath DescribePublishingDestination' {..} =
    Lude.mconcat
      [ "/detector/",
        Lude.toBS detectorId,
        "/publishingDestination/",
        Lude.toBS destinationId
      ]

instance Lude.ToQuery DescribePublishingDestination where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribePublishingDestinationResponse' smart constructor.
data DescribePublishingDestinationResponse = DescribePublishingDestinationResponse'
  { -- | The status of the publishing destination.
    status :: PublishingStatus,
    -- | The time, in epoch millisecond format, at which GuardDuty was first unable to publish findings to the destination.
    publishingFailureStartTimestamp :: Lude.Integer,
    -- | The type of publishing destination. Currently, only Amazon S3 buckets are supported.
    destinationType :: DestinationType,
    -- | A @DestinationProperties@ object that includes the @DestinationArn@ and @KmsKeyArn@ of the publishing destination.
    destinationProperties :: DestinationProperties,
    -- | The ID of the publishing destination.
    destinationId :: Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribePublishingDestinationResponse' with the minimum fields required to make a request.
--
-- * 'status' - The status of the publishing destination.
-- * 'publishingFailureStartTimestamp' - The time, in epoch millisecond format, at which GuardDuty was first unable to publish findings to the destination.
-- * 'destinationType' - The type of publishing destination. Currently, only Amazon S3 buckets are supported.
-- * 'destinationProperties' - A @DestinationProperties@ object that includes the @DestinationArn@ and @KmsKeyArn@ of the publishing destination.
-- * 'destinationId' - The ID of the publishing destination.
-- * 'responseStatus' - The response status code.
mkDescribePublishingDestinationResponse ::
  -- | 'status'
  PublishingStatus ->
  -- | 'publishingFailureStartTimestamp'
  Lude.Integer ->
  -- | 'destinationType'
  DestinationType ->
  -- | 'destinationProperties'
  DestinationProperties ->
  -- | 'destinationId'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  DescribePublishingDestinationResponse
mkDescribePublishingDestinationResponse
  pStatus_
  pPublishingFailureStartTimestamp_
  pDestinationType_
  pDestinationProperties_
  pDestinationId_
  pResponseStatus_ =
    DescribePublishingDestinationResponse'
      { status = pStatus_,
        publishingFailureStartTimestamp =
          pPublishingFailureStartTimestamp_,
        destinationType = pDestinationType_,
        destinationProperties = pDestinationProperties_,
        destinationId = pDestinationId_,
        responseStatus = pResponseStatus_
      }

-- | The status of the publishing destination.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpdfrsStatus :: Lens.Lens' DescribePublishingDestinationResponse PublishingStatus
dpdfrsStatus = Lens.lens (status :: DescribePublishingDestinationResponse -> PublishingStatus) (\s a -> s {status = a} :: DescribePublishingDestinationResponse)
{-# DEPRECATED dpdfrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The time, in epoch millisecond format, at which GuardDuty was first unable to publish findings to the destination.
--
-- /Note:/ Consider using 'publishingFailureStartTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpdfrsPublishingFailureStartTimestamp :: Lens.Lens' DescribePublishingDestinationResponse Lude.Integer
dpdfrsPublishingFailureStartTimestamp = Lens.lens (publishingFailureStartTimestamp :: DescribePublishingDestinationResponse -> Lude.Integer) (\s a -> s {publishingFailureStartTimestamp = a} :: DescribePublishingDestinationResponse)
{-# DEPRECATED dpdfrsPublishingFailureStartTimestamp "Use generic-lens or generic-optics with 'publishingFailureStartTimestamp' instead." #-}

-- | The type of publishing destination. Currently, only Amazon S3 buckets are supported.
--
-- /Note:/ Consider using 'destinationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpdfrsDestinationType :: Lens.Lens' DescribePublishingDestinationResponse DestinationType
dpdfrsDestinationType = Lens.lens (destinationType :: DescribePublishingDestinationResponse -> DestinationType) (\s a -> s {destinationType = a} :: DescribePublishingDestinationResponse)
{-# DEPRECATED dpdfrsDestinationType "Use generic-lens or generic-optics with 'destinationType' instead." #-}

-- | A @DestinationProperties@ object that includes the @DestinationArn@ and @KmsKeyArn@ of the publishing destination.
--
-- /Note:/ Consider using 'destinationProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpdfrsDestinationProperties :: Lens.Lens' DescribePublishingDestinationResponse DestinationProperties
dpdfrsDestinationProperties = Lens.lens (destinationProperties :: DescribePublishingDestinationResponse -> DestinationProperties) (\s a -> s {destinationProperties = a} :: DescribePublishingDestinationResponse)
{-# DEPRECATED dpdfrsDestinationProperties "Use generic-lens or generic-optics with 'destinationProperties' instead." #-}

-- | The ID of the publishing destination.
--
-- /Note:/ Consider using 'destinationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpdfrsDestinationId :: Lens.Lens' DescribePublishingDestinationResponse Lude.Text
dpdfrsDestinationId = Lens.lens (destinationId :: DescribePublishingDestinationResponse -> Lude.Text) (\s a -> s {destinationId = a} :: DescribePublishingDestinationResponse)
{-# DEPRECATED dpdfrsDestinationId "Use generic-lens or generic-optics with 'destinationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpdfrsResponseStatus :: Lens.Lens' DescribePublishingDestinationResponse Lude.Int
dpdfrsResponseStatus = Lens.lens (responseStatus :: DescribePublishingDestinationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribePublishingDestinationResponse)
{-# DEPRECATED dpdfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
