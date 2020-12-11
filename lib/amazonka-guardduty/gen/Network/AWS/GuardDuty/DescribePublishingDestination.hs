{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    desDetectorId,
    desDestinationId,

    -- * Destructuring the response
    DescribePublishingDestinationResponse (..),
    mkDescribePublishingDestinationResponse,

    -- ** Response lenses
    desrsResponseStatus,
    desrsDestinationId,
    desrsDestinationType,
    desrsStatus,
    desrsPublishingFailureStartTimestamp,
    desrsDestinationProperties,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribePublishingDestination' smart constructor.
data DescribePublishingDestination = DescribePublishingDestination'
  { detectorId ::
      Lude.Text,
    destinationId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribePublishingDestination' with the minimum fields required to make a request.
--
-- * 'destinationId' - The ID of the publishing destination to retrieve.
-- * 'detectorId' - The unique ID of the detector associated with the publishing destination to retrieve.
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
desDetectorId :: Lens.Lens' DescribePublishingDestination Lude.Text
desDetectorId = Lens.lens (detectorId :: DescribePublishingDestination -> Lude.Text) (\s a -> s {detectorId = a} :: DescribePublishingDestination)
{-# DEPRECATED desDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | The ID of the publishing destination to retrieve.
--
-- /Note:/ Consider using 'destinationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desDestinationId :: Lens.Lens' DescribePublishingDestination Lude.Text
desDestinationId = Lens.lens (destinationId :: DescribePublishingDestination -> Lude.Text) (\s a -> s {destinationId = a} :: DescribePublishingDestination)
{-# DEPRECATED desDestinationId "Use generic-lens or generic-optics with 'destinationId' instead." #-}

instance Lude.AWSRequest DescribePublishingDestination where
  type
    Rs DescribePublishingDestination =
      DescribePublishingDestinationResponse
  request = Req.get guardDutyService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribePublishingDestinationResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "destinationId")
            Lude.<*> (x Lude..:> "destinationType")
            Lude.<*> (x Lude..:> "status")
            Lude.<*> (x Lude..:> "publishingFailureStartTimestamp")
            Lude.<*> (x Lude..:> "destinationProperties")
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
  { responseStatus ::
      Lude.Int,
    destinationId ::
      Lude.Text,
    destinationType ::
      DestinationType,
    status ::
      PublishingStatus,
    publishingFailureStartTimestamp ::
      Lude.Integer,
    destinationProperties ::
      DestinationProperties
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribePublishingDestinationResponse' with the minimum fields required to make a request.
--
-- * 'destinationId' - The ID of the publishing destination.
-- * 'destinationProperties' - A @DestinationProperties@ object that includes the @DestinationArn@ and @KmsKeyArn@ of the publishing destination.
-- * 'destinationType' - The type of publishing destination. Currently, only Amazon S3 buckets are supported.
-- * 'publishingFailureStartTimestamp' - The time, in epoch millisecond format, at which GuardDuty was first unable to publish findings to the destination.
-- * 'responseStatus' - The response status code.
-- * 'status' - The status of the publishing destination.
mkDescribePublishingDestinationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'destinationId'
  Lude.Text ->
  -- | 'destinationType'
  DestinationType ->
  -- | 'status'
  PublishingStatus ->
  -- | 'publishingFailureStartTimestamp'
  Lude.Integer ->
  -- | 'destinationProperties'
  DestinationProperties ->
  DescribePublishingDestinationResponse
mkDescribePublishingDestinationResponse
  pResponseStatus_
  pDestinationId_
  pDestinationType_
  pStatus_
  pPublishingFailureStartTimestamp_
  pDestinationProperties_ =
    DescribePublishingDestinationResponse'
      { responseStatus =
          pResponseStatus_,
        destinationId = pDestinationId_,
        destinationType = pDestinationType_,
        status = pStatus_,
        publishingFailureStartTimestamp =
          pPublishingFailureStartTimestamp_,
        destinationProperties = pDestinationProperties_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsResponseStatus :: Lens.Lens' DescribePublishingDestinationResponse Lude.Int
desrsResponseStatus = Lens.lens (responseStatus :: DescribePublishingDestinationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribePublishingDestinationResponse)
{-# DEPRECATED desrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The ID of the publishing destination.
--
-- /Note:/ Consider using 'destinationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsDestinationId :: Lens.Lens' DescribePublishingDestinationResponse Lude.Text
desrsDestinationId = Lens.lens (destinationId :: DescribePublishingDestinationResponse -> Lude.Text) (\s a -> s {destinationId = a} :: DescribePublishingDestinationResponse)
{-# DEPRECATED desrsDestinationId "Use generic-lens or generic-optics with 'destinationId' instead." #-}

-- | The type of publishing destination. Currently, only Amazon S3 buckets are supported.
--
-- /Note:/ Consider using 'destinationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsDestinationType :: Lens.Lens' DescribePublishingDestinationResponse DestinationType
desrsDestinationType = Lens.lens (destinationType :: DescribePublishingDestinationResponse -> DestinationType) (\s a -> s {destinationType = a} :: DescribePublishingDestinationResponse)
{-# DEPRECATED desrsDestinationType "Use generic-lens or generic-optics with 'destinationType' instead." #-}

-- | The status of the publishing destination.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsStatus :: Lens.Lens' DescribePublishingDestinationResponse PublishingStatus
desrsStatus = Lens.lens (status :: DescribePublishingDestinationResponse -> PublishingStatus) (\s a -> s {status = a} :: DescribePublishingDestinationResponse)
{-# DEPRECATED desrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The time, in epoch millisecond format, at which GuardDuty was first unable to publish findings to the destination.
--
-- /Note:/ Consider using 'publishingFailureStartTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsPublishingFailureStartTimestamp :: Lens.Lens' DescribePublishingDestinationResponse Lude.Integer
desrsPublishingFailureStartTimestamp = Lens.lens (publishingFailureStartTimestamp :: DescribePublishingDestinationResponse -> Lude.Integer) (\s a -> s {publishingFailureStartTimestamp = a} :: DescribePublishingDestinationResponse)
{-# DEPRECATED desrsPublishingFailureStartTimestamp "Use generic-lens or generic-optics with 'publishingFailureStartTimestamp' instead." #-}

-- | A @DestinationProperties@ object that includes the @DestinationArn@ and @KmsKeyArn@ of the publishing destination.
--
-- /Note:/ Consider using 'destinationProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsDestinationProperties :: Lens.Lens' DescribePublishingDestinationResponse DestinationProperties
desrsDestinationProperties = Lens.lens (destinationProperties :: DescribePublishingDestinationResponse -> DestinationProperties) (\s a -> s {destinationProperties = a} :: DescribePublishingDestinationResponse)
{-# DEPRECATED desrsDestinationProperties "Use generic-lens or generic-optics with 'destinationProperties' instead." #-}
