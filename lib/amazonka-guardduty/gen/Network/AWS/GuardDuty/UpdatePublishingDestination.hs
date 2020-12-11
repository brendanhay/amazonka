{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.UpdatePublishingDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates information about the publishing destination specified by the @destinationId@ .
module Network.AWS.GuardDuty.UpdatePublishingDestination
  ( -- * Creating a request
    UpdatePublishingDestination (..),
    mkUpdatePublishingDestination,

    -- ** Request lenses
    updDestinationProperties,
    updDetectorId,
    updDestinationId,

    -- * Destructuring the response
    UpdatePublishingDestinationResponse (..),
    mkUpdatePublishingDestinationResponse,

    -- ** Response lenses
    updrsResponseStatus,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdatePublishingDestination' smart constructor.
data UpdatePublishingDestination = UpdatePublishingDestination'
  { destinationProperties ::
      Lude.Maybe DestinationProperties,
    detectorId :: Lude.Text,
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

-- | Creates a value of 'UpdatePublishingDestination' with the minimum fields required to make a request.
--
-- * 'destinationId' - The ID of the publishing destination to update.
-- * 'destinationProperties' - A @DestinationProperties@ object that includes the @DestinationArn@ and @KmsKeyArn@ of the publishing destination.
-- * 'detectorId' - The ID of the detector associated with the publishing destinations to update.
mkUpdatePublishingDestination ::
  -- | 'detectorId'
  Lude.Text ->
  -- | 'destinationId'
  Lude.Text ->
  UpdatePublishingDestination
mkUpdatePublishingDestination pDetectorId_ pDestinationId_ =
  UpdatePublishingDestination'
    { destinationProperties =
        Lude.Nothing,
      detectorId = pDetectorId_,
      destinationId = pDestinationId_
    }

-- | A @DestinationProperties@ object that includes the @DestinationArn@ and @KmsKeyArn@ of the publishing destination.
--
-- /Note:/ Consider using 'destinationProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updDestinationProperties :: Lens.Lens' UpdatePublishingDestination (Lude.Maybe DestinationProperties)
updDestinationProperties = Lens.lens (destinationProperties :: UpdatePublishingDestination -> Lude.Maybe DestinationProperties) (\s a -> s {destinationProperties = a} :: UpdatePublishingDestination)
{-# DEPRECATED updDestinationProperties "Use generic-lens or generic-optics with 'destinationProperties' instead." #-}

-- | The ID of the detector associated with the publishing destinations to update.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updDetectorId :: Lens.Lens' UpdatePublishingDestination Lude.Text
updDetectorId = Lens.lens (detectorId :: UpdatePublishingDestination -> Lude.Text) (\s a -> s {detectorId = a} :: UpdatePublishingDestination)
{-# DEPRECATED updDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | The ID of the publishing destination to update.
--
-- /Note:/ Consider using 'destinationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updDestinationId :: Lens.Lens' UpdatePublishingDestination Lude.Text
updDestinationId = Lens.lens (destinationId :: UpdatePublishingDestination -> Lude.Text) (\s a -> s {destinationId = a} :: UpdatePublishingDestination)
{-# DEPRECATED updDestinationId "Use generic-lens or generic-optics with 'destinationId' instead." #-}

instance Lude.AWSRequest UpdatePublishingDestination where
  type
    Rs UpdatePublishingDestination =
      UpdatePublishingDestinationResponse
  request = Req.postJSON guardDutyService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdatePublishingDestinationResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdatePublishingDestination where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdatePublishingDestination where
  toJSON UpdatePublishingDestination' {..} =
    Lude.object
      ( Lude.catMaybes
          [("destinationProperties" Lude..=) Lude.<$> destinationProperties]
      )

instance Lude.ToPath UpdatePublishingDestination where
  toPath UpdatePublishingDestination' {..} =
    Lude.mconcat
      [ "/detector/",
        Lude.toBS detectorId,
        "/publishingDestination/",
        Lude.toBS destinationId
      ]

instance Lude.ToQuery UpdatePublishingDestination where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdatePublishingDestinationResponse' smart constructor.
newtype UpdatePublishingDestinationResponse = UpdatePublishingDestinationResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdatePublishingDestinationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdatePublishingDestinationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdatePublishingDestinationResponse
mkUpdatePublishingDestinationResponse pResponseStatus_ =
  UpdatePublishingDestinationResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updrsResponseStatus :: Lens.Lens' UpdatePublishingDestinationResponse Lude.Int
updrsResponseStatus = Lens.lens (responseStatus :: UpdatePublishingDestinationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdatePublishingDestinationResponse)
{-# DEPRECATED updrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
