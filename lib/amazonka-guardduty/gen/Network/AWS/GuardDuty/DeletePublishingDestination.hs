{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.DeletePublishingDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the publishing definition with the specified @destinationId@ .
module Network.AWS.GuardDuty.DeletePublishingDestination
  ( -- * Creating a request
    DeletePublishingDestination (..),
    mkDeletePublishingDestination,

    -- ** Request lenses
    dpdDetectorId,
    dpdDestinationId,

    -- * Destructuring the response
    DeletePublishingDestinationResponse (..),
    mkDeletePublishingDestinationResponse,

    -- ** Response lenses
    dpdrsResponseStatus,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeletePublishingDestination' smart constructor.
data DeletePublishingDestination = DeletePublishingDestination'
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

-- | Creates a value of 'DeletePublishingDestination' with the minimum fields required to make a request.
--
-- * 'destinationId' - The ID of the publishing destination to delete.
-- * 'detectorId' - The unique ID of the detector associated with the publishing destination to delete.
mkDeletePublishingDestination ::
  -- | 'detectorId'
  Lude.Text ->
  -- | 'destinationId'
  Lude.Text ->
  DeletePublishingDestination
mkDeletePublishingDestination pDetectorId_ pDestinationId_ =
  DeletePublishingDestination'
    { detectorId = pDetectorId_,
      destinationId = pDestinationId_
    }

-- | The unique ID of the detector associated with the publishing destination to delete.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpdDetectorId :: Lens.Lens' DeletePublishingDestination Lude.Text
dpdDetectorId = Lens.lens (detectorId :: DeletePublishingDestination -> Lude.Text) (\s a -> s {detectorId = a} :: DeletePublishingDestination)
{-# DEPRECATED dpdDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | The ID of the publishing destination to delete.
--
-- /Note:/ Consider using 'destinationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpdDestinationId :: Lens.Lens' DeletePublishingDestination Lude.Text
dpdDestinationId = Lens.lens (destinationId :: DeletePublishingDestination -> Lude.Text) (\s a -> s {destinationId = a} :: DeletePublishingDestination)
{-# DEPRECATED dpdDestinationId "Use generic-lens or generic-optics with 'destinationId' instead." #-}

instance Lude.AWSRequest DeletePublishingDestination where
  type
    Rs DeletePublishingDestination =
      DeletePublishingDestinationResponse
  request = Req.delete guardDutyService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeletePublishingDestinationResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeletePublishingDestination where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeletePublishingDestination where
  toPath DeletePublishingDestination' {..} =
    Lude.mconcat
      [ "/detector/",
        Lude.toBS detectorId,
        "/publishingDestination/",
        Lude.toBS destinationId
      ]

instance Lude.ToQuery DeletePublishingDestination where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeletePublishingDestinationResponse' smart constructor.
newtype DeletePublishingDestinationResponse = DeletePublishingDestinationResponse'
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

-- | Creates a value of 'DeletePublishingDestinationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeletePublishingDestinationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeletePublishingDestinationResponse
mkDeletePublishingDestinationResponse pResponseStatus_ =
  DeletePublishingDestinationResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpdrsResponseStatus :: Lens.Lens' DeletePublishingDestinationResponse Lude.Int
dpdrsResponseStatus = Lens.lens (responseStatus :: DeletePublishingDestinationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeletePublishingDestinationResponse)
{-# DEPRECATED dpdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
