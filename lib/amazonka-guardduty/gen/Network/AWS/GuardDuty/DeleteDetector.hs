{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.DeleteDetector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon GuardDuty detector that is specified by the detector ID.
module Network.AWS.GuardDuty.DeleteDetector
  ( -- * Creating a request
    DeleteDetector (..),
    mkDeleteDetector,

    -- ** Request lenses
    ddDetectorId,

    -- * Destructuring the response
    DeleteDetectorResponse (..),
    mkDeleteDetectorResponse,

    -- ** Response lenses
    ddrsResponseStatus,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteDetector' smart constructor.
newtype DeleteDetector = DeleteDetector'
  { -- | The unique ID of the detector that you want to delete.
    detectorId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDetector' with the minimum fields required to make a request.
--
-- * 'detectorId' - The unique ID of the detector that you want to delete.
mkDeleteDetector ::
  -- | 'detectorId'
  Lude.Text ->
  DeleteDetector
mkDeleteDetector pDetectorId_ =
  DeleteDetector' {detectorId = pDetectorId_}

-- | The unique ID of the detector that you want to delete.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDetectorId :: Lens.Lens' DeleteDetector Lude.Text
ddDetectorId = Lens.lens (detectorId :: DeleteDetector -> Lude.Text) (\s a -> s {detectorId = a} :: DeleteDetector)
{-# DEPRECATED ddDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

instance Lude.AWSRequest DeleteDetector where
  type Rs DeleteDetector = DeleteDetectorResponse
  request = Req.delete guardDutyService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteDetectorResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteDetector where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteDetector where
  toPath DeleteDetector' {..} =
    Lude.mconcat ["/detector/", Lude.toBS detectorId]

instance Lude.ToQuery DeleteDetector where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteDetectorResponse' smart constructor.
newtype DeleteDetectorResponse = DeleteDetectorResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDetectorResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteDetectorResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteDetectorResponse
mkDeleteDetectorResponse pResponseStatus_ =
  DeleteDetectorResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrsResponseStatus :: Lens.Lens' DeleteDetectorResponse Lude.Int
ddrsResponseStatus = Lens.lens (responseStatus :: DeleteDetectorResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteDetectorResponse)
{-# DEPRECATED ddrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
