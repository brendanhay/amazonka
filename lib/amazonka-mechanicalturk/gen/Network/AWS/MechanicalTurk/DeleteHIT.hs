{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.DeleteHIT
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @DeleteHIT@ operation is used to delete HIT that is no longer needed. Only the Requester who created the HIT can delete it.
--
-- You can only dispose of HITs that are in the @Reviewable@ state, with all of their submitted assignments already either approved or rejected. If you call the DeleteHIT operation on a HIT that is not in the @Reviewable@ state (for example, that has not expired, or still has active assignments), or on a HIT that is Reviewable but without all of its submitted assignments already approved or rejected, the service will return an error.
module Network.AWS.MechanicalTurk.DeleteHIT
  ( -- * Creating a request
    DeleteHIT (..),
    mkDeleteHIT,

    -- ** Request lenses
    dhitHITId,

    -- * Destructuring the response
    DeleteHITResponse (..),
    mkDeleteHITResponse,

    -- ** Response lenses
    dhitrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteHIT' smart constructor.
newtype DeleteHIT = DeleteHIT' {hITId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteHIT' with the minimum fields required to make a request.
--
-- * 'hITId' - The ID of the HIT to be deleted.
mkDeleteHIT ::
  -- | 'hITId'
  Lude.Text ->
  DeleteHIT
mkDeleteHIT pHITId_ = DeleteHIT' {hITId = pHITId_}

-- | The ID of the HIT to be deleted.
--
-- /Note:/ Consider using 'hITId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhitHITId :: Lens.Lens' DeleteHIT Lude.Text
dhitHITId = Lens.lens (hITId :: DeleteHIT -> Lude.Text) (\s a -> s {hITId = a} :: DeleteHIT)
{-# DEPRECATED dhitHITId "Use generic-lens or generic-optics with 'hITId' instead." #-}

instance Lude.AWSRequest DeleteHIT where
  type Rs DeleteHIT = DeleteHITResponse
  request = Req.postJSON mechanicalTurkService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteHITResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteHIT where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("MTurkRequesterServiceV20170117.DeleteHIT" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteHIT where
  toJSON DeleteHIT' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("HITId" Lude..= hITId)])

instance Lude.ToPath DeleteHIT where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteHIT where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteHITResponse' smart constructor.
newtype DeleteHITResponse = DeleteHITResponse'
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

-- | Creates a value of 'DeleteHITResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteHITResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteHITResponse
mkDeleteHITResponse pResponseStatus_ =
  DeleteHITResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhitrsResponseStatus :: Lens.Lens' DeleteHITResponse Lude.Int
dhitrsResponseStatus = Lens.lens (responseStatus :: DeleteHITResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteHITResponse)
{-# DEPRECATED dhitrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
