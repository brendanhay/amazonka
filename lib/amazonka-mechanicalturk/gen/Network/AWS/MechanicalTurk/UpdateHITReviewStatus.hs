{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.UpdateHITReviewStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @UpdateHITReviewStatus@ operation updates the status of a HIT. If the status is Reviewable, this operation can update the status to Reviewing, or it can revert a Reviewing HIT back to the Reviewable status.
module Network.AWS.MechanicalTurk.UpdateHITReviewStatus
  ( -- * Creating a request
    UpdateHITReviewStatus (..),
    mkUpdateHITReviewStatus,

    -- ** Request lenses
    uhitrsRevert,
    uhitrsHITId,

    -- * Destructuring the response
    UpdateHITReviewStatusResponse (..),
    mkUpdateHITReviewStatusResponse,

    -- ** Response lenses
    uhitrsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateHITReviewStatus' smart constructor.
data UpdateHITReviewStatus = UpdateHITReviewStatus'
  { revert ::
      Lude.Maybe Lude.Bool,
    hITId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateHITReviewStatus' with the minimum fields required to make a request.
--
-- * 'hITId' - The ID of the HIT to update.
-- * 'revert' - Specifies how to update the HIT status. Default is @False@ .
--
--
--     * Setting this to false will only transition a HIT from @Reviewable@ to @Reviewing@
--
--
--     * Setting this to true will only transition a HIT from @Reviewing@ to @Reviewable@
mkUpdateHITReviewStatus ::
  -- | 'hITId'
  Lude.Text ->
  UpdateHITReviewStatus
mkUpdateHITReviewStatus pHITId_ =
  UpdateHITReviewStatus' {revert = Lude.Nothing, hITId = pHITId_}

-- | Specifies how to update the HIT status. Default is @False@ .
--
--
--     * Setting this to false will only transition a HIT from @Reviewable@ to @Reviewing@
--
--
--     * Setting this to true will only transition a HIT from @Reviewing@ to @Reviewable@
--
--
--
-- /Note:/ Consider using 'revert' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhitrsRevert :: Lens.Lens' UpdateHITReviewStatus (Lude.Maybe Lude.Bool)
uhitrsRevert = Lens.lens (revert :: UpdateHITReviewStatus -> Lude.Maybe Lude.Bool) (\s a -> s {revert = a} :: UpdateHITReviewStatus)
{-# DEPRECATED uhitrsRevert "Use generic-lens or generic-optics with 'revert' instead." #-}

-- | The ID of the HIT to update.
--
-- /Note:/ Consider using 'hITId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhitrsHITId :: Lens.Lens' UpdateHITReviewStatus Lude.Text
uhitrsHITId = Lens.lens (hITId :: UpdateHITReviewStatus -> Lude.Text) (\s a -> s {hITId = a} :: UpdateHITReviewStatus)
{-# DEPRECATED uhitrsHITId "Use generic-lens or generic-optics with 'hITId' instead." #-}

instance Lude.AWSRequest UpdateHITReviewStatus where
  type Rs UpdateHITReviewStatus = UpdateHITReviewStatusResponse
  request = Req.postJSON mechanicalTurkService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateHITReviewStatusResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateHITReviewStatus where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "MTurkRequesterServiceV20170117.UpdateHITReviewStatus" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateHITReviewStatus where
  toJSON UpdateHITReviewStatus' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Revert" Lude..=) Lude.<$> revert,
            Lude.Just ("HITId" Lude..= hITId)
          ]
      )

instance Lude.ToPath UpdateHITReviewStatus where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateHITReviewStatus where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateHITReviewStatusResponse' smart constructor.
newtype UpdateHITReviewStatusResponse = UpdateHITReviewStatusResponse'
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

-- | Creates a value of 'UpdateHITReviewStatusResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateHITReviewStatusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateHITReviewStatusResponse
mkUpdateHITReviewStatusResponse pResponseStatus_ =
  UpdateHITReviewStatusResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhitrsrsResponseStatus :: Lens.Lens' UpdateHITReviewStatusResponse Lude.Int
uhitrsrsResponseStatus = Lens.lens (responseStatus :: UpdateHITReviewStatusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateHITReviewStatusResponse)
{-# DEPRECATED uhitrsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
