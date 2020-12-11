{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.UpdateExpirationForHIT
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @UpdateExpirationForHIT@ operation allows you update the expiration time of a HIT. If you update it to a time in the past, the HIT will be immediately expired.
module Network.AWS.MechanicalTurk.UpdateExpirationForHIT
  ( -- * Creating a request
    UpdateExpirationForHIT (..),
    mkUpdateExpirationForHIT,

    -- ** Request lenses
    uefhitHITId,
    uefhitExpireAt,

    -- * Destructuring the response
    UpdateExpirationForHITResponse (..),
    mkUpdateExpirationForHITResponse,

    -- ** Response lenses
    uefhitrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateExpirationForHIT' smart constructor.
data UpdateExpirationForHIT = UpdateExpirationForHIT'
  { hITId ::
      Lude.Text,
    expireAt :: Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateExpirationForHIT' with the minimum fields required to make a request.
--
-- * 'expireAt' - The date and time at which you want the HIT to expire
-- * 'hITId' - The HIT to update.
mkUpdateExpirationForHIT ::
  -- | 'hITId'
  Lude.Text ->
  -- | 'expireAt'
  Lude.Timestamp ->
  UpdateExpirationForHIT
mkUpdateExpirationForHIT pHITId_ pExpireAt_ =
  UpdateExpirationForHIT' {hITId = pHITId_, expireAt = pExpireAt_}

-- | The HIT to update.
--
-- /Note:/ Consider using 'hITId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uefhitHITId :: Lens.Lens' UpdateExpirationForHIT Lude.Text
uefhitHITId = Lens.lens (hITId :: UpdateExpirationForHIT -> Lude.Text) (\s a -> s {hITId = a} :: UpdateExpirationForHIT)
{-# DEPRECATED uefhitHITId "Use generic-lens or generic-optics with 'hITId' instead." #-}

-- | The date and time at which you want the HIT to expire
--
-- /Note:/ Consider using 'expireAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uefhitExpireAt :: Lens.Lens' UpdateExpirationForHIT Lude.Timestamp
uefhitExpireAt = Lens.lens (expireAt :: UpdateExpirationForHIT -> Lude.Timestamp) (\s a -> s {expireAt = a} :: UpdateExpirationForHIT)
{-# DEPRECATED uefhitExpireAt "Use generic-lens or generic-optics with 'expireAt' instead." #-}

instance Lude.AWSRequest UpdateExpirationForHIT where
  type Rs UpdateExpirationForHIT = UpdateExpirationForHITResponse
  request = Req.postJSON mechanicalTurkService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateExpirationForHITResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateExpirationForHIT where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "MTurkRequesterServiceV20170117.UpdateExpirationForHIT" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateExpirationForHIT where
  toJSON UpdateExpirationForHIT' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("HITId" Lude..= hITId),
            Lude.Just ("ExpireAt" Lude..= expireAt)
          ]
      )

instance Lude.ToPath UpdateExpirationForHIT where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateExpirationForHIT where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateExpirationForHITResponse' smart constructor.
newtype UpdateExpirationForHITResponse = UpdateExpirationForHITResponse'
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

-- | Creates a value of 'UpdateExpirationForHITResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateExpirationForHITResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateExpirationForHITResponse
mkUpdateExpirationForHITResponse pResponseStatus_ =
  UpdateExpirationForHITResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uefhitrsResponseStatus :: Lens.Lens' UpdateExpirationForHITResponse Lude.Int
uefhitrsResponseStatus = Lens.lens (responseStatus :: UpdateExpirationForHITResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateExpirationForHITResponse)
{-# DEPRECATED uefhitrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
