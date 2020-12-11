{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.GetHIT
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @GetHIT@ operation retrieves the details of the specified HIT.
module Network.AWS.MechanicalTurk.GetHIT
  ( -- * Creating a request
    GetHIT (..),
    mkGetHIT,

    -- ** Request lenses
    ghitHITId,

    -- * Destructuring the response
    GetHITResponse (..),
    mkGetHITResponse,

    -- ** Response lenses
    ghitrsHIT,
    ghitrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetHIT' smart constructor.
newtype GetHIT = GetHIT' {hITId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetHIT' with the minimum fields required to make a request.
--
-- * 'hITId' - The ID of the HIT to be retrieved.
mkGetHIT ::
  -- | 'hITId'
  Lude.Text ->
  GetHIT
mkGetHIT pHITId_ = GetHIT' {hITId = pHITId_}

-- | The ID of the HIT to be retrieved.
--
-- /Note:/ Consider using 'hITId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghitHITId :: Lens.Lens' GetHIT Lude.Text
ghitHITId = Lens.lens (hITId :: GetHIT -> Lude.Text) (\s a -> s {hITId = a} :: GetHIT)
{-# DEPRECATED ghitHITId "Use generic-lens or generic-optics with 'hITId' instead." #-}

instance Lude.AWSRequest GetHIT where
  type Rs GetHIT = GetHITResponse
  request = Req.postJSON mechanicalTurkService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetHITResponse'
            Lude.<$> (x Lude..?> "HIT") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetHIT where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("MTurkRequesterServiceV20170117.GetHIT" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetHIT where
  toJSON GetHIT' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("HITId" Lude..= hITId)])

instance Lude.ToPath GetHIT where
  toPath = Lude.const "/"

instance Lude.ToQuery GetHIT where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetHITResponse' smart constructor.
data GetHITResponse = GetHITResponse'
  { hIT :: Lude.Maybe HIT,
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

-- | Creates a value of 'GetHITResponse' with the minimum fields required to make a request.
--
-- * 'hIT' - Contains the requested HIT data.
-- * 'responseStatus' - The response status code.
mkGetHITResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetHITResponse
mkGetHITResponse pResponseStatus_ =
  GetHITResponse'
    { hIT = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Contains the requested HIT data.
--
-- /Note:/ Consider using 'hIT' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghitrsHIT :: Lens.Lens' GetHITResponse (Lude.Maybe HIT)
ghitrsHIT = Lens.lens (hIT :: GetHITResponse -> Lude.Maybe HIT) (\s a -> s {hIT = a} :: GetHITResponse)
{-# DEPRECATED ghitrsHIT "Use generic-lens or generic-optics with 'hIT' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghitrsResponseStatus :: Lens.Lens' GetHITResponse Lude.Int
ghitrsResponseStatus = Lens.lens (responseStatus :: GetHITResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetHITResponse)
{-# DEPRECATED ghitrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
