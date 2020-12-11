{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.GetInvitationsCount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the count of all GuardDuty membership invitations that were sent to the current member account except the currently accepted invitation.
module Network.AWS.GuardDuty.GetInvitationsCount
  ( -- * Creating a request
    GetInvitationsCount (..),
    mkGetInvitationsCount,

    -- * Destructuring the response
    GetInvitationsCountResponse (..),
    mkGetInvitationsCountResponse,

    -- ** Response lenses
    gicrsInvitationsCount,
    gicrsResponseStatus,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetInvitationsCount' smart constructor.
data GetInvitationsCount = GetInvitationsCount'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetInvitationsCount' with the minimum fields required to make a request.
mkGetInvitationsCount ::
  GetInvitationsCount
mkGetInvitationsCount = GetInvitationsCount'

instance Lude.AWSRequest GetInvitationsCount where
  type Rs GetInvitationsCount = GetInvitationsCountResponse
  request = Req.get guardDutyService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetInvitationsCountResponse'
            Lude.<$> (x Lude..?> "invitationsCount")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetInvitationsCount where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetInvitationsCount where
  toPath = Lude.const "/invitation/count"

instance Lude.ToQuery GetInvitationsCount where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetInvitationsCountResponse' smart constructor.
data GetInvitationsCountResponse = GetInvitationsCountResponse'
  { invitationsCount ::
      Lude.Maybe Lude.Int,
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

-- | Creates a value of 'GetInvitationsCountResponse' with the minimum fields required to make a request.
--
-- * 'invitationsCount' - The number of received invitations.
-- * 'responseStatus' - The response status code.
mkGetInvitationsCountResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetInvitationsCountResponse
mkGetInvitationsCountResponse pResponseStatus_ =
  GetInvitationsCountResponse'
    { invitationsCount = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The number of received invitations.
--
-- /Note:/ Consider using 'invitationsCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gicrsInvitationsCount :: Lens.Lens' GetInvitationsCountResponse (Lude.Maybe Lude.Int)
gicrsInvitationsCount = Lens.lens (invitationsCount :: GetInvitationsCountResponse -> Lude.Maybe Lude.Int) (\s a -> s {invitationsCount = a} :: GetInvitationsCountResponse)
{-# DEPRECATED gicrsInvitationsCount "Use generic-lens or generic-optics with 'invitationsCount' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gicrsResponseStatus :: Lens.Lens' GetInvitationsCountResponse Lude.Int
gicrsResponseStatus = Lens.lens (responseStatus :: GetInvitationsCountResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetInvitationsCountResponse)
{-# DEPRECATED gicrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
