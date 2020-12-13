{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.AcceptInvitation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts the invitation to be monitored by a master GuardDuty account.
module Network.AWS.GuardDuty.AcceptInvitation
  ( -- * Creating a request
    AcceptInvitation (..),
    mkAcceptInvitation,

    -- ** Request lenses
    aiMasterId,
    aiInvitationId,
    aiDetectorId,

    -- * Destructuring the response
    AcceptInvitationResponse (..),
    mkAcceptInvitationResponse,

    -- ** Response lenses
    airsResponseStatus,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAcceptInvitation' smart constructor.
data AcceptInvitation = AcceptInvitation'
  { -- | The account ID of the master GuardDuty account whose invitation you're accepting.
    masterId :: Lude.Text,
    -- | The value that is used to validate the master account to the member account.
    invitationId :: Lude.Text,
    -- | The unique ID of the detector of the GuardDuty member account.
    detectorId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AcceptInvitation' with the minimum fields required to make a request.
--
-- * 'masterId' - The account ID of the master GuardDuty account whose invitation you're accepting.
-- * 'invitationId' - The value that is used to validate the master account to the member account.
-- * 'detectorId' - The unique ID of the detector of the GuardDuty member account.
mkAcceptInvitation ::
  -- | 'masterId'
  Lude.Text ->
  -- | 'invitationId'
  Lude.Text ->
  -- | 'detectorId'
  Lude.Text ->
  AcceptInvitation
mkAcceptInvitation pMasterId_ pInvitationId_ pDetectorId_ =
  AcceptInvitation'
    { masterId = pMasterId_,
      invitationId = pInvitationId_,
      detectorId = pDetectorId_
    }

-- | The account ID of the master GuardDuty account whose invitation you're accepting.
--
-- /Note:/ Consider using 'masterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiMasterId :: Lens.Lens' AcceptInvitation Lude.Text
aiMasterId = Lens.lens (masterId :: AcceptInvitation -> Lude.Text) (\s a -> s {masterId = a} :: AcceptInvitation)
{-# DEPRECATED aiMasterId "Use generic-lens or generic-optics with 'masterId' instead." #-}

-- | The value that is used to validate the master account to the member account.
--
-- /Note:/ Consider using 'invitationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiInvitationId :: Lens.Lens' AcceptInvitation Lude.Text
aiInvitationId = Lens.lens (invitationId :: AcceptInvitation -> Lude.Text) (\s a -> s {invitationId = a} :: AcceptInvitation)
{-# DEPRECATED aiInvitationId "Use generic-lens or generic-optics with 'invitationId' instead." #-}

-- | The unique ID of the detector of the GuardDuty member account.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiDetectorId :: Lens.Lens' AcceptInvitation Lude.Text
aiDetectorId = Lens.lens (detectorId :: AcceptInvitation -> Lude.Text) (\s a -> s {detectorId = a} :: AcceptInvitation)
{-# DEPRECATED aiDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

instance Lude.AWSRequest AcceptInvitation where
  type Rs AcceptInvitation = AcceptInvitationResponse
  request = Req.postJSON guardDutyService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AcceptInvitationResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AcceptInvitation where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AcceptInvitation where
  toJSON AcceptInvitation' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("masterId" Lude..= masterId),
            Lude.Just ("invitationId" Lude..= invitationId)
          ]
      )

instance Lude.ToPath AcceptInvitation where
  toPath AcceptInvitation' {..} =
    Lude.mconcat ["/detector/", Lude.toBS detectorId, "/master"]

instance Lude.ToQuery AcceptInvitation where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAcceptInvitationResponse' smart constructor.
newtype AcceptInvitationResponse = AcceptInvitationResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AcceptInvitationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAcceptInvitationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AcceptInvitationResponse
mkAcceptInvitationResponse pResponseStatus_ =
  AcceptInvitationResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
airsResponseStatus :: Lens.Lens' AcceptInvitationResponse Lude.Int
airsResponseStatus = Lens.lens (responseStatus :: AcceptInvitationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AcceptInvitationResponse)
{-# DEPRECATED airsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
