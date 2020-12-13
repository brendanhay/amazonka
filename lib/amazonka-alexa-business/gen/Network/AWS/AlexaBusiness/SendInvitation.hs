{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.SendInvitation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends an enrollment invitation email with a URL to a user. The URL is valid for 30 days or until you call this operation again, whichever comes first.
module Network.AWS.AlexaBusiness.SendInvitation
  ( -- * Creating a request
    SendInvitation (..),
    mkSendInvitation,

    -- ** Request lenses
    siUserARN,

    -- * Destructuring the response
    SendInvitationResponse (..),
    mkSendInvitationResponse,

    -- ** Response lenses
    sirsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSendInvitation' smart constructor.
newtype SendInvitation = SendInvitation'
  { -- | The ARN of the user to whom to send an invitation. Required.
    userARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SendInvitation' with the minimum fields required to make a request.
--
-- * 'userARN' - The ARN of the user to whom to send an invitation. Required.
mkSendInvitation ::
  SendInvitation
mkSendInvitation = SendInvitation' {userARN = Lude.Nothing}

-- | The ARN of the user to whom to send an invitation. Required.
--
-- /Note:/ Consider using 'userARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siUserARN :: Lens.Lens' SendInvitation (Lude.Maybe Lude.Text)
siUserARN = Lens.lens (userARN :: SendInvitation -> Lude.Maybe Lude.Text) (\s a -> s {userARN = a} :: SendInvitation)
{-# DEPRECATED siUserARN "Use generic-lens or generic-optics with 'userARN' instead." #-}

instance Lude.AWSRequest SendInvitation where
  type Rs SendInvitation = SendInvitationResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveEmpty
      ( \s h x ->
          SendInvitationResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SendInvitation where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.SendInvitation" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SendInvitation where
  toJSON SendInvitation' {..} =
    Lude.object
      (Lude.catMaybes [("UserArn" Lude..=) Lude.<$> userARN])

instance Lude.ToPath SendInvitation where
  toPath = Lude.const "/"

instance Lude.ToQuery SendInvitation where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSendInvitationResponse' smart constructor.
newtype SendInvitationResponse = SendInvitationResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SendInvitationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkSendInvitationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SendInvitationResponse
mkSendInvitationResponse pResponseStatus_ =
  SendInvitationResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirsResponseStatus :: Lens.Lens' SendInvitationResponse Lude.Int
sirsResponseStatus = Lens.lens (responseStatus :: SendInvitationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SendInvitationResponse)
{-# DEPRECATED sirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
