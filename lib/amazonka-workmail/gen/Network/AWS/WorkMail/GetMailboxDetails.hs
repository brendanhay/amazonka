{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.GetMailboxDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests a user's mailbox details for a specified organization and user.
module Network.AWS.WorkMail.GetMailboxDetails
  ( -- * Creating a request
    GetMailboxDetails (..),
    mkGetMailboxDetails,

    -- ** Request lenses
    gmdOrganizationId,
    gmdUserId,

    -- * Destructuring the response
    GetMailboxDetailsResponse (..),
    mkGetMailboxDetailsResponse,

    -- ** Response lenses
    gmdrsMailboxQuota,
    gmdrsMailboxSize,
    gmdrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkMail.Types

-- | /See:/ 'mkGetMailboxDetails' smart constructor.
data GetMailboxDetails = GetMailboxDetails'
  { organizationId ::
      Lude.Text,
    userId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetMailboxDetails' with the minimum fields required to make a request.
--
-- * 'organizationId' - The identifier for the organization that contains the user whose mailbox details are being requested.
-- * 'userId' - The identifier for the user whose mailbox details are being requested.
mkGetMailboxDetails ::
  -- | 'organizationId'
  Lude.Text ->
  -- | 'userId'
  Lude.Text ->
  GetMailboxDetails
mkGetMailboxDetails pOrganizationId_ pUserId_ =
  GetMailboxDetails'
    { organizationId = pOrganizationId_,
      userId = pUserId_
    }

-- | The identifier for the organization that contains the user whose mailbox details are being requested.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdOrganizationId :: Lens.Lens' GetMailboxDetails Lude.Text
gmdOrganizationId = Lens.lens (organizationId :: GetMailboxDetails -> Lude.Text) (\s a -> s {organizationId = a} :: GetMailboxDetails)
{-# DEPRECATED gmdOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | The identifier for the user whose mailbox details are being requested.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdUserId :: Lens.Lens' GetMailboxDetails Lude.Text
gmdUserId = Lens.lens (userId :: GetMailboxDetails -> Lude.Text) (\s a -> s {userId = a} :: GetMailboxDetails)
{-# DEPRECATED gmdUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

instance Lude.AWSRequest GetMailboxDetails where
  type Rs GetMailboxDetails = GetMailboxDetailsResponse
  request = Req.postJSON workMailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetMailboxDetailsResponse'
            Lude.<$> (x Lude..?> "MailboxQuota")
            Lude.<*> (x Lude..?> "MailboxSize")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetMailboxDetails where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkMailService.GetMailboxDetails" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetMailboxDetails where
  toJSON GetMailboxDetails' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("OrganizationId" Lude..= organizationId),
            Lude.Just ("UserId" Lude..= userId)
          ]
      )

instance Lude.ToPath GetMailboxDetails where
  toPath = Lude.const "/"

instance Lude.ToQuery GetMailboxDetails where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetMailboxDetailsResponse' smart constructor.
data GetMailboxDetailsResponse = GetMailboxDetailsResponse'
  { mailboxQuota ::
      Lude.Maybe Lude.Natural,
    mailboxSize :: Lude.Maybe Lude.Double,
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

-- | Creates a value of 'GetMailboxDetailsResponse' with the minimum fields required to make a request.
--
-- * 'mailboxQuota' - The maximum allowed mailbox size, in MB, for the specified user.
-- * 'mailboxSize' - The current mailbox size, in MB, for the specified user.
-- * 'responseStatus' - The response status code.
mkGetMailboxDetailsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetMailboxDetailsResponse
mkGetMailboxDetailsResponse pResponseStatus_ =
  GetMailboxDetailsResponse'
    { mailboxQuota = Lude.Nothing,
      mailboxSize = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The maximum allowed mailbox size, in MB, for the specified user.
--
-- /Note:/ Consider using 'mailboxQuota' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdrsMailboxQuota :: Lens.Lens' GetMailboxDetailsResponse (Lude.Maybe Lude.Natural)
gmdrsMailboxQuota = Lens.lens (mailboxQuota :: GetMailboxDetailsResponse -> Lude.Maybe Lude.Natural) (\s a -> s {mailboxQuota = a} :: GetMailboxDetailsResponse)
{-# DEPRECATED gmdrsMailboxQuota "Use generic-lens or generic-optics with 'mailboxQuota' instead." #-}

-- | The current mailbox size, in MB, for the specified user.
--
-- /Note:/ Consider using 'mailboxSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdrsMailboxSize :: Lens.Lens' GetMailboxDetailsResponse (Lude.Maybe Lude.Double)
gmdrsMailboxSize = Lens.lens (mailboxSize :: GetMailboxDetailsResponse -> Lude.Maybe Lude.Double) (\s a -> s {mailboxSize = a} :: GetMailboxDetailsResponse)
{-# DEPRECATED gmdrsMailboxSize "Use generic-lens or generic-optics with 'mailboxSize' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdrsResponseStatus :: Lens.Lens' GetMailboxDetailsResponse Lude.Int
gmdrsResponseStatus = Lens.lens (responseStatus :: GetMailboxDetailsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetMailboxDetailsResponse)
{-# DEPRECATED gmdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
