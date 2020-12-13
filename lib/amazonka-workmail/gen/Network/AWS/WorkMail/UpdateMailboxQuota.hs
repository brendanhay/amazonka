{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.UpdateMailboxQuota
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a user's current mailbox quota for a specified organization and user.
module Network.AWS.WorkMail.UpdateMailboxQuota
  ( -- * Creating a request
    UpdateMailboxQuota (..),
    mkUpdateMailboxQuota,

    -- ** Request lenses
    umqMailboxQuota,
    umqUserId,
    umqOrganizationId,

    -- * Destructuring the response
    UpdateMailboxQuotaResponse (..),
    mkUpdateMailboxQuotaResponse,

    -- ** Response lenses
    umqrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkMail.Types

-- | /See:/ 'mkUpdateMailboxQuota' smart constructor.
data UpdateMailboxQuota = UpdateMailboxQuota'
  { -- | The updated mailbox quota, in MB, for the specified user.
    mailboxQuota :: Lude.Natural,
    -- | The identifer for the user for whom to update the mailbox quota.
    userId :: Lude.Text,
    -- | The identifier for the organization that contains the user for whom to update the mailbox quota.
    organizationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateMailboxQuota' with the minimum fields required to make a request.
--
-- * 'mailboxQuota' - The updated mailbox quota, in MB, for the specified user.
-- * 'userId' - The identifer for the user for whom to update the mailbox quota.
-- * 'organizationId' - The identifier for the organization that contains the user for whom to update the mailbox quota.
mkUpdateMailboxQuota ::
  -- | 'mailboxQuota'
  Lude.Natural ->
  -- | 'userId'
  Lude.Text ->
  -- | 'organizationId'
  Lude.Text ->
  UpdateMailboxQuota
mkUpdateMailboxQuota pMailboxQuota_ pUserId_ pOrganizationId_ =
  UpdateMailboxQuota'
    { mailboxQuota = pMailboxQuota_,
      userId = pUserId_,
      organizationId = pOrganizationId_
    }

-- | The updated mailbox quota, in MB, for the specified user.
--
-- /Note:/ Consider using 'mailboxQuota' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umqMailboxQuota :: Lens.Lens' UpdateMailboxQuota Lude.Natural
umqMailboxQuota = Lens.lens (mailboxQuota :: UpdateMailboxQuota -> Lude.Natural) (\s a -> s {mailboxQuota = a} :: UpdateMailboxQuota)
{-# DEPRECATED umqMailboxQuota "Use generic-lens or generic-optics with 'mailboxQuota' instead." #-}

-- | The identifer for the user for whom to update the mailbox quota.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umqUserId :: Lens.Lens' UpdateMailboxQuota Lude.Text
umqUserId = Lens.lens (userId :: UpdateMailboxQuota -> Lude.Text) (\s a -> s {userId = a} :: UpdateMailboxQuota)
{-# DEPRECATED umqUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | The identifier for the organization that contains the user for whom to update the mailbox quota.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umqOrganizationId :: Lens.Lens' UpdateMailboxQuota Lude.Text
umqOrganizationId = Lens.lens (organizationId :: UpdateMailboxQuota -> Lude.Text) (\s a -> s {organizationId = a} :: UpdateMailboxQuota)
{-# DEPRECATED umqOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

instance Lude.AWSRequest UpdateMailboxQuota where
  type Rs UpdateMailboxQuota = UpdateMailboxQuotaResponse
  request = Req.postJSON workMailService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateMailboxQuotaResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateMailboxQuota where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkMailService.UpdateMailboxQuota" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateMailboxQuota where
  toJSON UpdateMailboxQuota' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("MailboxQuota" Lude..= mailboxQuota),
            Lude.Just ("UserId" Lude..= userId),
            Lude.Just ("OrganizationId" Lude..= organizationId)
          ]
      )

instance Lude.ToPath UpdateMailboxQuota where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateMailboxQuota where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateMailboxQuotaResponse' smart constructor.
newtype UpdateMailboxQuotaResponse = UpdateMailboxQuotaResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateMailboxQuotaResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateMailboxQuotaResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateMailboxQuotaResponse
mkUpdateMailboxQuotaResponse pResponseStatus_ =
  UpdateMailboxQuotaResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umqrsResponseStatus :: Lens.Lens' UpdateMailboxQuotaResponse Lude.Int
umqrsResponseStatus = Lens.lens (responseStatus :: UpdateMailboxQuotaResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateMailboxQuotaResponse)
{-# DEPRECATED umqrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
