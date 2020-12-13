{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.DeregisterFromWorkMail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Mark a user, group, or resource as no longer used in Amazon WorkMail. This action disassociates the mailbox and schedules it for clean-up. WorkMail keeps mailboxes for 30 days before they are permanently removed. The functionality in the console is /Disable/ .
module Network.AWS.WorkMail.DeregisterFromWorkMail
  ( -- * Creating a request
    DeregisterFromWorkMail (..),
    mkDeregisterFromWorkMail,

    -- ** Request lenses
    dfwmEntityId,
    dfwmOrganizationId,

    -- * Destructuring the response
    DeregisterFromWorkMailResponse (..),
    mkDeregisterFromWorkMailResponse,

    -- ** Response lenses
    dfwmrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkMail.Types

-- | /See:/ 'mkDeregisterFromWorkMail' smart constructor.
data DeregisterFromWorkMail = DeregisterFromWorkMail'
  { -- | The identifier for the member (user or group) to be updated.
    entityId :: Lude.Text,
    -- | The identifier for the organization under which the Amazon WorkMail entity exists.
    organizationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeregisterFromWorkMail' with the minimum fields required to make a request.
--
-- * 'entityId' - The identifier for the member (user or group) to be updated.
-- * 'organizationId' - The identifier for the organization under which the Amazon WorkMail entity exists.
mkDeregisterFromWorkMail ::
  -- | 'entityId'
  Lude.Text ->
  -- | 'organizationId'
  Lude.Text ->
  DeregisterFromWorkMail
mkDeregisterFromWorkMail pEntityId_ pOrganizationId_ =
  DeregisterFromWorkMail'
    { entityId = pEntityId_,
      organizationId = pOrganizationId_
    }

-- | The identifier for the member (user or group) to be updated.
--
-- /Note:/ Consider using 'entityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfwmEntityId :: Lens.Lens' DeregisterFromWorkMail Lude.Text
dfwmEntityId = Lens.lens (entityId :: DeregisterFromWorkMail -> Lude.Text) (\s a -> s {entityId = a} :: DeregisterFromWorkMail)
{-# DEPRECATED dfwmEntityId "Use generic-lens or generic-optics with 'entityId' instead." #-}

-- | The identifier for the organization under which the Amazon WorkMail entity exists.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfwmOrganizationId :: Lens.Lens' DeregisterFromWorkMail Lude.Text
dfwmOrganizationId = Lens.lens (organizationId :: DeregisterFromWorkMail -> Lude.Text) (\s a -> s {organizationId = a} :: DeregisterFromWorkMail)
{-# DEPRECATED dfwmOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

instance Lude.AWSRequest DeregisterFromWorkMail where
  type Rs DeregisterFromWorkMail = DeregisterFromWorkMailResponse
  request = Req.postJSON workMailService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeregisterFromWorkMailResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeregisterFromWorkMail where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkMailService.DeregisterFromWorkMail" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeregisterFromWorkMail where
  toJSON DeregisterFromWorkMail' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("EntityId" Lude..= entityId),
            Lude.Just ("OrganizationId" Lude..= organizationId)
          ]
      )

instance Lude.ToPath DeregisterFromWorkMail where
  toPath = Lude.const "/"

instance Lude.ToQuery DeregisterFromWorkMail where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeregisterFromWorkMailResponse' smart constructor.
newtype DeregisterFromWorkMailResponse = DeregisterFromWorkMailResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeregisterFromWorkMailResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeregisterFromWorkMailResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeregisterFromWorkMailResponse
mkDeregisterFromWorkMailResponse pResponseStatus_ =
  DeregisterFromWorkMailResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfwmrsResponseStatus :: Lens.Lens' DeregisterFromWorkMailResponse Lude.Int
dfwmrsResponseStatus = Lens.lens (responseStatus :: DeregisterFromWorkMailResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeregisterFromWorkMailResponse)
{-# DEPRECATED dfwmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
