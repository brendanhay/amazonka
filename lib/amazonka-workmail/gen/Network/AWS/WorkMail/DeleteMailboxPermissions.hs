{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.DeleteMailboxPermissions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes permissions granted to a member (user or group).
module Network.AWS.WorkMail.DeleteMailboxPermissions
  ( -- * Creating a request
    DeleteMailboxPermissions (..),
    mkDeleteMailboxPermissions,

    -- ** Request lenses
    dmpGranteeId,
    dmpEntityId,
    dmpOrganizationId,

    -- * Destructuring the response
    DeleteMailboxPermissionsResponse (..),
    mkDeleteMailboxPermissionsResponse,

    -- ** Response lenses
    dmprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkMail.Types

-- | /See:/ 'mkDeleteMailboxPermissions' smart constructor.
data DeleteMailboxPermissions = DeleteMailboxPermissions'
  { -- | The identifier of the member (user or group) for which to delete granted permissions.
    granteeId :: Lude.Text,
    -- | The identifier of the member (user or group) that owns the mailbox.
    entityId :: Lude.Text,
    -- | The identifier of the organization under which the member (user or group) exists.
    organizationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteMailboxPermissions' with the minimum fields required to make a request.
--
-- * 'granteeId' - The identifier of the member (user or group) for which to delete granted permissions.
-- * 'entityId' - The identifier of the member (user or group) that owns the mailbox.
-- * 'organizationId' - The identifier of the organization under which the member (user or group) exists.
mkDeleteMailboxPermissions ::
  -- | 'granteeId'
  Lude.Text ->
  -- | 'entityId'
  Lude.Text ->
  -- | 'organizationId'
  Lude.Text ->
  DeleteMailboxPermissions
mkDeleteMailboxPermissions pGranteeId_ pEntityId_ pOrganizationId_ =
  DeleteMailboxPermissions'
    { granteeId = pGranteeId_,
      entityId = pEntityId_,
      organizationId = pOrganizationId_
    }

-- | The identifier of the member (user or group) for which to delete granted permissions.
--
-- /Note:/ Consider using 'granteeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmpGranteeId :: Lens.Lens' DeleteMailboxPermissions Lude.Text
dmpGranteeId = Lens.lens (granteeId :: DeleteMailboxPermissions -> Lude.Text) (\s a -> s {granteeId = a} :: DeleteMailboxPermissions)
{-# DEPRECATED dmpGranteeId "Use generic-lens or generic-optics with 'granteeId' instead." #-}

-- | The identifier of the member (user or group) that owns the mailbox.
--
-- /Note:/ Consider using 'entityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmpEntityId :: Lens.Lens' DeleteMailboxPermissions Lude.Text
dmpEntityId = Lens.lens (entityId :: DeleteMailboxPermissions -> Lude.Text) (\s a -> s {entityId = a} :: DeleteMailboxPermissions)
{-# DEPRECATED dmpEntityId "Use generic-lens or generic-optics with 'entityId' instead." #-}

-- | The identifier of the organization under which the member (user or group) exists.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmpOrganizationId :: Lens.Lens' DeleteMailboxPermissions Lude.Text
dmpOrganizationId = Lens.lens (organizationId :: DeleteMailboxPermissions -> Lude.Text) (\s a -> s {organizationId = a} :: DeleteMailboxPermissions)
{-# DEPRECATED dmpOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

instance Lude.AWSRequest DeleteMailboxPermissions where
  type Rs DeleteMailboxPermissions = DeleteMailboxPermissionsResponse
  request = Req.postJSON workMailService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteMailboxPermissionsResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteMailboxPermissions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkMailService.DeleteMailboxPermissions" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteMailboxPermissions where
  toJSON DeleteMailboxPermissions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("GranteeId" Lude..= granteeId),
            Lude.Just ("EntityId" Lude..= entityId),
            Lude.Just ("OrganizationId" Lude..= organizationId)
          ]
      )

instance Lude.ToPath DeleteMailboxPermissions where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteMailboxPermissions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteMailboxPermissionsResponse' smart constructor.
newtype DeleteMailboxPermissionsResponse = DeleteMailboxPermissionsResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteMailboxPermissionsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteMailboxPermissionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteMailboxPermissionsResponse
mkDeleteMailboxPermissionsResponse pResponseStatus_ =
  DeleteMailboxPermissionsResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprsResponseStatus :: Lens.Lens' DeleteMailboxPermissionsResponse Lude.Int
dmprsResponseStatus = Lens.lens (responseStatus :: DeleteMailboxPermissionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteMailboxPermissionsResponse)
{-# DEPRECATED dmprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
