{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.PutMailboxPermissions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets permissions for a user, group, or resource. This replaces any pre-existing permissions.
module Network.AWS.WorkMail.PutMailboxPermissions
  ( -- * Creating a request
    PutMailboxPermissions (..),
    mkPutMailboxPermissions,

    -- ** Request lenses
    pmpPermissionValues,
    pmpGranteeId,
    pmpEntityId,
    pmpOrganizationId,

    -- * Destructuring the response
    PutMailboxPermissionsResponse (..),
    mkPutMailboxPermissionsResponse,

    -- ** Response lenses
    pmprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkMail.Types

-- | /See:/ 'mkPutMailboxPermissions' smart constructor.
data PutMailboxPermissions = PutMailboxPermissions'
  { -- | The permissions granted to the grantee. SEND_AS allows the grantee to send email as the owner of the mailbox (the grantee is not mentioned on these emails). SEND_ON_BEHALF allows the grantee to send email on behalf of the owner of the mailbox (the grantee is not mentioned as the physical sender of these emails). FULL_ACCESS allows the grantee full access to the mailbox, irrespective of other folder-level permissions set on the mailbox.
    permissionValues :: [PermissionType],
    -- | The identifier of the user, group, or resource to which to grant the permissions.
    granteeId :: Lude.Text,
    -- | The identifier of the user, group, or resource for which to update mailbox permissions.
    entityId :: Lude.Text,
    -- | The identifier of the organization under which the user, group, or resource exists.
    organizationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutMailboxPermissions' with the minimum fields required to make a request.
--
-- * 'permissionValues' - The permissions granted to the grantee. SEND_AS allows the grantee to send email as the owner of the mailbox (the grantee is not mentioned on these emails). SEND_ON_BEHALF allows the grantee to send email on behalf of the owner of the mailbox (the grantee is not mentioned as the physical sender of these emails). FULL_ACCESS allows the grantee full access to the mailbox, irrespective of other folder-level permissions set on the mailbox.
-- * 'granteeId' - The identifier of the user, group, or resource to which to grant the permissions.
-- * 'entityId' - The identifier of the user, group, or resource for which to update mailbox permissions.
-- * 'organizationId' - The identifier of the organization under which the user, group, or resource exists.
mkPutMailboxPermissions ::
  -- | 'granteeId'
  Lude.Text ->
  -- | 'entityId'
  Lude.Text ->
  -- | 'organizationId'
  Lude.Text ->
  PutMailboxPermissions
mkPutMailboxPermissions pGranteeId_ pEntityId_ pOrganizationId_ =
  PutMailboxPermissions'
    { permissionValues = Lude.mempty,
      granteeId = pGranteeId_,
      entityId = pEntityId_,
      organizationId = pOrganizationId_
    }

-- | The permissions granted to the grantee. SEND_AS allows the grantee to send email as the owner of the mailbox (the grantee is not mentioned on these emails). SEND_ON_BEHALF allows the grantee to send email on behalf of the owner of the mailbox (the grantee is not mentioned as the physical sender of these emails). FULL_ACCESS allows the grantee full access to the mailbox, irrespective of other folder-level permissions set on the mailbox.
--
-- /Note:/ Consider using 'permissionValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmpPermissionValues :: Lens.Lens' PutMailboxPermissions [PermissionType]
pmpPermissionValues = Lens.lens (permissionValues :: PutMailboxPermissions -> [PermissionType]) (\s a -> s {permissionValues = a} :: PutMailboxPermissions)
{-# DEPRECATED pmpPermissionValues "Use generic-lens or generic-optics with 'permissionValues' instead." #-}

-- | The identifier of the user, group, or resource to which to grant the permissions.
--
-- /Note:/ Consider using 'granteeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmpGranteeId :: Lens.Lens' PutMailboxPermissions Lude.Text
pmpGranteeId = Lens.lens (granteeId :: PutMailboxPermissions -> Lude.Text) (\s a -> s {granteeId = a} :: PutMailboxPermissions)
{-# DEPRECATED pmpGranteeId "Use generic-lens or generic-optics with 'granteeId' instead." #-}

-- | The identifier of the user, group, or resource for which to update mailbox permissions.
--
-- /Note:/ Consider using 'entityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmpEntityId :: Lens.Lens' PutMailboxPermissions Lude.Text
pmpEntityId = Lens.lens (entityId :: PutMailboxPermissions -> Lude.Text) (\s a -> s {entityId = a} :: PutMailboxPermissions)
{-# DEPRECATED pmpEntityId "Use generic-lens or generic-optics with 'entityId' instead." #-}

-- | The identifier of the organization under which the user, group, or resource exists.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmpOrganizationId :: Lens.Lens' PutMailboxPermissions Lude.Text
pmpOrganizationId = Lens.lens (organizationId :: PutMailboxPermissions -> Lude.Text) (\s a -> s {organizationId = a} :: PutMailboxPermissions)
{-# DEPRECATED pmpOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

instance Lude.AWSRequest PutMailboxPermissions where
  type Rs PutMailboxPermissions = PutMailboxPermissionsResponse
  request = Req.postJSON workMailService
  response =
    Res.receiveEmpty
      ( \s h x ->
          PutMailboxPermissionsResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutMailboxPermissions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkMailService.PutMailboxPermissions" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutMailboxPermissions where
  toJSON PutMailboxPermissions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("PermissionValues" Lude..= permissionValues),
            Lude.Just ("GranteeId" Lude..= granteeId),
            Lude.Just ("EntityId" Lude..= entityId),
            Lude.Just ("OrganizationId" Lude..= organizationId)
          ]
      )

instance Lude.ToPath PutMailboxPermissions where
  toPath = Lude.const "/"

instance Lude.ToQuery PutMailboxPermissions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutMailboxPermissionsResponse' smart constructor.
newtype PutMailboxPermissionsResponse = PutMailboxPermissionsResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutMailboxPermissionsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkPutMailboxPermissionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutMailboxPermissionsResponse
mkPutMailboxPermissionsResponse pResponseStatus_ =
  PutMailboxPermissionsResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmprsResponseStatus :: Lens.Lens' PutMailboxPermissionsResponse Lude.Int
pmprsResponseStatus = Lens.lens (responseStatus :: PutMailboxPermissionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutMailboxPermissionsResponse)
{-# DEPRECATED pmprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
