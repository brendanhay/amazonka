{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.ModifyDocumentPermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Shares a Systems Manager document publicly or privately. If you share a document privately, you must specify the AWS user account IDs for those people who can use the document. If you share a document publicly, you must specify /All/ as the account ID.
module Network.AWS.SSM.ModifyDocumentPermission
  ( -- * Creating a request
    ModifyDocumentPermission (..),
    mkModifyDocumentPermission,

    -- ** Request lenses
    mdpName,
    mdpPermissionType,
    mdpAccountIdsToAdd,
    mdpAccountIdsToRemove,
    mdpSharedDocumentVersion,

    -- * Destructuring the response
    ModifyDocumentPermissionResponse (..),
    mkModifyDocumentPermissionResponse,

    -- ** Response lenses
    mdprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkModifyDocumentPermission' smart constructor.
data ModifyDocumentPermission = ModifyDocumentPermission'
  { -- | The name of the document that you want to share.
    name :: Types.Name,
    -- | The permission type for the document. The permission type can be /Share/ .
    permissionType :: Types.DocumentPermissionType,
    -- | The AWS user accounts that should have access to the document. The account IDs can either be a group of account IDs or /All/ .
    accountIdsToAdd :: Core.Maybe [Types.AccountId],
    -- | The AWS user accounts that should no longer have access to the document. The AWS user account can either be a group of account IDs or /All/ . This action has a higher priority than /AccountIdsToAdd/ . If you specify an account ID to add and the same ID to remove, the system removes access to the document.
    accountIdsToRemove :: Core.Maybe [Types.AccountId],
    -- | (Optional) The version of the document to share. If it's not specified, the system choose the @Default@ version to share.
    sharedDocumentVersion :: Core.Maybe Types.SharedDocumentVersion
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyDocumentPermission' value with any optional fields omitted.
mkModifyDocumentPermission ::
  -- | 'name'
  Types.Name ->
  -- | 'permissionType'
  Types.DocumentPermissionType ->
  ModifyDocumentPermission
mkModifyDocumentPermission name permissionType =
  ModifyDocumentPermission'
    { name,
      permissionType,
      accountIdsToAdd = Core.Nothing,
      accountIdsToRemove = Core.Nothing,
      sharedDocumentVersion = Core.Nothing
    }

-- | The name of the document that you want to share.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdpName :: Lens.Lens' ModifyDocumentPermission Types.Name
mdpName = Lens.field @"name"
{-# DEPRECATED mdpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The permission type for the document. The permission type can be /Share/ .
--
-- /Note:/ Consider using 'permissionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdpPermissionType :: Lens.Lens' ModifyDocumentPermission Types.DocumentPermissionType
mdpPermissionType = Lens.field @"permissionType"
{-# DEPRECATED mdpPermissionType "Use generic-lens or generic-optics with 'permissionType' instead." #-}

-- | The AWS user accounts that should have access to the document. The account IDs can either be a group of account IDs or /All/ .
--
-- /Note:/ Consider using 'accountIdsToAdd' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdpAccountIdsToAdd :: Lens.Lens' ModifyDocumentPermission (Core.Maybe [Types.AccountId])
mdpAccountIdsToAdd = Lens.field @"accountIdsToAdd"
{-# DEPRECATED mdpAccountIdsToAdd "Use generic-lens or generic-optics with 'accountIdsToAdd' instead." #-}

-- | The AWS user accounts that should no longer have access to the document. The AWS user account can either be a group of account IDs or /All/ . This action has a higher priority than /AccountIdsToAdd/ . If you specify an account ID to add and the same ID to remove, the system removes access to the document.
--
-- /Note:/ Consider using 'accountIdsToRemove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdpAccountIdsToRemove :: Lens.Lens' ModifyDocumentPermission (Core.Maybe [Types.AccountId])
mdpAccountIdsToRemove = Lens.field @"accountIdsToRemove"
{-# DEPRECATED mdpAccountIdsToRemove "Use generic-lens or generic-optics with 'accountIdsToRemove' instead." #-}

-- | (Optional) The version of the document to share. If it's not specified, the system choose the @Default@ version to share.
--
-- /Note:/ Consider using 'sharedDocumentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdpSharedDocumentVersion :: Lens.Lens' ModifyDocumentPermission (Core.Maybe Types.SharedDocumentVersion)
mdpSharedDocumentVersion = Lens.field @"sharedDocumentVersion"
{-# DEPRECATED mdpSharedDocumentVersion "Use generic-lens or generic-optics with 'sharedDocumentVersion' instead." #-}

instance Core.FromJSON ModifyDocumentPermission where
  toJSON ModifyDocumentPermission {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("PermissionType" Core..= permissionType),
            ("AccountIdsToAdd" Core..=) Core.<$> accountIdsToAdd,
            ("AccountIdsToRemove" Core..=) Core.<$> accountIdsToRemove,
            ("SharedDocumentVersion" Core..=) Core.<$> sharedDocumentVersion
          ]
      )

instance Core.AWSRequest ModifyDocumentPermission where
  type Rs ModifyDocumentPermission = ModifyDocumentPermissionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonSSM.ModifyDocumentPermission")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          ModifyDocumentPermissionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkModifyDocumentPermissionResponse' smart constructor.
newtype ModifyDocumentPermissionResponse = ModifyDocumentPermissionResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyDocumentPermissionResponse' value with any optional fields omitted.
mkModifyDocumentPermissionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ModifyDocumentPermissionResponse
mkModifyDocumentPermissionResponse responseStatus =
  ModifyDocumentPermissionResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdprrsResponseStatus :: Lens.Lens' ModifyDocumentPermissionResponse Core.Int
mdprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED mdprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
