{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeDocumentPermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the permissions for a Systems Manager document. If you created the document, you are the owner. If a document is shared, it can either be shared privately (by specifying a user's AWS account ID) or publicly (/All/ ).
module Network.AWS.SSM.DescribeDocumentPermission
  ( -- * Creating a request
    DescribeDocumentPermission (..),
    mkDescribeDocumentPermission,

    -- ** Request lenses
    ddpName,
    ddpPermissionType,

    -- * Destructuring the response
    DescribeDocumentPermissionResponse (..),
    mkDescribeDocumentPermissionResponse,

    -- ** Response lenses
    ddprrsAccountIds,
    ddprrsAccountSharingInfoList,
    ddprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkDescribeDocumentPermission' smart constructor.
data DescribeDocumentPermission = DescribeDocumentPermission'
  { -- | The name of the document for which you are the owner.
    name :: Types.DocumentName,
    -- | The permission type for the document. The permission type can be /Share/ .
    permissionType :: Types.DocumentPermissionType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDocumentPermission' value with any optional fields omitted.
mkDescribeDocumentPermission ::
  -- | 'name'
  Types.DocumentName ->
  -- | 'permissionType'
  Types.DocumentPermissionType ->
  DescribeDocumentPermission
mkDescribeDocumentPermission name permissionType =
  DescribeDocumentPermission' {name, permissionType}

-- | The name of the document for which you are the owner.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddpName :: Lens.Lens' DescribeDocumentPermission Types.DocumentName
ddpName = Lens.field @"name"
{-# DEPRECATED ddpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The permission type for the document. The permission type can be /Share/ .
--
-- /Note:/ Consider using 'permissionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddpPermissionType :: Lens.Lens' DescribeDocumentPermission Types.DocumentPermissionType
ddpPermissionType = Lens.field @"permissionType"
{-# DEPRECATED ddpPermissionType "Use generic-lens or generic-optics with 'permissionType' instead." #-}

instance Core.FromJSON DescribeDocumentPermission where
  toJSON DescribeDocumentPermission {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("PermissionType" Core..= permissionType)
          ]
      )

instance Core.AWSRequest DescribeDocumentPermission where
  type
    Rs DescribeDocumentPermission =
      DescribeDocumentPermissionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonSSM.DescribeDocumentPermission")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDocumentPermissionResponse'
            Core.<$> (x Core..:? "AccountIds")
            Core.<*> (x Core..:? "AccountSharingInfoList")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeDocumentPermissionResponse' smart constructor.
data DescribeDocumentPermissionResponse = DescribeDocumentPermissionResponse'
  { -- | The account IDs that have permission to use this document. The ID can be either an AWS account or /All/ .
    accountIds :: Core.Maybe [Types.AccountId],
    -- | A list of AWS accounts where the current document is shared and the version shared with each account.
    accountSharingInfoList :: Core.Maybe [Types.AccountSharingInfo],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDocumentPermissionResponse' value with any optional fields omitted.
mkDescribeDocumentPermissionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeDocumentPermissionResponse
mkDescribeDocumentPermissionResponse responseStatus =
  DescribeDocumentPermissionResponse'
    { accountIds = Core.Nothing,
      accountSharingInfoList = Core.Nothing,
      responseStatus
    }

-- | The account IDs that have permission to use this document. The ID can be either an AWS account or /All/ .
--
-- /Note:/ Consider using 'accountIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddprrsAccountIds :: Lens.Lens' DescribeDocumentPermissionResponse (Core.Maybe [Types.AccountId])
ddprrsAccountIds = Lens.field @"accountIds"
{-# DEPRECATED ddprrsAccountIds "Use generic-lens or generic-optics with 'accountIds' instead." #-}

-- | A list of AWS accounts where the current document is shared and the version shared with each account.
--
-- /Note:/ Consider using 'accountSharingInfoList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddprrsAccountSharingInfoList :: Lens.Lens' DescribeDocumentPermissionResponse (Core.Maybe [Types.AccountSharingInfo])
ddprrsAccountSharingInfoList = Lens.field @"accountSharingInfoList"
{-# DEPRECATED ddprrsAccountSharingInfoList "Use generic-lens or generic-optics with 'accountSharingInfoList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddprrsResponseStatus :: Lens.Lens' DescribeDocumentPermissionResponse Core.Int
ddprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
