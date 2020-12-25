{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribePermissions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the permissions for a specified stack.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DescribePermissions
  ( -- * Creating a request
    DescribePermissions (..),
    mkDescribePermissions,

    -- ** Request lenses
    dpIamUserArn,
    dpStackId,

    -- * Destructuring the response
    DescribePermissionsResponse (..),
    mkDescribePermissionsResponse,

    -- ** Response lenses
    dprrsPermissions,
    dprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribePermissions' smart constructor.
data DescribePermissions = DescribePermissions'
  { -- | The user's IAM ARN. This can also be a federated user's ARN. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
    iamUserArn :: Core.Maybe Types.String,
    -- | The stack ID.
    stackId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribePermissions' value with any optional fields omitted.
mkDescribePermissions ::
  DescribePermissions
mkDescribePermissions =
  DescribePermissions'
    { iamUserArn = Core.Nothing,
      stackId = Core.Nothing
    }

-- | The user's IAM ARN. This can also be a federated user's ARN. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
--
-- /Note:/ Consider using 'iamUserArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpIamUserArn :: Lens.Lens' DescribePermissions (Core.Maybe Types.String)
dpIamUserArn = Lens.field @"iamUserArn"
{-# DEPRECATED dpIamUserArn "Use generic-lens or generic-optics with 'iamUserArn' instead." #-}

-- | The stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpStackId :: Lens.Lens' DescribePermissions (Core.Maybe Types.String)
dpStackId = Lens.field @"stackId"
{-# DEPRECATED dpStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

instance Core.FromJSON DescribePermissions where
  toJSON DescribePermissions {..} =
    Core.object
      ( Core.catMaybes
          [ ("IamUserArn" Core..=) Core.<$> iamUserArn,
            ("StackId" Core..=) Core.<$> stackId
          ]
      )

instance Core.AWSRequest DescribePermissions where
  type Rs DescribePermissions = DescribePermissionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "OpsWorks_20130218.DescribePermissions")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePermissionsResponse'
            Core.<$> (x Core..:? "Permissions") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the response to a @DescribePermissions@ request.
--
-- /See:/ 'mkDescribePermissionsResponse' smart constructor.
data DescribePermissionsResponse = DescribePermissionsResponse'
  { -- | An array of @Permission@ objects that describe the stack permissions.
    --
    --
    --     * If the request object contains only a stack ID, the array contains a @Permission@ object with permissions for each of the stack IAM ARNs.
    --
    --
    --     * If the request object contains only an IAM ARN, the array contains a @Permission@ object with permissions for each of the user's stack IDs.
    --
    --
    --     * If the request contains a stack ID and an IAM ARN, the array contains a single @Permission@ object with permissions for the specified stack and IAM ARN.
    permissions :: Core.Maybe [Types.Permission],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribePermissionsResponse' value with any optional fields omitted.
mkDescribePermissionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribePermissionsResponse
mkDescribePermissionsResponse responseStatus =
  DescribePermissionsResponse'
    { permissions = Core.Nothing,
      responseStatus
    }

-- | An array of @Permission@ objects that describe the stack permissions.
--
--
--     * If the request object contains only a stack ID, the array contains a @Permission@ object with permissions for each of the stack IAM ARNs.
--
--
--     * If the request object contains only an IAM ARN, the array contains a @Permission@ object with permissions for each of the user's stack IDs.
--
--
--     * If the request contains a stack ID and an IAM ARN, the array contains a single @Permission@ object with permissions for the specified stack and IAM ARN.
--
--
--
-- /Note:/ Consider using 'permissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprrsPermissions :: Lens.Lens' DescribePermissionsResponse (Core.Maybe [Types.Permission])
dprrsPermissions = Lens.field @"permissions"
{-# DEPRECATED dprrsPermissions "Use generic-lens or generic-optics with 'permissions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprrsResponseStatus :: Lens.Lens' DescribePermissionsResponse Core.Int
dprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
