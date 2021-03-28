{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteRolePermissionsBoundary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the permissions boundary for the specified IAM role. 
--
-- /Important:/ Deleting the permissions boundary for a role might increase its permissions. For example, it might allow anyone who assumes the role to perform all the actions granted in its permissions policies. 
module Network.AWS.IAM.DeleteRolePermissionsBoundary
    (
    -- * Creating a request
      DeleteRolePermissionsBoundary (..)
    , mkDeleteRolePermissionsBoundary
    -- ** Request lenses
    , drpbRoleName

    -- * Destructuring the response
    , DeleteRolePermissionsBoundaryResponse (..)
    , mkDeleteRolePermissionsBoundaryResponse
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteRolePermissionsBoundary' smart constructor.
newtype DeleteRolePermissionsBoundary = DeleteRolePermissionsBoundary'
  { roleName :: Types.RoleName
    -- ^ The name (friendly name, not ARN) of the IAM role from which you want to remove the permissions boundary.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRolePermissionsBoundary' value with any optional fields omitted.
mkDeleteRolePermissionsBoundary
    :: Types.RoleName -- ^ 'roleName'
    -> DeleteRolePermissionsBoundary
mkDeleteRolePermissionsBoundary roleName
  = DeleteRolePermissionsBoundary'{roleName}

-- | The name (friendly name, not ARN) of the IAM role from which you want to remove the permissions boundary.
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpbRoleName :: Lens.Lens' DeleteRolePermissionsBoundary Types.RoleName
drpbRoleName = Lens.field @"roleName"
{-# INLINEABLE drpbRoleName #-}
{-# DEPRECATED roleName "Use generic-lens or generic-optics with 'roleName' instead"  #-}

instance Core.ToQuery DeleteRolePermissionsBoundary where
        toQuery DeleteRolePermissionsBoundary{..}
          = Core.toQueryPair "Action"
              ("DeleteRolePermissionsBoundary" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<> Core.toQueryPair "RoleName" roleName

instance Core.ToHeaders DeleteRolePermissionsBoundary where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteRolePermissionsBoundary where
        type Rs DeleteRolePermissionsBoundary =
             DeleteRolePermissionsBoundaryResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull DeleteRolePermissionsBoundaryResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteRolePermissionsBoundaryResponse' smart constructor.
data DeleteRolePermissionsBoundaryResponse = DeleteRolePermissionsBoundaryResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRolePermissionsBoundaryResponse' value with any optional fields omitted.
mkDeleteRolePermissionsBoundaryResponse
    :: DeleteRolePermissionsBoundaryResponse
mkDeleteRolePermissionsBoundaryResponse
  = DeleteRolePermissionsBoundaryResponse'
