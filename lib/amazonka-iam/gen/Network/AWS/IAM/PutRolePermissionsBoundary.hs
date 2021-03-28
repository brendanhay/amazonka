{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.PutRolePermissionsBoundary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates the policy that is specified as the IAM role's permissions boundary. You can use an AWS managed policy or a customer managed policy to set the boundary for a role. Use the boundary to control the maximum permissions that the role can have. Setting a permissions boundary is an advanced feature that can affect the permissions for the role.
--
-- You cannot set the boundary for a service-linked role. 
-- /Important:/ Policies used as permissions boundaries do not provide permissions. You must also attach a permissions policy to the role. To learn how the effective permissions for a role are evaluated, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_evaluation-logic.html IAM JSON Policy Evaluation Logic> in the IAM User Guide. 
module Network.AWS.IAM.PutRolePermissionsBoundary
    (
    -- * Creating a request
      PutRolePermissionsBoundary (..)
    , mkPutRolePermissionsBoundary
    -- ** Request lenses
    , prpbRoleName
    , prpbPermissionsBoundary

    -- * Destructuring the response
    , PutRolePermissionsBoundaryResponse (..)
    , mkPutRolePermissionsBoundaryResponse
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutRolePermissionsBoundary' smart constructor.
data PutRolePermissionsBoundary = PutRolePermissionsBoundary'
  { roleName :: Types.RoleName
    -- ^ The name (friendly name, not ARN) of the IAM role for which you want to set the permissions boundary.
  , permissionsBoundary :: Types.PermissionsBoundary
    -- ^ The ARN of the policy that is used to set the permissions boundary for the role.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutRolePermissionsBoundary' value with any optional fields omitted.
mkPutRolePermissionsBoundary
    :: Types.RoleName -- ^ 'roleName'
    -> Types.PermissionsBoundary -- ^ 'permissionsBoundary'
    -> PutRolePermissionsBoundary
mkPutRolePermissionsBoundary roleName permissionsBoundary
  = PutRolePermissionsBoundary'{roleName, permissionsBoundary}

-- | The name (friendly name, not ARN) of the IAM role for which you want to set the permissions boundary.
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prpbRoleName :: Lens.Lens' PutRolePermissionsBoundary Types.RoleName
prpbRoleName = Lens.field @"roleName"
{-# INLINEABLE prpbRoleName #-}
{-# DEPRECATED roleName "Use generic-lens or generic-optics with 'roleName' instead"  #-}

-- | The ARN of the policy that is used to set the permissions boundary for the role.
--
-- /Note:/ Consider using 'permissionsBoundary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prpbPermissionsBoundary :: Lens.Lens' PutRolePermissionsBoundary Types.PermissionsBoundary
prpbPermissionsBoundary = Lens.field @"permissionsBoundary"
{-# INLINEABLE prpbPermissionsBoundary #-}
{-# DEPRECATED permissionsBoundary "Use generic-lens or generic-optics with 'permissionsBoundary' instead"  #-}

instance Core.ToQuery PutRolePermissionsBoundary where
        toQuery PutRolePermissionsBoundary{..}
          = Core.toQueryPair "Action"
              ("PutRolePermissionsBoundary" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<> Core.toQueryPair "RoleName" roleName
              Core.<> Core.toQueryPair "PermissionsBoundary" permissionsBoundary

instance Core.ToHeaders PutRolePermissionsBoundary where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest PutRolePermissionsBoundary where
        type Rs PutRolePermissionsBoundary =
             PutRolePermissionsBoundaryResponse
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
          = Response.receiveNull PutRolePermissionsBoundaryResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutRolePermissionsBoundaryResponse' smart constructor.
data PutRolePermissionsBoundaryResponse = PutRolePermissionsBoundaryResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutRolePermissionsBoundaryResponse' value with any optional fields omitted.
mkPutRolePermissionsBoundaryResponse
    :: PutRolePermissionsBoundaryResponse
mkPutRolePermissionsBoundaryResponse
  = PutRolePermissionsBoundaryResponse'
