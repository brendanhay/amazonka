{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteRolePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified inline policy that is embedded in the specified IAM role.
--
-- A role can also have managed policies attached to it. To detach a managed policy from a role, use 'DetachRolePolicy' . For more information about policies, refer to <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
module Network.AWS.IAM.DeleteRolePolicy
    (
    -- * Creating a request
      DeleteRolePolicy (..)
    , mkDeleteRolePolicy
    -- ** Request lenses
    , drpfRoleName
    , drpfPolicyName

    -- * Destructuring the response
    , DeleteRolePolicyResponse (..)
    , mkDeleteRolePolicyResponse
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteRolePolicy' smart constructor.
data DeleteRolePolicy = DeleteRolePolicy'
  { roleName :: Types.RoleName
    -- ^ The name (friendly name, not ARN) identifying the role that the policy is embedded in.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
  , policyName :: Types.PolicyName
    -- ^ The name of the inline policy to delete from the specified IAM role.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRolePolicy' value with any optional fields omitted.
mkDeleteRolePolicy
    :: Types.RoleName -- ^ 'roleName'
    -> Types.PolicyName -- ^ 'policyName'
    -> DeleteRolePolicy
mkDeleteRolePolicy roleName policyName
  = DeleteRolePolicy'{roleName, policyName}

-- | The name (friendly name, not ARN) identifying the role that the policy is embedded in.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpfRoleName :: Lens.Lens' DeleteRolePolicy Types.RoleName
drpfRoleName = Lens.field @"roleName"
{-# INLINEABLE drpfRoleName #-}
{-# DEPRECATED roleName "Use generic-lens or generic-optics with 'roleName' instead"  #-}

-- | The name of the inline policy to delete from the specified IAM role.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpfPolicyName :: Lens.Lens' DeleteRolePolicy Types.PolicyName
drpfPolicyName = Lens.field @"policyName"
{-# INLINEABLE drpfPolicyName #-}
{-# DEPRECATED policyName "Use generic-lens or generic-optics with 'policyName' instead"  #-}

instance Core.ToQuery DeleteRolePolicy where
        toQuery DeleteRolePolicy{..}
          = Core.toQueryPair "Action" ("DeleteRolePolicy" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<> Core.toQueryPair "RoleName" roleName
              Core.<> Core.toQueryPair "PolicyName" policyName

instance Core.ToHeaders DeleteRolePolicy where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteRolePolicy where
        type Rs DeleteRolePolicy = DeleteRolePolicyResponse
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
        parseResponse = Response.receiveNull DeleteRolePolicyResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteRolePolicyResponse' smart constructor.
data DeleteRolePolicyResponse = DeleteRolePolicyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRolePolicyResponse' value with any optional fields omitted.
mkDeleteRolePolicyResponse
    :: DeleteRolePolicyResponse
mkDeleteRolePolicyResponse = DeleteRolePolicyResponse'
