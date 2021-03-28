{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DetachRolePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified managed policy from the specified role.
--
-- A role can also have inline policies embedded with it. To delete an inline policy, use the 'DeleteRolePolicy' API. For information about policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
module Network.AWS.IAM.DetachRolePolicy
    (
    -- * Creating a request
      DetachRolePolicy (..)
    , mkDetachRolePolicy
    -- ** Request lenses
    , drpRoleName
    , drpPolicyArn

    -- * Destructuring the response
    , DetachRolePolicyResponse (..)
    , mkDetachRolePolicyResponse
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDetachRolePolicy' smart constructor.
data DetachRolePolicy = DetachRolePolicy'
  { roleName :: Types.RoleName
    -- ^ The name (friendly name, not ARN) of the IAM role to detach the policy from.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
  , policyArn :: Types.PolicyArn
    -- ^ The Amazon Resource Name (ARN) of the IAM policy you want to detach.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetachRolePolicy' value with any optional fields omitted.
mkDetachRolePolicy
    :: Types.RoleName -- ^ 'roleName'
    -> Types.PolicyArn -- ^ 'policyArn'
    -> DetachRolePolicy
mkDetachRolePolicy roleName policyArn
  = DetachRolePolicy'{roleName, policyArn}

-- | The name (friendly name, not ARN) of the IAM role to detach the policy from.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpRoleName :: Lens.Lens' DetachRolePolicy Types.RoleName
drpRoleName = Lens.field @"roleName"
{-# INLINEABLE drpRoleName #-}
{-# DEPRECATED roleName "Use generic-lens or generic-optics with 'roleName' instead"  #-}

-- | The Amazon Resource Name (ARN) of the IAM policy you want to detach.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'policyArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpPolicyArn :: Lens.Lens' DetachRolePolicy Types.PolicyArn
drpPolicyArn = Lens.field @"policyArn"
{-# INLINEABLE drpPolicyArn #-}
{-# DEPRECATED policyArn "Use generic-lens or generic-optics with 'policyArn' instead"  #-}

instance Core.ToQuery DetachRolePolicy where
        toQuery DetachRolePolicy{..}
          = Core.toQueryPair "Action" ("DetachRolePolicy" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<> Core.toQueryPair "RoleName" roleName
              Core.<> Core.toQueryPair "PolicyArn" policyArn

instance Core.ToHeaders DetachRolePolicy where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DetachRolePolicy where
        type Rs DetachRolePolicy = DetachRolePolicyResponse
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
        parseResponse = Response.receiveNull DetachRolePolicyResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDetachRolePolicyResponse' smart constructor.
data DetachRolePolicyResponse = DetachRolePolicyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetachRolePolicyResponse' value with any optional fields omitted.
mkDetachRolePolicyResponse
    :: DetachRolePolicyResponse
mkDetachRolePolicyResponse = DetachRolePolicyResponse'
