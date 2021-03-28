{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeletePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified managed policy.
--
-- Before you can delete a managed policy, you must first detach the policy from all users, groups, and roles that it is attached to. In addition, you must delete all the policy's versions. The following steps describe the process for deleting a managed policy:
--
--     * Detach the policy from all users, groups, and roles that the policy is attached to, using the 'DetachUserPolicy' , 'DetachGroupPolicy' , or 'DetachRolePolicy' API operations. To list all the users, groups, and roles that a policy is attached to, use 'ListEntitiesForPolicy' .
--
--
--     * Delete all versions of the policy using 'DeletePolicyVersion' . To list the policy's versions, use 'ListPolicyVersions' . You cannot use 'DeletePolicyVersion' to delete the version that is marked as the default version. You delete the policy's default version in the next step of the process.
--
--
--     * Delete the policy (this automatically deletes the policy's default version) using this API.
--
--
-- For information about managed policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
module Network.AWS.IAM.DeletePolicy
    (
    -- * Creating a request
      DeletePolicy (..)
    , mkDeletePolicy
    -- ** Request lenses
    , dpPolicyArn

    -- * Destructuring the response
    , DeletePolicyResponse (..)
    , mkDeletePolicyResponse
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeletePolicy' smart constructor.
newtype DeletePolicy = DeletePolicy'
  { policyArn :: Types.PolicyArn
    -- ^ The Amazon Resource Name (ARN) of the IAM policy you want to delete.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePolicy' value with any optional fields omitted.
mkDeletePolicy
    :: Types.PolicyArn -- ^ 'policyArn'
    -> DeletePolicy
mkDeletePolicy policyArn = DeletePolicy'{policyArn}

-- | The Amazon Resource Name (ARN) of the IAM policy you want to delete.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'policyArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpPolicyArn :: Lens.Lens' DeletePolicy Types.PolicyArn
dpPolicyArn = Lens.field @"policyArn"
{-# INLINEABLE dpPolicyArn #-}
{-# DEPRECATED policyArn "Use generic-lens or generic-optics with 'policyArn' instead"  #-}

instance Core.ToQuery DeletePolicy where
        toQuery DeletePolicy{..}
          = Core.toQueryPair "Action" ("DeletePolicy" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<> Core.toQueryPair "PolicyArn" policyArn

instance Core.ToHeaders DeletePolicy where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeletePolicy where
        type Rs DeletePolicy = DeletePolicyResponse
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
        parseResponse = Response.receiveNull DeletePolicyResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeletePolicyResponse' smart constructor.
data DeletePolicyResponse = DeletePolicyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePolicyResponse' value with any optional fields omitted.
mkDeletePolicyResponse
    :: DeletePolicyResponse
mkDeletePolicyResponse = DeletePolicyResponse'
