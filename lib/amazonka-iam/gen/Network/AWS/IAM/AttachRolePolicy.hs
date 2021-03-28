{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.AttachRolePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches the specified managed policy to the specified IAM role. When you attach a managed policy to a role, the managed policy becomes part of the role's permission (access) policy.
--
-- Use this API to attach a /managed/ policy to a role. To embed an inline policy in a role, use 'PutRolePolicy' . For more information about policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
module Network.AWS.IAM.AttachRolePolicy
    (
    -- * Creating a request
      AttachRolePolicy (..)
    , mkAttachRolePolicy
    -- ** Request lenses
    , arpRoleName
    , arpPolicyArn

    -- * Destructuring the response
    , AttachRolePolicyResponse (..)
    , mkAttachRolePolicyResponse
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAttachRolePolicy' smart constructor.
data AttachRolePolicy = AttachRolePolicy'
  { roleName :: Types.RoleNameType
    -- ^ The name (friendly name, not ARN) of the role to attach the policy to.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
  , policyArn :: Types.ArnType
    -- ^ The Amazon Resource Name (ARN) of the IAM policy you want to attach.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttachRolePolicy' value with any optional fields omitted.
mkAttachRolePolicy
    :: Types.RoleNameType -- ^ 'roleName'
    -> Types.ArnType -- ^ 'policyArn'
    -> AttachRolePolicy
mkAttachRolePolicy roleName policyArn
  = AttachRolePolicy'{roleName, policyArn}

-- | The name (friendly name, not ARN) of the role to attach the policy to.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arpRoleName :: Lens.Lens' AttachRolePolicy Types.RoleNameType
arpRoleName = Lens.field @"roleName"
{-# INLINEABLE arpRoleName #-}
{-# DEPRECATED roleName "Use generic-lens or generic-optics with 'roleName' instead"  #-}

-- | The Amazon Resource Name (ARN) of the IAM policy you want to attach.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'policyArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arpPolicyArn :: Lens.Lens' AttachRolePolicy Types.ArnType
arpPolicyArn = Lens.field @"policyArn"
{-# INLINEABLE arpPolicyArn #-}
{-# DEPRECATED policyArn "Use generic-lens or generic-optics with 'policyArn' instead"  #-}

instance Core.ToQuery AttachRolePolicy where
        toQuery AttachRolePolicy{..}
          = Core.toQueryPair "Action" ("AttachRolePolicy" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<> Core.toQueryPair "RoleName" roleName
              Core.<> Core.toQueryPair "PolicyArn" policyArn

instance Core.ToHeaders AttachRolePolicy where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest AttachRolePolicy where
        type Rs AttachRolePolicy = AttachRolePolicyResponse
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
        parseResponse = Response.receiveNull AttachRolePolicyResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAttachRolePolicyResponse' smart constructor.
data AttachRolePolicyResponse = AttachRolePolicyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttachRolePolicyResponse' value with any optional fields omitted.
mkAttachRolePolicyResponse
    :: AttachRolePolicyResponse
mkAttachRolePolicyResponse = AttachRolePolicyResponse'
