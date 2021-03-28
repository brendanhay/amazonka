{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.PutRolePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates an inline policy document that is embedded in the specified IAM role.
--
-- When you embed an inline policy in a role, the inline policy is used as part of the role's access (permissions) policy. The role's trust policy is created at the same time as the role, using 'CreateRole' . You can update a role's trust policy using 'UpdateAssumeRolePolicy' . For more information about IAM roles, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/roles-toplevel.html Using Roles to Delegate Permissions and Federate Identities> .
-- A role can also have a managed policy attached to it. To attach a managed policy to a role, use 'AttachRolePolicy' . To create a new managed policy, use 'CreatePolicy' . For information about policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
-- For information about limits on the number of inline policies that you can embed with a role, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html Limitations on IAM Entities> in the /IAM User Guide/ .
module Network.AWS.IAM.PutRolePolicy
    (
    -- * Creating a request
      PutRolePolicy (..)
    , mkPutRolePolicy
    -- ** Request lenses
    , prpRoleName
    , prpPolicyName
    , prpPolicyDocument

    -- * Destructuring the response
    , PutRolePolicyResponse (..)
    , mkPutRolePolicyResponse
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutRolePolicy' smart constructor.
data PutRolePolicy = PutRolePolicy'
  { roleName :: Types.RoleName
    -- ^ The name of the role to associate the policy with.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
  , policyName :: Types.PolicyName
    -- ^ The name of the policy document.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
  , policyDocument :: Types.PolicyDocument
    -- ^ The policy document.
--
-- You must provide policies in JSON format in IAM. However, for AWS CloudFormation templates formatted in YAML, you can provide the policy in JSON or YAML format. AWS CloudFormation always converts a YAML policy to JSON format before submitting it to IAM.
-- The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:
--
--     * Any printable ASCII character ranging from the space character (@\u0020@ ) through the end of the ASCII character range
--
--
--     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through @\u00FF@ )
--
--
--     * The special characters tab (@\u0009@ ), line feed (@\u000A@ ), and carriage return (@\u000D@ )
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutRolePolicy' value with any optional fields omitted.
mkPutRolePolicy
    :: Types.RoleName -- ^ 'roleName'
    -> Types.PolicyName -- ^ 'policyName'
    -> Types.PolicyDocument -- ^ 'policyDocument'
    -> PutRolePolicy
mkPutRolePolicy roleName policyName policyDocument
  = PutRolePolicy'{roleName, policyName, policyDocument}

-- | The name of the role to associate the policy with.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prpRoleName :: Lens.Lens' PutRolePolicy Types.RoleName
prpRoleName = Lens.field @"roleName"
{-# INLINEABLE prpRoleName #-}
{-# DEPRECATED roleName "Use generic-lens or generic-optics with 'roleName' instead"  #-}

-- | The name of the policy document.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prpPolicyName :: Lens.Lens' PutRolePolicy Types.PolicyName
prpPolicyName = Lens.field @"policyName"
{-# INLINEABLE prpPolicyName #-}
{-# DEPRECATED policyName "Use generic-lens or generic-optics with 'policyName' instead"  #-}

-- | The policy document.
--
-- You must provide policies in JSON format in IAM. However, for AWS CloudFormation templates formatted in YAML, you can provide the policy in JSON or YAML format. AWS CloudFormation always converts a YAML policy to JSON format before submitting it to IAM.
-- The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:
--
--     * Any printable ASCII character ranging from the space character (@\u0020@ ) through the end of the ASCII character range
--
--
--     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through @\u00FF@ )
--
--
--     * The special characters tab (@\u0009@ ), line feed (@\u000A@ ), and carriage return (@\u000D@ )
--
--
--
-- /Note:/ Consider using 'policyDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prpPolicyDocument :: Lens.Lens' PutRolePolicy Types.PolicyDocument
prpPolicyDocument = Lens.field @"policyDocument"
{-# INLINEABLE prpPolicyDocument #-}
{-# DEPRECATED policyDocument "Use generic-lens or generic-optics with 'policyDocument' instead"  #-}

instance Core.ToQuery PutRolePolicy where
        toQuery PutRolePolicy{..}
          = Core.toQueryPair "Action" ("PutRolePolicy" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<> Core.toQueryPair "RoleName" roleName
              Core.<> Core.toQueryPair "PolicyName" policyName
              Core.<> Core.toQueryPair "PolicyDocument" policyDocument

instance Core.ToHeaders PutRolePolicy where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest PutRolePolicy where
        type Rs PutRolePolicy = PutRolePolicyResponse
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
        parseResponse = Response.receiveNull PutRolePolicyResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutRolePolicyResponse' smart constructor.
data PutRolePolicyResponse = PutRolePolicyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutRolePolicyResponse' value with any optional fields omitted.
mkPutRolePolicyResponse
    :: PutRolePolicyResponse
mkPutRolePolicyResponse = PutRolePolicyResponse'
