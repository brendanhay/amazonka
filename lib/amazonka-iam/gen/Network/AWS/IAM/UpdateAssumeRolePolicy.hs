{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.UpdateAssumeRolePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the policy that grants an IAM entity permission to assume a role. This is typically referred to as the "role trust policy". For more information about roles, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/roles-toplevel.html Using Roles to Delegate Permissions and Federate Identities> .
module Network.AWS.IAM.UpdateAssumeRolePolicy
    (
    -- * Creating a request
      UpdateAssumeRolePolicy (..)
    , mkUpdateAssumeRolePolicy
    -- ** Request lenses
    , uarpRoleName
    , uarpPolicyDocument

    -- * Destructuring the response
    , UpdateAssumeRolePolicyResponse (..)
    , mkUpdateAssumeRolePolicyResponse
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateAssumeRolePolicy' smart constructor.
data UpdateAssumeRolePolicy = UpdateAssumeRolePolicy'
  { roleName :: Types.RoleName
    -- ^ The name of the role to update with the new policy.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
  , policyDocument :: Types.PolicyDocument
    -- ^ The policy that grants an entity permission to assume the role.
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

-- | Creates a 'UpdateAssumeRolePolicy' value with any optional fields omitted.
mkUpdateAssumeRolePolicy
    :: Types.RoleName -- ^ 'roleName'
    -> Types.PolicyDocument -- ^ 'policyDocument'
    -> UpdateAssumeRolePolicy
mkUpdateAssumeRolePolicy roleName policyDocument
  = UpdateAssumeRolePolicy'{roleName, policyDocument}

-- | The name of the role to update with the new policy.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarpRoleName :: Lens.Lens' UpdateAssumeRolePolicy Types.RoleName
uarpRoleName = Lens.field @"roleName"
{-# INLINEABLE uarpRoleName #-}
{-# DEPRECATED roleName "Use generic-lens or generic-optics with 'roleName' instead"  #-}

-- | The policy that grants an entity permission to assume the role.
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
uarpPolicyDocument :: Lens.Lens' UpdateAssumeRolePolicy Types.PolicyDocument
uarpPolicyDocument = Lens.field @"policyDocument"
{-# INLINEABLE uarpPolicyDocument #-}
{-# DEPRECATED policyDocument "Use generic-lens or generic-optics with 'policyDocument' instead"  #-}

instance Core.ToQuery UpdateAssumeRolePolicy where
        toQuery UpdateAssumeRolePolicy{..}
          = Core.toQueryPair "Action" ("UpdateAssumeRolePolicy" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<> Core.toQueryPair "RoleName" roleName
              Core.<> Core.toQueryPair "PolicyDocument" policyDocument

instance Core.ToHeaders UpdateAssumeRolePolicy where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest UpdateAssumeRolePolicy where
        type Rs UpdateAssumeRolePolicy = UpdateAssumeRolePolicyResponse
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
          = Response.receiveNull UpdateAssumeRolePolicyResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateAssumeRolePolicyResponse' smart constructor.
data UpdateAssumeRolePolicyResponse = UpdateAssumeRolePolicyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateAssumeRolePolicyResponse' value with any optional fields omitted.
mkUpdateAssumeRolePolicyResponse
    :: UpdateAssumeRolePolicyResponse
mkUpdateAssumeRolePolicyResponse = UpdateAssumeRolePolicyResponse'
