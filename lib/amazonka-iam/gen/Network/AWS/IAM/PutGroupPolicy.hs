{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.PutGroupPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates an inline policy document that is embedded in the specified IAM group.
--
-- A user can also have managed policies attached to it. To attach a managed policy to a group, use 'AttachGroupPolicy' . To create a new managed policy, use 'CreatePolicy' . For information about policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
-- For information about limits on the number of inline policies that you can embed in a group, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html Limitations on IAM Entities> in the /IAM User Guide/ .
module Network.AWS.IAM.PutGroupPolicy
  ( -- * Creating a request
    PutGroupPolicy (..),
    mkPutGroupPolicy,

    -- ** Request lenses
    pgpGroupName,
    pgpPolicyName,
    pgpPolicyDocument,

    -- * Destructuring the response
    PutGroupPolicyResponse (..),
    mkPutGroupPolicyResponse,
  )
where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutGroupPolicy' smart constructor.
data PutGroupPolicy = PutGroupPolicy'
  { -- | The name of the group to associate the policy with.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-.
    groupName :: Types.GroupName,
    -- | The name of the policy document.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    policyName :: Types.PolicyName,
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
    policyDocument :: Types.PolicyDocument
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutGroupPolicy' value with any optional fields omitted.
mkPutGroupPolicy ::
  -- | 'groupName'
  Types.GroupName ->
  -- | 'policyName'
  Types.PolicyName ->
  -- | 'policyDocument'
  Types.PolicyDocument ->
  PutGroupPolicy
mkPutGroupPolicy groupName policyName policyDocument =
  PutGroupPolicy' {groupName, policyName, policyDocument}

-- | The name of the group to associate the policy with.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgpGroupName :: Lens.Lens' PutGroupPolicy Types.GroupName
pgpGroupName = Lens.field @"groupName"
{-# DEPRECATED pgpGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | The name of the policy document.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgpPolicyName :: Lens.Lens' PutGroupPolicy Types.PolicyName
pgpPolicyName = Lens.field @"policyName"
{-# DEPRECATED pgpPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

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
pgpPolicyDocument :: Lens.Lens' PutGroupPolicy Types.PolicyDocument
pgpPolicyDocument = Lens.field @"policyDocument"
{-# DEPRECATED pgpPolicyDocument "Use generic-lens or generic-optics with 'policyDocument' instead." #-}

instance Core.AWSRequest PutGroupPolicy where
  type Rs PutGroupPolicy = PutGroupPolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "PutGroupPolicy")
                Core.<> (Core.pure ("Version", "2010-05-08"))
                Core.<> (Core.toQueryValue "GroupName" groupName)
                Core.<> (Core.toQueryValue "PolicyName" policyName)
                Core.<> (Core.toQueryValue "PolicyDocument" policyDocument)
            )
      }
  response = Response.receiveNull PutGroupPolicyResponse'

-- | /See:/ 'mkPutGroupPolicyResponse' smart constructor.
data PutGroupPolicyResponse = PutGroupPolicyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutGroupPolicyResponse' value with any optional fields omitted.
mkPutGroupPolicyResponse ::
  PutGroupPolicyResponse
mkPutGroupPolicyResponse = PutGroupPolicyResponse'
