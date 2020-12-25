{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetUserPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the specified inline policy document that is embedded in the specified IAM user.
--
-- An IAM user can also have managed policies attached to it. To retrieve a managed policy document that is attached to a user, use 'GetPolicy' to determine the policy's default version. Then use 'GetPolicyVersion' to retrieve the policy document.
-- For more information about policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
module Network.AWS.IAM.GetUserPolicy
  ( -- * Creating a request
    GetUserPolicy (..),
    mkGetUserPolicy,

    -- ** Request lenses
    gupUserName,
    gupPolicyName,

    -- * Destructuring the response
    GetUserPolicyResponse (..),
    mkGetUserPolicyResponse,

    -- ** Response lenses
    guprrsUserName,
    guprrsPolicyName,
    guprrsPolicyDocument,
    guprrsResponseStatus,
  )
where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetUserPolicy' smart constructor.
data GetUserPolicy = GetUserPolicy'
  { -- | The name of the user who the policy is associated with.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    userName :: Types.ExistingUserNameType,
    -- | The name of the policy document to get.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    policyName :: Types.PolicyNameType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetUserPolicy' value with any optional fields omitted.
mkGetUserPolicy ::
  -- | 'userName'
  Types.ExistingUserNameType ->
  -- | 'policyName'
  Types.PolicyNameType ->
  GetUserPolicy
mkGetUserPolicy userName policyName =
  GetUserPolicy' {userName, policyName}

-- | The name of the user who the policy is associated with.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gupUserName :: Lens.Lens' GetUserPolicy Types.ExistingUserNameType
gupUserName = Lens.field @"userName"
{-# DEPRECATED gupUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The name of the policy document to get.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gupPolicyName :: Lens.Lens' GetUserPolicy Types.PolicyNameType
gupPolicyName = Lens.field @"policyName"
{-# DEPRECATED gupPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

instance Core.AWSRequest GetUserPolicy where
  type Rs GetUserPolicy = GetUserPolicyResponse
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
            ( Core.pure ("Action", "GetUserPolicy")
                Core.<> (Core.pure ("Version", "2010-05-08"))
                Core.<> (Core.toQueryValue "UserName" userName)
                Core.<> (Core.toQueryValue "PolicyName" policyName)
            )
      }
  response =
    Response.receiveXMLWrapper
      "GetUserPolicyResult"
      ( \s h x ->
          GetUserPolicyResponse'
            Core.<$> (x Core..@ "UserName")
            Core.<*> (x Core..@ "PolicyName")
            Core.<*> (x Core..@ "PolicyDocument")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the response to a successful 'GetUserPolicy' request.
--
-- /See:/ 'mkGetUserPolicyResponse' smart constructor.
data GetUserPolicyResponse = GetUserPolicyResponse'
  { -- | The user the policy is associated with.
    userName :: Types.UserName,
    -- | The name of the policy.
    policyName :: Types.PolicyName,
    -- | The policy document.
    --
    -- IAM stores policies in JSON format. However, resources that were created using AWS CloudFormation templates can be formatted in YAML. AWS CloudFormation always converts a YAML policy to JSON format before submitting it to IAM.
    policyDocument :: Types.PolicyDocument,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetUserPolicyResponse' value with any optional fields omitted.
mkGetUserPolicyResponse ::
  -- | 'userName'
  Types.UserName ->
  -- | 'policyName'
  Types.PolicyName ->
  -- | 'policyDocument'
  Types.PolicyDocument ->
  -- | 'responseStatus'
  Core.Int ->
  GetUserPolicyResponse
mkGetUserPolicyResponse
  userName
  policyName
  policyDocument
  responseStatus =
    GetUserPolicyResponse'
      { userName,
        policyName,
        policyDocument,
        responseStatus
      }

-- | The user the policy is associated with.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guprrsUserName :: Lens.Lens' GetUserPolicyResponse Types.UserName
guprrsUserName = Lens.field @"userName"
{-# DEPRECATED guprrsUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The name of the policy.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guprrsPolicyName :: Lens.Lens' GetUserPolicyResponse Types.PolicyName
guprrsPolicyName = Lens.field @"policyName"
{-# DEPRECATED guprrsPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The policy document.
--
-- IAM stores policies in JSON format. However, resources that were created using AWS CloudFormation templates can be formatted in YAML. AWS CloudFormation always converts a YAML policy to JSON format before submitting it to IAM.
--
-- /Note:/ Consider using 'policyDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guprrsPolicyDocument :: Lens.Lens' GetUserPolicyResponse Types.PolicyDocument
guprrsPolicyDocument = Lens.field @"policyDocument"
{-# DEPRECATED guprrsPolicyDocument "Use generic-lens or generic-optics with 'policyDocument' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guprrsResponseStatus :: Lens.Lens' GetUserPolicyResponse Core.Int
guprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED guprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
