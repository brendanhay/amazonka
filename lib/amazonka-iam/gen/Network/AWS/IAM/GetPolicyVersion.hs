{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetPolicyVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the specified version of the specified managed policy, including the policy document.
--
-- To list the available versions for a policy, use 'ListPolicyVersions' .
-- This API retrieves information about managed policies. To retrieve information about an inline policy that is embedded in a user, group, or role, use the 'GetUserPolicy' , 'GetGroupPolicy' , or 'GetRolePolicy' API.
-- For more information about the types of policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
-- For more information about managed policy versions, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies> in the /IAM User Guide/ .
module Network.AWS.IAM.GetPolicyVersion
  ( -- * Creating a request
    GetPolicyVersion (..),
    mkGetPolicyVersion,

    -- ** Request lenses
    gpvPolicyArn,
    gpvVersionId,

    -- * Destructuring the response
    GetPolicyVersionResponse (..),
    mkGetPolicyVersionResponse,

    -- ** Response lenses
    gpvrrsPolicyVersion,
    gpvrrsResponseStatus,
  )
where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetPolicyVersion' smart constructor.
data GetPolicyVersion = GetPolicyVersion'
  { -- | The Amazon Resource Name (ARN) of the managed policy that you want information about.
    --
    -- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
    policyArn :: Types.PolicyArn,
    -- | Identifies the policy version to retrieve.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that consists of the lowercase letter 'v' followed by one or two digits, and optionally followed by a period '.' and a string of letters and digits.
    versionId :: Types.PolicyVersionIdType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetPolicyVersion' value with any optional fields omitted.
mkGetPolicyVersion ::
  -- | 'policyArn'
  Types.PolicyArn ->
  -- | 'versionId'
  Types.PolicyVersionIdType ->
  GetPolicyVersion
mkGetPolicyVersion policyArn versionId =
  GetPolicyVersion' {policyArn, versionId}

-- | The Amazon Resource Name (ARN) of the managed policy that you want information about.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'policyArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvPolicyArn :: Lens.Lens' GetPolicyVersion Types.PolicyArn
gpvPolicyArn = Lens.field @"policyArn"
{-# DEPRECATED gpvPolicyArn "Use generic-lens or generic-optics with 'policyArn' instead." #-}

-- | Identifies the policy version to retrieve.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that consists of the lowercase letter 'v' followed by one or two digits, and optionally followed by a period '.' and a string of letters and digits.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvVersionId :: Lens.Lens' GetPolicyVersion Types.PolicyVersionIdType
gpvVersionId = Lens.field @"versionId"
{-# DEPRECATED gpvVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

instance Core.AWSRequest GetPolicyVersion where
  type Rs GetPolicyVersion = GetPolicyVersionResponse
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
            ( Core.pure ("Action", "GetPolicyVersion")
                Core.<> (Core.pure ("Version", "2010-05-08"))
                Core.<> (Core.toQueryValue "PolicyArn" policyArn)
                Core.<> (Core.toQueryValue "VersionId" versionId)
            )
      }
  response =
    Response.receiveXMLWrapper
      "GetPolicyVersionResult"
      ( \s h x ->
          GetPolicyVersionResponse'
            Core.<$> (x Core..@? "PolicyVersion")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the response to a successful 'GetPolicyVersion' request.
--
-- /See:/ 'mkGetPolicyVersionResponse' smart constructor.
data GetPolicyVersionResponse = GetPolicyVersionResponse'
  { -- | A structure containing details about the policy version.
    policyVersion :: Core.Maybe Types.PolicyVersion,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetPolicyVersionResponse' value with any optional fields omitted.
mkGetPolicyVersionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetPolicyVersionResponse
mkGetPolicyVersionResponse responseStatus =
  GetPolicyVersionResponse'
    { policyVersion = Core.Nothing,
      responseStatus
    }

-- | A structure containing details about the policy version.
--
-- /Note:/ Consider using 'policyVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvrrsPolicyVersion :: Lens.Lens' GetPolicyVersionResponse (Core.Maybe Types.PolicyVersion)
gpvrrsPolicyVersion = Lens.field @"policyVersion"
{-# DEPRECATED gpvrrsPolicyVersion "Use generic-lens or generic-optics with 'policyVersion' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvrrsResponseStatus :: Lens.Lens' GetPolicyVersionResponse Core.Int
gpvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gpvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
