{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeletePolicyVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified version from the specified managed policy.
--
-- You cannot delete the default version from a policy using this API. To delete the default version from a policy, use 'DeletePolicy' . To find out which version of a policy is marked as the default version, use 'ListPolicyVersions' .
-- For information about versions for managed policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies> in the /IAM User Guide/ .
module Network.AWS.IAM.DeletePolicyVersion
  ( -- * Creating a request
    DeletePolicyVersion (..),
    mkDeletePolicyVersion,

    -- ** Request lenses
    dpvPolicyArn,
    dpvVersionId,

    -- * Destructuring the response
    DeletePolicyVersionResponse (..),
    mkDeletePolicyVersionResponse,
  )
where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeletePolicyVersion' smart constructor.
data DeletePolicyVersion = DeletePolicyVersion'
  { -- | The Amazon Resource Name (ARN) of the IAM policy from which you want to delete a version.
    --
    -- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
    policyArn :: Types.PolicyArn,
    -- | The policy version to delete.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that consists of the lowercase letter 'v' followed by one or two digits, and optionally followed by a period '.' and a string of letters and digits.
    -- For more information about managed policy versions, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies> in the /IAM User Guide/ .
    versionId :: Types.PolicyVersionIdType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePolicyVersion' value with any optional fields omitted.
mkDeletePolicyVersion ::
  -- | 'policyArn'
  Types.PolicyArn ->
  -- | 'versionId'
  Types.PolicyVersionIdType ->
  DeletePolicyVersion
mkDeletePolicyVersion policyArn versionId =
  DeletePolicyVersion' {policyArn, versionId}

-- | The Amazon Resource Name (ARN) of the IAM policy from which you want to delete a version.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'policyArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpvPolicyArn :: Lens.Lens' DeletePolicyVersion Types.PolicyArn
dpvPolicyArn = Lens.field @"policyArn"
{-# DEPRECATED dpvPolicyArn "Use generic-lens or generic-optics with 'policyArn' instead." #-}

-- | The policy version to delete.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that consists of the lowercase letter 'v' followed by one or two digits, and optionally followed by a period '.' and a string of letters and digits.
-- For more information about managed policy versions, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpvVersionId :: Lens.Lens' DeletePolicyVersion Types.PolicyVersionIdType
dpvVersionId = Lens.field @"versionId"
{-# DEPRECATED dpvVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

instance Core.AWSRequest DeletePolicyVersion where
  type Rs DeletePolicyVersion = DeletePolicyVersionResponse
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
            ( Core.pure ("Action", "DeletePolicyVersion")
                Core.<> (Core.pure ("Version", "2010-05-08"))
                Core.<> (Core.toQueryValue "PolicyArn" policyArn)
                Core.<> (Core.toQueryValue "VersionId" versionId)
            )
      }
  response = Response.receiveNull DeletePolicyVersionResponse'

-- | /See:/ 'mkDeletePolicyVersionResponse' smart constructor.
data DeletePolicyVersionResponse = DeletePolicyVersionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePolicyVersionResponse' value with any optional fields omitted.
mkDeletePolicyVersionResponse ::
  DeletePolicyVersionResponse
mkDeletePolicyVersionResponse = DeletePolicyVersionResponse'
