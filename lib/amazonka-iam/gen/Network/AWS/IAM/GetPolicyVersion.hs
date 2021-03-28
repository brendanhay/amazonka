{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetPolicyVersion (..)
    , mkGetPolicyVersion
    -- ** Request lenses
    , gpvPolicyArn
    , gpvVersionId

    -- * Destructuring the response
    , GetPolicyVersionResponse (..)
    , mkGetPolicyVersionResponse
    -- ** Response lenses
    , gpvrrsPolicyVersion
    , gpvrrsResponseStatus
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetPolicyVersion' smart constructor.
data GetPolicyVersion = GetPolicyVersion'
  { policyArn :: Types.PolicyArn
    -- ^ The Amazon Resource Name (ARN) of the managed policy that you want information about.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
  , versionId :: Types.PolicyVersionIdType
    -- ^ Identifies the policy version to retrieve.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that consists of the lowercase letter 'v' followed by one or two digits, and optionally followed by a period '.' and a string of letters and digits.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetPolicyVersion' value with any optional fields omitted.
mkGetPolicyVersion
    :: Types.PolicyArn -- ^ 'policyArn'
    -> Types.PolicyVersionIdType -- ^ 'versionId'
    -> GetPolicyVersion
mkGetPolicyVersion policyArn versionId
  = GetPolicyVersion'{policyArn, versionId}

-- | The Amazon Resource Name (ARN) of the managed policy that you want information about.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'policyArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvPolicyArn :: Lens.Lens' GetPolicyVersion Types.PolicyArn
gpvPolicyArn = Lens.field @"policyArn"
{-# INLINEABLE gpvPolicyArn #-}
{-# DEPRECATED policyArn "Use generic-lens or generic-optics with 'policyArn' instead"  #-}

-- | Identifies the policy version to retrieve.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that consists of the lowercase letter 'v' followed by one or two digits, and optionally followed by a period '.' and a string of letters and digits.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvVersionId :: Lens.Lens' GetPolicyVersion Types.PolicyVersionIdType
gpvVersionId = Lens.field @"versionId"
{-# INLINEABLE gpvVersionId #-}
{-# DEPRECATED versionId "Use generic-lens or generic-optics with 'versionId' instead"  #-}

instance Core.ToQuery GetPolicyVersion where
        toQuery GetPolicyVersion{..}
          = Core.toQueryPair "Action" ("GetPolicyVersion" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<> Core.toQueryPair "PolicyArn" policyArn
              Core.<> Core.toQueryPair "VersionId" versionId

instance Core.ToHeaders GetPolicyVersion where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetPolicyVersion where
        type Rs GetPolicyVersion = GetPolicyVersionResponse
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
          = Response.receiveXMLWrapper "GetPolicyVersionResult"
              (\ s h x ->
                 GetPolicyVersionResponse' Core.<$>
                   (x Core..@? "PolicyVersion") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the response to a successful 'GetPolicyVersion' request. 
--
-- /See:/ 'mkGetPolicyVersionResponse' smart constructor.
data GetPolicyVersionResponse = GetPolicyVersionResponse'
  { policyVersion :: Core.Maybe Types.PolicyVersion
    -- ^ A structure containing details about the policy version.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetPolicyVersionResponse' value with any optional fields omitted.
mkGetPolicyVersionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetPolicyVersionResponse
mkGetPolicyVersionResponse responseStatus
  = GetPolicyVersionResponse'{policyVersion = Core.Nothing,
                              responseStatus}

-- | A structure containing details about the policy version.
--
-- /Note:/ Consider using 'policyVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvrrsPolicyVersion :: Lens.Lens' GetPolicyVersionResponse (Core.Maybe Types.PolicyVersion)
gpvrrsPolicyVersion = Lens.field @"policyVersion"
{-# INLINEABLE gpvrrsPolicyVersion #-}
{-# DEPRECATED policyVersion "Use generic-lens or generic-optics with 'policyVersion' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvrrsResponseStatus :: Lens.Lens' GetPolicyVersionResponse Core.Int
gpvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gpvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
