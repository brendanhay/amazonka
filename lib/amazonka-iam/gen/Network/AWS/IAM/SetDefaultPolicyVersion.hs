{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.SetDefaultPolicyVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the specified version of the specified policy as the policy's default (operative) version.
--
-- This operation affects all users, groups, and roles that the policy is attached to. To list the users, groups, and roles that the policy is attached to, use the 'ListEntitiesForPolicy' API.
-- For information about managed policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
module Network.AWS.IAM.SetDefaultPolicyVersion
    (
    -- * Creating a request
      SetDefaultPolicyVersion (..)
    , mkSetDefaultPolicyVersion
    -- ** Request lenses
    , sdpvPolicyArn
    , sdpvVersionId

    -- * Destructuring the response
    , SetDefaultPolicyVersionResponse (..)
    , mkSetDefaultPolicyVersionResponse
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSetDefaultPolicyVersion' smart constructor.
data SetDefaultPolicyVersion = SetDefaultPolicyVersion'
  { policyArn :: Types.PolicyArn
    -- ^ The Amazon Resource Name (ARN) of the IAM policy whose default version you want to set.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
  , versionId :: Types.PolicyVersionIdType
    -- ^ The version of the policy to set as the default (operative) version.
--
-- For more information about managed policy versions, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies> in the /IAM User Guide/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetDefaultPolicyVersion' value with any optional fields omitted.
mkSetDefaultPolicyVersion
    :: Types.PolicyArn -- ^ 'policyArn'
    -> Types.PolicyVersionIdType -- ^ 'versionId'
    -> SetDefaultPolicyVersion
mkSetDefaultPolicyVersion policyArn versionId
  = SetDefaultPolicyVersion'{policyArn, versionId}

-- | The Amazon Resource Name (ARN) of the IAM policy whose default version you want to set.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'policyArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdpvPolicyArn :: Lens.Lens' SetDefaultPolicyVersion Types.PolicyArn
sdpvPolicyArn = Lens.field @"policyArn"
{-# INLINEABLE sdpvPolicyArn #-}
{-# DEPRECATED policyArn "Use generic-lens or generic-optics with 'policyArn' instead"  #-}

-- | The version of the policy to set as the default (operative) version.
--
-- For more information about managed policy versions, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdpvVersionId :: Lens.Lens' SetDefaultPolicyVersion Types.PolicyVersionIdType
sdpvVersionId = Lens.field @"versionId"
{-# INLINEABLE sdpvVersionId #-}
{-# DEPRECATED versionId "Use generic-lens or generic-optics with 'versionId' instead"  #-}

instance Core.ToQuery SetDefaultPolicyVersion where
        toQuery SetDefaultPolicyVersion{..}
          = Core.toQueryPair "Action"
              ("SetDefaultPolicyVersion" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<> Core.toQueryPair "PolicyArn" policyArn
              Core.<> Core.toQueryPair "VersionId" versionId

instance Core.ToHeaders SetDefaultPolicyVersion where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest SetDefaultPolicyVersion where
        type Rs SetDefaultPolicyVersion = SetDefaultPolicyVersionResponse
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
          = Response.receiveNull SetDefaultPolicyVersionResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSetDefaultPolicyVersionResponse' smart constructor.
data SetDefaultPolicyVersionResponse = SetDefaultPolicyVersionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetDefaultPolicyVersionResponse' value with any optional fields omitted.
mkSetDefaultPolicyVersionResponse
    :: SetDefaultPolicyVersionResponse
mkSetDefaultPolicyVersionResponse
  = SetDefaultPolicyVersionResponse'
