{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreatePolicyVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new version of the specified AWS IoT policy. To update a policy, create a new policy version. A managed policy can have up to five versions. If the policy has five versions, you must use 'DeletePolicyVersion' to delete an existing version before you create a new one.
--
-- Optionally, you can set the new version as the policy's default version. The default version is the operative version (that is, the version that is in effect for the certificates to which the policy is attached).
module Network.AWS.IoT.CreatePolicyVersion
  ( -- * Creating a request
    CreatePolicyVersion (..),
    mkCreatePolicyVersion,

    -- ** Request lenses
    cpvPolicyName,
    cpvPolicyDocument,
    cpvSetAsDefault,

    -- * Destructuring the response
    CreatePolicyVersionResponse (..),
    mkCreatePolicyVersionResponse,

    -- ** Response lenses
    cpvrrsIsDefaultVersion,
    cpvrrsPolicyArn,
    cpvrrsPolicyDocument,
    cpvrrsPolicyVersionId,
    cpvrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the CreatePolicyVersion operation.
--
-- /See:/ 'mkCreatePolicyVersion' smart constructor.
data CreatePolicyVersion = CreatePolicyVersion'
  { -- | The policy name.
    policyName :: Types.PolicyName,
    -- | The JSON document that describes the policy. Minimum length of 1. Maximum length of 2048, excluding whitespace.
    policyDocument :: Types.PolicyDocument,
    -- | Specifies whether the policy version is set as the default. When this parameter is true, the new policy version becomes the operative version (that is, the version that is in effect for the certificates to which the policy is attached).
    setAsDefault :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePolicyVersion' value with any optional fields omitted.
mkCreatePolicyVersion ::
  -- | 'policyName'
  Types.PolicyName ->
  -- | 'policyDocument'
  Types.PolicyDocument ->
  CreatePolicyVersion
mkCreatePolicyVersion policyName policyDocument =
  CreatePolicyVersion'
    { policyName,
      policyDocument,
      setAsDefault = Core.Nothing
    }

-- | The policy name.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvPolicyName :: Lens.Lens' CreatePolicyVersion Types.PolicyName
cpvPolicyName = Lens.field @"policyName"
{-# DEPRECATED cpvPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The JSON document that describes the policy. Minimum length of 1. Maximum length of 2048, excluding whitespace.
--
-- /Note:/ Consider using 'policyDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvPolicyDocument :: Lens.Lens' CreatePolicyVersion Types.PolicyDocument
cpvPolicyDocument = Lens.field @"policyDocument"
{-# DEPRECATED cpvPolicyDocument "Use generic-lens or generic-optics with 'policyDocument' instead." #-}

-- | Specifies whether the policy version is set as the default. When this parameter is true, the new policy version becomes the operative version (that is, the version that is in effect for the certificates to which the policy is attached).
--
-- /Note:/ Consider using 'setAsDefault' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvSetAsDefault :: Lens.Lens' CreatePolicyVersion (Core.Maybe Core.Bool)
cpvSetAsDefault = Lens.field @"setAsDefault"
{-# DEPRECATED cpvSetAsDefault "Use generic-lens or generic-optics with 'setAsDefault' instead." #-}

instance Core.FromJSON CreatePolicyVersion where
  toJSON CreatePolicyVersion {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("policyDocument" Core..= policyDocument)]
      )

instance Core.AWSRequest CreatePolicyVersion where
  type Rs CreatePolicyVersion = CreatePolicyVersionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/policies/" Core.<> (Core.toText policyName)
                Core.<> ("/version")
            ),
        Core._rqQuery =
          Core.toQueryValue "setAsDefault" Core.<$> setAsDefault,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePolicyVersionResponse'
            Core.<$> (x Core..:? "isDefaultVersion")
            Core.<*> (x Core..:? "policyArn")
            Core.<*> (x Core..:? "policyDocument")
            Core.<*> (x Core..:? "policyVersionId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The output of the CreatePolicyVersion operation.
--
-- /See:/ 'mkCreatePolicyVersionResponse' smart constructor.
data CreatePolicyVersionResponse = CreatePolicyVersionResponse'
  { -- | Specifies whether the policy version is the default.
    isDefaultVersion :: Core.Maybe Core.Bool,
    -- | The policy ARN.
    policyArn :: Core.Maybe Types.PolicyArn,
    -- | The JSON document that describes the policy.
    policyDocument :: Core.Maybe Types.PolicyDocument,
    -- | The policy version ID.
    policyVersionId :: Core.Maybe Types.PolicyVersionId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePolicyVersionResponse' value with any optional fields omitted.
mkCreatePolicyVersionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreatePolicyVersionResponse
mkCreatePolicyVersionResponse responseStatus =
  CreatePolicyVersionResponse'
    { isDefaultVersion = Core.Nothing,
      policyArn = Core.Nothing,
      policyDocument = Core.Nothing,
      policyVersionId = Core.Nothing,
      responseStatus
    }

-- | Specifies whether the policy version is the default.
--
-- /Note:/ Consider using 'isDefaultVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvrrsIsDefaultVersion :: Lens.Lens' CreatePolicyVersionResponse (Core.Maybe Core.Bool)
cpvrrsIsDefaultVersion = Lens.field @"isDefaultVersion"
{-# DEPRECATED cpvrrsIsDefaultVersion "Use generic-lens or generic-optics with 'isDefaultVersion' instead." #-}

-- | The policy ARN.
--
-- /Note:/ Consider using 'policyArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvrrsPolicyArn :: Lens.Lens' CreatePolicyVersionResponse (Core.Maybe Types.PolicyArn)
cpvrrsPolicyArn = Lens.field @"policyArn"
{-# DEPRECATED cpvrrsPolicyArn "Use generic-lens or generic-optics with 'policyArn' instead." #-}

-- | The JSON document that describes the policy.
--
-- /Note:/ Consider using 'policyDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvrrsPolicyDocument :: Lens.Lens' CreatePolicyVersionResponse (Core.Maybe Types.PolicyDocument)
cpvrrsPolicyDocument = Lens.field @"policyDocument"
{-# DEPRECATED cpvrrsPolicyDocument "Use generic-lens or generic-optics with 'policyDocument' instead." #-}

-- | The policy version ID.
--
-- /Note:/ Consider using 'policyVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvrrsPolicyVersionId :: Lens.Lens' CreatePolicyVersionResponse (Core.Maybe Types.PolicyVersionId)
cpvrrsPolicyVersionId = Lens.field @"policyVersionId"
{-# DEPRECATED cpvrrsPolicyVersionId "Use generic-lens or generic-optics with 'policyVersionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvrrsResponseStatus :: Lens.Lens' CreatePolicyVersionResponse Core.Int
cpvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cpvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
