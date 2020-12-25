{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.GetPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified policy with the policy document of the default version.
module Network.AWS.IoT.GetPolicy
  ( -- * Creating a request
    GetPolicy (..),
    mkGetPolicy,

    -- ** Request lenses
    gpPolicyName,

    -- * Destructuring the response
    GetPolicyResponse (..),
    mkGetPolicyResponse,

    -- ** Response lenses
    gprrsCreationDate,
    gprrsDefaultVersionId,
    gprrsGenerationId,
    gprrsLastModifiedDate,
    gprrsPolicyArn,
    gprrsPolicyDocument,
    gprrsPolicyName,
    gprrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the GetPolicy operation.
--
-- /See:/ 'mkGetPolicy' smart constructor.
newtype GetPolicy = GetPolicy'
  { -- | The name of the policy.
    policyName :: Types.PolicyName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetPolicy' value with any optional fields omitted.
mkGetPolicy ::
  -- | 'policyName'
  Types.PolicyName ->
  GetPolicy
mkGetPolicy policyName = GetPolicy' {policyName}

-- | The name of the policy.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpPolicyName :: Lens.Lens' GetPolicy Types.PolicyName
gpPolicyName = Lens.field @"policyName"
{-# DEPRECATED gpPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

instance Core.AWSRequest GetPolicy where
  type Rs GetPolicy = GetPolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath ("/policies/" Core.<> (Core.toText policyName)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPolicyResponse'
            Core.<$> (x Core..:? "creationDate")
            Core.<*> (x Core..:? "defaultVersionId")
            Core.<*> (x Core..:? "generationId")
            Core.<*> (x Core..:? "lastModifiedDate")
            Core.<*> (x Core..:? "policyArn")
            Core.<*> (x Core..:? "policyDocument")
            Core.<*> (x Core..:? "policyName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The output from the GetPolicy operation.
--
-- /See:/ 'mkGetPolicyResponse' smart constructor.
data GetPolicyResponse = GetPolicyResponse'
  { -- | The date the policy was created.
    creationDate :: Core.Maybe Core.NominalDiffTime,
    -- | The default policy version ID.
    defaultVersionId :: Core.Maybe Types.DefaultVersionId,
    -- | The generation ID of the policy.
    generationId :: Core.Maybe Types.GenerationId,
    -- | The date the policy was last modified.
    lastModifiedDate :: Core.Maybe Core.NominalDiffTime,
    -- | The policy ARN.
    policyArn :: Core.Maybe Types.PolicyArn,
    -- | The JSON document that describes the policy.
    policyDocument :: Core.Maybe Types.PolicyDocument,
    -- | The policy name.
    policyName :: Core.Maybe Types.PolicyName,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetPolicyResponse' value with any optional fields omitted.
mkGetPolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetPolicyResponse
mkGetPolicyResponse responseStatus =
  GetPolicyResponse'
    { creationDate = Core.Nothing,
      defaultVersionId = Core.Nothing,
      generationId = Core.Nothing,
      lastModifiedDate = Core.Nothing,
      policyArn = Core.Nothing,
      policyDocument = Core.Nothing,
      policyName = Core.Nothing,
      responseStatus
    }

-- | The date the policy was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprrsCreationDate :: Lens.Lens' GetPolicyResponse (Core.Maybe Core.NominalDiffTime)
gprrsCreationDate = Lens.field @"creationDate"
{-# DEPRECATED gprrsCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The default policy version ID.
--
-- /Note:/ Consider using 'defaultVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprrsDefaultVersionId :: Lens.Lens' GetPolicyResponse (Core.Maybe Types.DefaultVersionId)
gprrsDefaultVersionId = Lens.field @"defaultVersionId"
{-# DEPRECATED gprrsDefaultVersionId "Use generic-lens or generic-optics with 'defaultVersionId' instead." #-}

-- | The generation ID of the policy.
--
-- /Note:/ Consider using 'generationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprrsGenerationId :: Lens.Lens' GetPolicyResponse (Core.Maybe Types.GenerationId)
gprrsGenerationId = Lens.field @"generationId"
{-# DEPRECATED gprrsGenerationId "Use generic-lens or generic-optics with 'generationId' instead." #-}

-- | The date the policy was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprrsLastModifiedDate :: Lens.Lens' GetPolicyResponse (Core.Maybe Core.NominalDiffTime)
gprrsLastModifiedDate = Lens.field @"lastModifiedDate"
{-# DEPRECATED gprrsLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The policy ARN.
--
-- /Note:/ Consider using 'policyArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprrsPolicyArn :: Lens.Lens' GetPolicyResponse (Core.Maybe Types.PolicyArn)
gprrsPolicyArn = Lens.field @"policyArn"
{-# DEPRECATED gprrsPolicyArn "Use generic-lens or generic-optics with 'policyArn' instead." #-}

-- | The JSON document that describes the policy.
--
-- /Note:/ Consider using 'policyDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprrsPolicyDocument :: Lens.Lens' GetPolicyResponse (Core.Maybe Types.PolicyDocument)
gprrsPolicyDocument = Lens.field @"policyDocument"
{-# DEPRECATED gprrsPolicyDocument "Use generic-lens or generic-optics with 'policyDocument' instead." #-}

-- | The policy name.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprrsPolicyName :: Lens.Lens' GetPolicyResponse (Core.Maybe Types.PolicyName)
gprrsPolicyName = Lens.field @"policyName"
{-# DEPRECATED gprrsPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprrsResponseStatus :: Lens.Lens' GetPolicyResponse Core.Int
gprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
