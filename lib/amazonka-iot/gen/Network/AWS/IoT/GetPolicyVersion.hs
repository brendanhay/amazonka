{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.GetPolicyVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified policy version.
module Network.AWS.IoT.GetPolicyVersion
  ( -- * Creating a request
    GetPolicyVersion (..),
    mkGetPolicyVersion,

    -- ** Request lenses
    gpvPolicyName,
    gpvPolicyVersionId,

    -- * Destructuring the response
    GetPolicyVersionResponse (..),
    mkGetPolicyVersionResponse,

    -- ** Response lenses
    gpvrrsCreationDate,
    gpvrrsGenerationId,
    gpvrrsIsDefaultVersion,
    gpvrrsLastModifiedDate,
    gpvrrsPolicyArn,
    gpvrrsPolicyDocument,
    gpvrrsPolicyName,
    gpvrrsPolicyVersionId,
    gpvrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the GetPolicyVersion operation.
--
-- /See:/ 'mkGetPolicyVersion' smart constructor.
data GetPolicyVersion = GetPolicyVersion'
  { -- | The name of the policy.
    policyName :: Types.PolicyName,
    -- | The policy version ID.
    policyVersionId :: Types.PolicyVersionId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetPolicyVersion' value with any optional fields omitted.
mkGetPolicyVersion ::
  -- | 'policyName'
  Types.PolicyName ->
  -- | 'policyVersionId'
  Types.PolicyVersionId ->
  GetPolicyVersion
mkGetPolicyVersion policyName policyVersionId =
  GetPolicyVersion' {policyName, policyVersionId}

-- | The name of the policy.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvPolicyName :: Lens.Lens' GetPolicyVersion Types.PolicyName
gpvPolicyName = Lens.field @"policyName"
{-# DEPRECATED gpvPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The policy version ID.
--
-- /Note:/ Consider using 'policyVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvPolicyVersionId :: Lens.Lens' GetPolicyVersion Types.PolicyVersionId
gpvPolicyVersionId = Lens.field @"policyVersionId"
{-# DEPRECATED gpvPolicyVersionId "Use generic-lens or generic-optics with 'policyVersionId' instead." #-}

instance Core.AWSRequest GetPolicyVersion where
  type Rs GetPolicyVersion = GetPolicyVersionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/policies/" Core.<> (Core.toText policyName)
                Core.<> ("/version/")
                Core.<> (Core.toText policyVersionId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPolicyVersionResponse'
            Core.<$> (x Core..:? "creationDate")
            Core.<*> (x Core..:? "generationId")
            Core.<*> (x Core..:? "isDefaultVersion")
            Core.<*> (x Core..:? "lastModifiedDate")
            Core.<*> (x Core..:? "policyArn")
            Core.<*> (x Core..:? "policyDocument")
            Core.<*> (x Core..:? "policyName")
            Core.<*> (x Core..:? "policyVersionId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The output from the GetPolicyVersion operation.
--
-- /See:/ 'mkGetPolicyVersionResponse' smart constructor.
data GetPolicyVersionResponse = GetPolicyVersionResponse'
  { -- | The date the policy was created.
    creationDate :: Core.Maybe Core.NominalDiffTime,
    -- | The generation ID of the policy version.
    generationId :: Core.Maybe Types.GenerationId,
    -- | Specifies whether the policy version is the default.
    isDefaultVersion :: Core.Maybe Core.Bool,
    -- | The date the policy was last modified.
    lastModifiedDate :: Core.Maybe Core.NominalDiffTime,
    -- | The policy ARN.
    policyArn :: Core.Maybe Types.PolicyArn,
    -- | The JSON document that describes the policy.
    policyDocument :: Core.Maybe Types.PolicyDocument,
    -- | The policy name.
    policyName :: Core.Maybe Types.PolicyName,
    -- | The policy version ID.
    policyVersionId :: Core.Maybe Types.PolicyVersionId,
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
    { creationDate = Core.Nothing,
      generationId = Core.Nothing,
      isDefaultVersion = Core.Nothing,
      lastModifiedDate = Core.Nothing,
      policyArn = Core.Nothing,
      policyDocument = Core.Nothing,
      policyName = Core.Nothing,
      policyVersionId = Core.Nothing,
      responseStatus
    }

-- | The date the policy was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvrrsCreationDate :: Lens.Lens' GetPolicyVersionResponse (Core.Maybe Core.NominalDiffTime)
gpvrrsCreationDate = Lens.field @"creationDate"
{-# DEPRECATED gpvrrsCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The generation ID of the policy version.
--
-- /Note:/ Consider using 'generationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvrrsGenerationId :: Lens.Lens' GetPolicyVersionResponse (Core.Maybe Types.GenerationId)
gpvrrsGenerationId = Lens.field @"generationId"
{-# DEPRECATED gpvrrsGenerationId "Use generic-lens or generic-optics with 'generationId' instead." #-}

-- | Specifies whether the policy version is the default.
--
-- /Note:/ Consider using 'isDefaultVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvrrsIsDefaultVersion :: Lens.Lens' GetPolicyVersionResponse (Core.Maybe Core.Bool)
gpvrrsIsDefaultVersion = Lens.field @"isDefaultVersion"
{-# DEPRECATED gpvrrsIsDefaultVersion "Use generic-lens or generic-optics with 'isDefaultVersion' instead." #-}

-- | The date the policy was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvrrsLastModifiedDate :: Lens.Lens' GetPolicyVersionResponse (Core.Maybe Core.NominalDiffTime)
gpvrrsLastModifiedDate = Lens.field @"lastModifiedDate"
{-# DEPRECATED gpvrrsLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The policy ARN.
--
-- /Note:/ Consider using 'policyArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvrrsPolicyArn :: Lens.Lens' GetPolicyVersionResponse (Core.Maybe Types.PolicyArn)
gpvrrsPolicyArn = Lens.field @"policyArn"
{-# DEPRECATED gpvrrsPolicyArn "Use generic-lens or generic-optics with 'policyArn' instead." #-}

-- | The JSON document that describes the policy.
--
-- /Note:/ Consider using 'policyDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvrrsPolicyDocument :: Lens.Lens' GetPolicyVersionResponse (Core.Maybe Types.PolicyDocument)
gpvrrsPolicyDocument = Lens.field @"policyDocument"
{-# DEPRECATED gpvrrsPolicyDocument "Use generic-lens or generic-optics with 'policyDocument' instead." #-}

-- | The policy name.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvrrsPolicyName :: Lens.Lens' GetPolicyVersionResponse (Core.Maybe Types.PolicyName)
gpvrrsPolicyName = Lens.field @"policyName"
{-# DEPRECATED gpvrrsPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The policy version ID.
--
-- /Note:/ Consider using 'policyVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvrrsPolicyVersionId :: Lens.Lens' GetPolicyVersionResponse (Core.Maybe Types.PolicyVersionId)
gpvrrsPolicyVersionId = Lens.field @"policyVersionId"
{-# DEPRECATED gpvrrsPolicyVersionId "Use generic-lens or generic-optics with 'policyVersionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvrrsResponseStatus :: Lens.Lens' GetPolicyVersionResponse Core.Int
gpvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gpvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
