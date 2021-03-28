{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetPolicy (..)
    , mkGetPolicy
    -- ** Request lenses
    , gpPolicyName

    -- * Destructuring the response
    , GetPolicyResponse (..)
    , mkGetPolicyResponse
    -- ** Response lenses
    , gprrsCreationDate
    , gprrsDefaultVersionId
    , gprrsGenerationId
    , gprrsLastModifiedDate
    , gprrsPolicyArn
    , gprrsPolicyDocument
    , gprrsPolicyName
    , gprrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the GetPolicy operation.
--
-- /See:/ 'mkGetPolicy' smart constructor.
newtype GetPolicy = GetPolicy'
  { policyName :: Types.PolicyName
    -- ^ The name of the policy.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetPolicy' value with any optional fields omitted.
mkGetPolicy
    :: Types.PolicyName -- ^ 'policyName'
    -> GetPolicy
mkGetPolicy policyName = GetPolicy'{policyName}

-- | The name of the policy.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpPolicyName :: Lens.Lens' GetPolicy Types.PolicyName
gpPolicyName = Lens.field @"policyName"
{-# INLINEABLE gpPolicyName #-}
{-# DEPRECATED policyName "Use generic-lens or generic-optics with 'policyName' instead"  #-}

instance Core.ToQuery GetPolicy where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetPolicy where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetPolicy where
        type Rs GetPolicy = GetPolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/policies/" Core.<> Core.toText policyName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetPolicyResponse' Core.<$>
                   (x Core..:? "creationDate") Core.<*> x Core..:? "defaultVersionId"
                     Core.<*> x Core..:? "generationId"
                     Core.<*> x Core..:? "lastModifiedDate"
                     Core.<*> x Core..:? "policyArn"
                     Core.<*> x Core..:? "policyDocument"
                     Core.<*> x Core..:? "policyName"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The output from the GetPolicy operation.
--
-- /See:/ 'mkGetPolicyResponse' smart constructor.
data GetPolicyResponse = GetPolicyResponse'
  { creationDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date the policy was created.
  , defaultVersionId :: Core.Maybe Types.DefaultVersionId
    -- ^ The default policy version ID.
  , generationId :: Core.Maybe Types.GenerationId
    -- ^ The generation ID of the policy.
  , lastModifiedDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date the policy was last modified.
  , policyArn :: Core.Maybe Types.PolicyArn
    -- ^ The policy ARN.
  , policyDocument :: Core.Maybe Types.PolicyDocument
    -- ^ The JSON document that describes the policy.
  , policyName :: Core.Maybe Types.PolicyName
    -- ^ The policy name.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetPolicyResponse' value with any optional fields omitted.
mkGetPolicyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetPolicyResponse
mkGetPolicyResponse responseStatus
  = GetPolicyResponse'{creationDate = Core.Nothing,
                       defaultVersionId = Core.Nothing, generationId = Core.Nothing,
                       lastModifiedDate = Core.Nothing, policyArn = Core.Nothing,
                       policyDocument = Core.Nothing, policyName = Core.Nothing,
                       responseStatus}

-- | The date the policy was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprrsCreationDate :: Lens.Lens' GetPolicyResponse (Core.Maybe Core.NominalDiffTime)
gprrsCreationDate = Lens.field @"creationDate"
{-# INLINEABLE gprrsCreationDate #-}
{-# DEPRECATED creationDate "Use generic-lens or generic-optics with 'creationDate' instead"  #-}

-- | The default policy version ID.
--
-- /Note:/ Consider using 'defaultVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprrsDefaultVersionId :: Lens.Lens' GetPolicyResponse (Core.Maybe Types.DefaultVersionId)
gprrsDefaultVersionId = Lens.field @"defaultVersionId"
{-# INLINEABLE gprrsDefaultVersionId #-}
{-# DEPRECATED defaultVersionId "Use generic-lens or generic-optics with 'defaultVersionId' instead"  #-}

-- | The generation ID of the policy.
--
-- /Note:/ Consider using 'generationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprrsGenerationId :: Lens.Lens' GetPolicyResponse (Core.Maybe Types.GenerationId)
gprrsGenerationId = Lens.field @"generationId"
{-# INLINEABLE gprrsGenerationId #-}
{-# DEPRECATED generationId "Use generic-lens or generic-optics with 'generationId' instead"  #-}

-- | The date the policy was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprrsLastModifiedDate :: Lens.Lens' GetPolicyResponse (Core.Maybe Core.NominalDiffTime)
gprrsLastModifiedDate = Lens.field @"lastModifiedDate"
{-# INLINEABLE gprrsLastModifiedDate #-}
{-# DEPRECATED lastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead"  #-}

-- | The policy ARN.
--
-- /Note:/ Consider using 'policyArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprrsPolicyArn :: Lens.Lens' GetPolicyResponse (Core.Maybe Types.PolicyArn)
gprrsPolicyArn = Lens.field @"policyArn"
{-# INLINEABLE gprrsPolicyArn #-}
{-# DEPRECATED policyArn "Use generic-lens or generic-optics with 'policyArn' instead"  #-}

-- | The JSON document that describes the policy.
--
-- /Note:/ Consider using 'policyDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprrsPolicyDocument :: Lens.Lens' GetPolicyResponse (Core.Maybe Types.PolicyDocument)
gprrsPolicyDocument = Lens.field @"policyDocument"
{-# INLINEABLE gprrsPolicyDocument #-}
{-# DEPRECATED policyDocument "Use generic-lens or generic-optics with 'policyDocument' instead"  #-}

-- | The policy name.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprrsPolicyName :: Lens.Lens' GetPolicyResponse (Core.Maybe Types.PolicyName)
gprrsPolicyName = Lens.field @"policyName"
{-# INLINEABLE gprrsPolicyName #-}
{-# DEPRECATED policyName "Use generic-lens or generic-optics with 'policyName' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprrsResponseStatus :: Lens.Lens' GetPolicyResponse Core.Int
gprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
