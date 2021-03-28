{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreatePolicyVersion (..)
    , mkCreatePolicyVersion
    -- ** Request lenses
    , cpvPolicyName
    , cpvPolicyDocument
    , cpvSetAsDefault

    -- * Destructuring the response
    , CreatePolicyVersionResponse (..)
    , mkCreatePolicyVersionResponse
    -- ** Response lenses
    , cpvrrsIsDefaultVersion
    , cpvrrsPolicyArn
    , cpvrrsPolicyDocument
    , cpvrrsPolicyVersionId
    , cpvrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the CreatePolicyVersion operation.
--
-- /See:/ 'mkCreatePolicyVersion' smart constructor.
data CreatePolicyVersion = CreatePolicyVersion'
  { policyName :: Types.PolicyName
    -- ^ The policy name.
  , policyDocument :: Types.PolicyDocument
    -- ^ The JSON document that describes the policy. Minimum length of 1. Maximum length of 2048, excluding whitespace.
  , setAsDefault :: Core.Maybe Core.Bool
    -- ^ Specifies whether the policy version is set as the default. When this parameter is true, the new policy version becomes the operative version (that is, the version that is in effect for the certificates to which the policy is attached).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePolicyVersion' value with any optional fields omitted.
mkCreatePolicyVersion
    :: Types.PolicyName -- ^ 'policyName'
    -> Types.PolicyDocument -- ^ 'policyDocument'
    -> CreatePolicyVersion
mkCreatePolicyVersion policyName policyDocument
  = CreatePolicyVersion'{policyName, policyDocument,
                         setAsDefault = Core.Nothing}

-- | The policy name.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvPolicyName :: Lens.Lens' CreatePolicyVersion Types.PolicyName
cpvPolicyName = Lens.field @"policyName"
{-# INLINEABLE cpvPolicyName #-}
{-# DEPRECATED policyName "Use generic-lens or generic-optics with 'policyName' instead"  #-}

-- | The JSON document that describes the policy. Minimum length of 1. Maximum length of 2048, excluding whitespace.
--
-- /Note:/ Consider using 'policyDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvPolicyDocument :: Lens.Lens' CreatePolicyVersion Types.PolicyDocument
cpvPolicyDocument = Lens.field @"policyDocument"
{-# INLINEABLE cpvPolicyDocument #-}
{-# DEPRECATED policyDocument "Use generic-lens or generic-optics with 'policyDocument' instead"  #-}

-- | Specifies whether the policy version is set as the default. When this parameter is true, the new policy version becomes the operative version (that is, the version that is in effect for the certificates to which the policy is attached).
--
-- /Note:/ Consider using 'setAsDefault' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvSetAsDefault :: Lens.Lens' CreatePolicyVersion (Core.Maybe Core.Bool)
cpvSetAsDefault = Lens.field @"setAsDefault"
{-# INLINEABLE cpvSetAsDefault #-}
{-# DEPRECATED setAsDefault "Use generic-lens or generic-optics with 'setAsDefault' instead"  #-}

instance Core.ToQuery CreatePolicyVersion where
        toQuery CreatePolicyVersion{..}
          = Core.maybe Core.mempty (Core.toQueryPair "setAsDefault")
              setAsDefault

instance Core.ToHeaders CreatePolicyVersion where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON CreatePolicyVersion where
        toJSON CreatePolicyVersion{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("policyDocument" Core..= policyDocument)])

instance Core.AWSRequest CreatePolicyVersion where
        type Rs CreatePolicyVersion = CreatePolicyVersionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/policies/" Core.<> Core.toText policyName Core.<> "/version",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreatePolicyVersionResponse' Core.<$>
                   (x Core..:? "isDefaultVersion") Core.<*> x Core..:? "policyArn"
                     Core.<*> x Core..:? "policyDocument"
                     Core.<*> x Core..:? "policyVersionId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The output of the CreatePolicyVersion operation.
--
-- /See:/ 'mkCreatePolicyVersionResponse' smart constructor.
data CreatePolicyVersionResponse = CreatePolicyVersionResponse'
  { isDefaultVersion :: Core.Maybe Core.Bool
    -- ^ Specifies whether the policy version is the default.
  , policyArn :: Core.Maybe Types.PolicyArn
    -- ^ The policy ARN.
  , policyDocument :: Core.Maybe Types.PolicyDocument
    -- ^ The JSON document that describes the policy.
  , policyVersionId :: Core.Maybe Types.PolicyVersionId
    -- ^ The policy version ID.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePolicyVersionResponse' value with any optional fields omitted.
mkCreatePolicyVersionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreatePolicyVersionResponse
mkCreatePolicyVersionResponse responseStatus
  = CreatePolicyVersionResponse'{isDefaultVersion = Core.Nothing,
                                 policyArn = Core.Nothing, policyDocument = Core.Nothing,
                                 policyVersionId = Core.Nothing, responseStatus}

-- | Specifies whether the policy version is the default.
--
-- /Note:/ Consider using 'isDefaultVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvrrsIsDefaultVersion :: Lens.Lens' CreatePolicyVersionResponse (Core.Maybe Core.Bool)
cpvrrsIsDefaultVersion = Lens.field @"isDefaultVersion"
{-# INLINEABLE cpvrrsIsDefaultVersion #-}
{-# DEPRECATED isDefaultVersion "Use generic-lens or generic-optics with 'isDefaultVersion' instead"  #-}

-- | The policy ARN.
--
-- /Note:/ Consider using 'policyArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvrrsPolicyArn :: Lens.Lens' CreatePolicyVersionResponse (Core.Maybe Types.PolicyArn)
cpvrrsPolicyArn = Lens.field @"policyArn"
{-# INLINEABLE cpvrrsPolicyArn #-}
{-# DEPRECATED policyArn "Use generic-lens or generic-optics with 'policyArn' instead"  #-}

-- | The JSON document that describes the policy.
--
-- /Note:/ Consider using 'policyDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvrrsPolicyDocument :: Lens.Lens' CreatePolicyVersionResponse (Core.Maybe Types.PolicyDocument)
cpvrrsPolicyDocument = Lens.field @"policyDocument"
{-# INLINEABLE cpvrrsPolicyDocument #-}
{-# DEPRECATED policyDocument "Use generic-lens or generic-optics with 'policyDocument' instead"  #-}

-- | The policy version ID.
--
-- /Note:/ Consider using 'policyVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvrrsPolicyVersionId :: Lens.Lens' CreatePolicyVersionResponse (Core.Maybe Types.PolicyVersionId)
cpvrrsPolicyVersionId = Lens.field @"policyVersionId"
{-# INLINEABLE cpvrrsPolicyVersionId #-}
{-# DEPRECATED policyVersionId "Use generic-lens or generic-optics with 'policyVersionId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvrrsResponseStatus :: Lens.Lens' CreatePolicyVersionResponse Core.Int
cpvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cpvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
