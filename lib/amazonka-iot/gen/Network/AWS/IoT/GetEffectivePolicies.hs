{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.GetEffectivePolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the policies that have an effect on the authorization behavior of the specified device when it connects to the AWS IoT device gateway.
module Network.AWS.IoT.GetEffectivePolicies
    (
    -- * Creating a request
      GetEffectivePolicies (..)
    , mkGetEffectivePolicies
    -- ** Request lenses
    , gepCognitoIdentityPoolId
    , gepPrincipal
    , gepThingName

    -- * Destructuring the response
    , GetEffectivePoliciesResponse (..)
    , mkGetEffectivePoliciesResponse
    -- ** Response lenses
    , geprrsEffectivePolicies
    , geprrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetEffectivePolicies' smart constructor.
data GetEffectivePolicies = GetEffectivePolicies'
  { cognitoIdentityPoolId :: Core.Maybe Types.CognitoIdentityPoolId
    -- ^ The Cognito identity pool ID.
  , principal :: Core.Maybe Types.Principal
    -- ^ The principal. Valid principals are CertificateArn (arn:aws:iot:/region/ :/accountId/ :cert//certificateId/ ), thingGroupArn (arn:aws:iot:/region/ :/accountId/ :thinggroup//groupName/ ) and CognitoId (/region/ :/id/ ).
  , thingName :: Core.Maybe Types.ThingName
    -- ^ The thing name.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetEffectivePolicies' value with any optional fields omitted.
mkGetEffectivePolicies
    :: GetEffectivePolicies
mkGetEffectivePolicies
  = GetEffectivePolicies'{cognitoIdentityPoolId = Core.Nothing,
                          principal = Core.Nothing, thingName = Core.Nothing}

-- | The Cognito identity pool ID.
--
-- /Note:/ Consider using 'cognitoIdentityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gepCognitoIdentityPoolId :: Lens.Lens' GetEffectivePolicies (Core.Maybe Types.CognitoIdentityPoolId)
gepCognitoIdentityPoolId = Lens.field @"cognitoIdentityPoolId"
{-# INLINEABLE gepCognitoIdentityPoolId #-}
{-# DEPRECATED cognitoIdentityPoolId "Use generic-lens or generic-optics with 'cognitoIdentityPoolId' instead"  #-}

-- | The principal. Valid principals are CertificateArn (arn:aws:iot:/region/ :/accountId/ :cert//certificateId/ ), thingGroupArn (arn:aws:iot:/region/ :/accountId/ :thinggroup//groupName/ ) and CognitoId (/region/ :/id/ ).
--
-- /Note:/ Consider using 'principal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gepPrincipal :: Lens.Lens' GetEffectivePolicies (Core.Maybe Types.Principal)
gepPrincipal = Lens.field @"principal"
{-# INLINEABLE gepPrincipal #-}
{-# DEPRECATED principal "Use generic-lens or generic-optics with 'principal' instead"  #-}

-- | The thing name.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gepThingName :: Lens.Lens' GetEffectivePolicies (Core.Maybe Types.ThingName)
gepThingName = Lens.field @"thingName"
{-# INLINEABLE gepThingName #-}
{-# DEPRECATED thingName "Use generic-lens or generic-optics with 'thingName' instead"  #-}

instance Core.ToQuery GetEffectivePolicies where
        toQuery GetEffectivePolicies{..}
          = Core.maybe Core.mempty (Core.toQueryPair "thingName") thingName

instance Core.ToHeaders GetEffectivePolicies where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON GetEffectivePolicies where
        toJSON GetEffectivePolicies{..}
          = Core.object
              (Core.catMaybes
                 [("cognitoIdentityPoolId" Core..=) Core.<$> cognitoIdentityPoolId,
                  ("principal" Core..=) Core.<$> principal])

instance Core.AWSRequest GetEffectivePolicies where
        type Rs GetEffectivePolicies = GetEffectivePoliciesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/effective-policies",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetEffectivePoliciesResponse' Core.<$>
                   (x Core..:? "effectivePolicies") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetEffectivePoliciesResponse' smart constructor.
data GetEffectivePoliciesResponse = GetEffectivePoliciesResponse'
  { effectivePolicies :: Core.Maybe [Types.EffectivePolicy]
    -- ^ The effective policies.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetEffectivePoliciesResponse' value with any optional fields omitted.
mkGetEffectivePoliciesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetEffectivePoliciesResponse
mkGetEffectivePoliciesResponse responseStatus
  = GetEffectivePoliciesResponse'{effectivePolicies = Core.Nothing,
                                  responseStatus}

-- | The effective policies.
--
-- /Note:/ Consider using 'effectivePolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geprrsEffectivePolicies :: Lens.Lens' GetEffectivePoliciesResponse (Core.Maybe [Types.EffectivePolicy])
geprrsEffectivePolicies = Lens.field @"effectivePolicies"
{-# INLINEABLE geprrsEffectivePolicies #-}
{-# DEPRECATED effectivePolicies "Use generic-lens or generic-optics with 'effectivePolicies' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geprrsResponseStatus :: Lens.Lens' GetEffectivePoliciesResponse Core.Int
geprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE geprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
