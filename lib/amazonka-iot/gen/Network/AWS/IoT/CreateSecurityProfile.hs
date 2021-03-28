{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreateSecurityProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Device Defender security profile.
module Network.AWS.IoT.CreateSecurityProfile
    (
    -- * Creating a request
      CreateSecurityProfile (..)
    , mkCreateSecurityProfile
    -- ** Request lenses
    , cspSecurityProfileName
    , cspAdditionalMetricsToRetain
    , cspAdditionalMetricsToRetainV2
    , cspAlertTargets
    , cspBehaviors
    , cspSecurityProfileDescription
    , cspTags

    -- * Destructuring the response
    , CreateSecurityProfileResponse (..)
    , mkCreateSecurityProfileResponse
    -- ** Response lenses
    , csprrsSecurityProfileArn
    , csprrsSecurityProfileName
    , csprrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateSecurityProfile' smart constructor.
data CreateSecurityProfile = CreateSecurityProfile'
  { securityProfileName :: Types.SecurityProfileName
    -- ^ The name you are giving to the security profile.
  , additionalMetricsToRetain :: Core.Maybe [Types.BehaviorMetric]
    -- ^ /Please use 'CreateSecurityProfileRequest$additionalMetricsToRetainV2' instead./ 
--
-- A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's @behaviors@ , but it is also retained for any metric specified here.
  , additionalMetricsToRetainV2 :: Core.Maybe [Types.MetricToRetain]
    -- ^ A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's @behaviors@ , but it is also retained for any metric specified here.
  , alertTargets :: Core.Maybe (Core.HashMap Types.AlertTargetType Types.AlertTarget)
    -- ^ Specifies the destinations to which alerts are sent. (Alerts are always sent to the console.) Alerts are generated when a device (thing) violates a behavior.
  , behaviors :: Core.Maybe [Types.Behavior]
    -- ^ Specifies the behaviors that, when violated by a device (thing), cause an alert.
  , securityProfileDescription :: Core.Maybe Types.SecurityProfileDescription
    -- ^ A description of the security profile.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Metadata that can be used to manage the security profile.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSecurityProfile' value with any optional fields omitted.
mkCreateSecurityProfile
    :: Types.SecurityProfileName -- ^ 'securityProfileName'
    -> CreateSecurityProfile
mkCreateSecurityProfile securityProfileName
  = CreateSecurityProfile'{securityProfileName,
                           additionalMetricsToRetain = Core.Nothing,
                           additionalMetricsToRetainV2 = Core.Nothing,
                           alertTargets = Core.Nothing, behaviors = Core.Nothing,
                           securityProfileDescription = Core.Nothing, tags = Core.Nothing}

-- | The name you are giving to the security profile.
--
-- /Note:/ Consider using 'securityProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspSecurityProfileName :: Lens.Lens' CreateSecurityProfile Types.SecurityProfileName
cspSecurityProfileName = Lens.field @"securityProfileName"
{-# INLINEABLE cspSecurityProfileName #-}
{-# DEPRECATED securityProfileName "Use generic-lens or generic-optics with 'securityProfileName' instead"  #-}

-- | /Please use 'CreateSecurityProfileRequest$additionalMetricsToRetainV2' instead./ 
--
-- A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's @behaviors@ , but it is also retained for any metric specified here.
--
-- /Note:/ Consider using 'additionalMetricsToRetain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspAdditionalMetricsToRetain :: Lens.Lens' CreateSecurityProfile (Core.Maybe [Types.BehaviorMetric])
cspAdditionalMetricsToRetain = Lens.field @"additionalMetricsToRetain"
{-# INLINEABLE cspAdditionalMetricsToRetain #-}
{-# DEPRECATED additionalMetricsToRetain "Use generic-lens or generic-optics with 'additionalMetricsToRetain' instead"  #-}

-- | A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's @behaviors@ , but it is also retained for any metric specified here.
--
-- /Note:/ Consider using 'additionalMetricsToRetainV2' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspAdditionalMetricsToRetainV2 :: Lens.Lens' CreateSecurityProfile (Core.Maybe [Types.MetricToRetain])
cspAdditionalMetricsToRetainV2 = Lens.field @"additionalMetricsToRetainV2"
{-# INLINEABLE cspAdditionalMetricsToRetainV2 #-}
{-# DEPRECATED additionalMetricsToRetainV2 "Use generic-lens or generic-optics with 'additionalMetricsToRetainV2' instead"  #-}

-- | Specifies the destinations to which alerts are sent. (Alerts are always sent to the console.) Alerts are generated when a device (thing) violates a behavior.
--
-- /Note:/ Consider using 'alertTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspAlertTargets :: Lens.Lens' CreateSecurityProfile (Core.Maybe (Core.HashMap Types.AlertTargetType Types.AlertTarget))
cspAlertTargets = Lens.field @"alertTargets"
{-# INLINEABLE cspAlertTargets #-}
{-# DEPRECATED alertTargets "Use generic-lens or generic-optics with 'alertTargets' instead"  #-}

-- | Specifies the behaviors that, when violated by a device (thing), cause an alert.
--
-- /Note:/ Consider using 'behaviors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspBehaviors :: Lens.Lens' CreateSecurityProfile (Core.Maybe [Types.Behavior])
cspBehaviors = Lens.field @"behaviors"
{-# INLINEABLE cspBehaviors #-}
{-# DEPRECATED behaviors "Use generic-lens or generic-optics with 'behaviors' instead"  #-}

-- | A description of the security profile.
--
-- /Note:/ Consider using 'securityProfileDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspSecurityProfileDescription :: Lens.Lens' CreateSecurityProfile (Core.Maybe Types.SecurityProfileDescription)
cspSecurityProfileDescription = Lens.field @"securityProfileDescription"
{-# INLINEABLE cspSecurityProfileDescription #-}
{-# DEPRECATED securityProfileDescription "Use generic-lens or generic-optics with 'securityProfileDescription' instead"  #-}

-- | Metadata that can be used to manage the security profile.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspTags :: Lens.Lens' CreateSecurityProfile (Core.Maybe [Types.Tag])
cspTags = Lens.field @"tags"
{-# INLINEABLE cspTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateSecurityProfile where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateSecurityProfile where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON CreateSecurityProfile where
        toJSON CreateSecurityProfile{..}
          = Core.object
              (Core.catMaybes
                 [("additionalMetricsToRetain" Core..=) Core.<$>
                    additionalMetricsToRetain,
                  ("additionalMetricsToRetainV2" Core..=) Core.<$>
                    additionalMetricsToRetainV2,
                  ("alertTargets" Core..=) Core.<$> alertTargets,
                  ("behaviors" Core..=) Core.<$> behaviors,
                  ("securityProfileDescription" Core..=) Core.<$>
                    securityProfileDescription,
                  ("tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateSecurityProfile where
        type Rs CreateSecurityProfile = CreateSecurityProfileResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/security-profiles/" Core.<> Core.toText securityProfileName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateSecurityProfileResponse' Core.<$>
                   (x Core..:? "securityProfileArn") Core.<*>
                     x Core..:? "securityProfileName"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateSecurityProfileResponse' smart constructor.
data CreateSecurityProfileResponse = CreateSecurityProfileResponse'
  { securityProfileArn :: Core.Maybe Types.SecurityProfileArn
    -- ^ The ARN of the security profile.
  , securityProfileName :: Core.Maybe Types.SecurityProfileName
    -- ^ The name you gave to the security profile.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSecurityProfileResponse' value with any optional fields omitted.
mkCreateSecurityProfileResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateSecurityProfileResponse
mkCreateSecurityProfileResponse responseStatus
  = CreateSecurityProfileResponse'{securityProfileArn = Core.Nothing,
                                   securityProfileName = Core.Nothing, responseStatus}

-- | The ARN of the security profile.
--
-- /Note:/ Consider using 'securityProfileArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csprrsSecurityProfileArn :: Lens.Lens' CreateSecurityProfileResponse (Core.Maybe Types.SecurityProfileArn)
csprrsSecurityProfileArn = Lens.field @"securityProfileArn"
{-# INLINEABLE csprrsSecurityProfileArn #-}
{-# DEPRECATED securityProfileArn "Use generic-lens or generic-optics with 'securityProfileArn' instead"  #-}

-- | The name you gave to the security profile.
--
-- /Note:/ Consider using 'securityProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csprrsSecurityProfileName :: Lens.Lens' CreateSecurityProfileResponse (Core.Maybe Types.SecurityProfileName)
csprrsSecurityProfileName = Lens.field @"securityProfileName"
{-# INLINEABLE csprrsSecurityProfileName #-}
{-# DEPRECATED securityProfileName "Use generic-lens or generic-optics with 'securityProfileName' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csprrsResponseStatus :: Lens.Lens' CreateSecurityProfileResponse Core.Int
csprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE csprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
