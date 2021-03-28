{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeSecurityProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a Device Defender security profile.
module Network.AWS.IoT.DescribeSecurityProfile
    (
    -- * Creating a request
      DescribeSecurityProfile (..)
    , mkDescribeSecurityProfile
    -- ** Request lenses
    , dSecurityProfileName

    -- * Destructuring the response
    , DescribeSecurityProfileResponse (..)
    , mkDescribeSecurityProfileResponse
    -- ** Response lenses
    , dsprfrsAdditionalMetricsToRetain
    , dsprfrsAdditionalMetricsToRetainV2
    , dsprfrsAlertTargets
    , dsprfrsBehaviors
    , dsprfrsCreationDate
    , dsprfrsLastModifiedDate
    , dsprfrsSecurityProfileArn
    , dsprfrsSecurityProfileDescription
    , dsprfrsSecurityProfileName
    , dsprfrsVersion
    , dsprfrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeSecurityProfile' smart constructor.
newtype DescribeSecurityProfile = DescribeSecurityProfile'
  { securityProfileName :: Types.SecurityProfileName
    -- ^ The name of the security profile whose information you want to get.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSecurityProfile' value with any optional fields omitted.
mkDescribeSecurityProfile
    :: Types.SecurityProfileName -- ^ 'securityProfileName'
    -> DescribeSecurityProfile
mkDescribeSecurityProfile securityProfileName
  = DescribeSecurityProfile'{securityProfileName}

-- | The name of the security profile whose information you want to get.
--
-- /Note:/ Consider using 'securityProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dSecurityProfileName :: Lens.Lens' DescribeSecurityProfile Types.SecurityProfileName
dSecurityProfileName = Lens.field @"securityProfileName"
{-# INLINEABLE dSecurityProfileName #-}
{-# DEPRECATED securityProfileName "Use generic-lens or generic-optics with 'securityProfileName' instead"  #-}

instance Core.ToQuery DescribeSecurityProfile where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeSecurityProfile where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeSecurityProfile where
        type Rs DescribeSecurityProfile = DescribeSecurityProfileResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/security-profiles/" Core.<> Core.toText securityProfileName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeSecurityProfileResponse' Core.<$>
                   (x Core..:? "additionalMetricsToRetain") Core.<*>
                     x Core..:? "additionalMetricsToRetainV2"
                     Core.<*> x Core..:? "alertTargets"
                     Core.<*> x Core..:? "behaviors"
                     Core.<*> x Core..:? "creationDate"
                     Core.<*> x Core..:? "lastModifiedDate"
                     Core.<*> x Core..:? "securityProfileArn"
                     Core.<*> x Core..:? "securityProfileDescription"
                     Core.<*> x Core..:? "securityProfileName"
                     Core.<*> x Core..:? "version"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeSecurityProfileResponse' smart constructor.
data DescribeSecurityProfileResponse = DescribeSecurityProfileResponse'
  { additionalMetricsToRetain :: Core.Maybe [Types.BehaviorMetric]
    -- ^ /Please use 'DescribeSecurityProfileResponse$additionalMetricsToRetainV2' instead./ 
--
-- A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's @behaviors@ , but it is also retained for any metric specified here.
  , additionalMetricsToRetainV2 :: Core.Maybe [Types.MetricToRetain]
    -- ^ A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's behaviors, but it is also retained for any metric specified here.
  , alertTargets :: Core.Maybe (Core.HashMap Types.AlertTargetType Types.AlertTarget)
    -- ^ Where the alerts are sent. (Alerts are always sent to the console.)
  , behaviors :: Core.Maybe [Types.Behavior]
    -- ^ Specifies the behaviors that, when violated by a device (thing), cause an alert.
  , creationDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The time the security profile was created.
  , lastModifiedDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The time the security profile was last modified.
  , securityProfileArn :: Core.Maybe Types.SecurityProfileArn
    -- ^ The ARN of the security profile.
  , securityProfileDescription :: Core.Maybe Types.SecurityProfileDescription
    -- ^ A description of the security profile (associated with the security profile when it was created or updated).
  , securityProfileName :: Core.Maybe Types.SecurityProfileName
    -- ^ The name of the security profile.
  , version :: Core.Maybe Core.Integer
    -- ^ The version of the security profile. A new version is generated whenever the security profile is updated.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeSecurityProfileResponse' value with any optional fields omitted.
mkDescribeSecurityProfileResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeSecurityProfileResponse
mkDescribeSecurityProfileResponse responseStatus
  = DescribeSecurityProfileResponse'{additionalMetricsToRetain =
                                       Core.Nothing,
                                     additionalMetricsToRetainV2 = Core.Nothing,
                                     alertTargets = Core.Nothing, behaviors = Core.Nothing,
                                     creationDate = Core.Nothing, lastModifiedDate = Core.Nothing,
                                     securityProfileArn = Core.Nothing,
                                     securityProfileDescription = Core.Nothing,
                                     securityProfileName = Core.Nothing, version = Core.Nothing,
                                     responseStatus}

-- | /Please use 'DescribeSecurityProfileResponse$additionalMetricsToRetainV2' instead./ 
--
-- A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's @behaviors@ , but it is also retained for any metric specified here.
--
-- /Note:/ Consider using 'additionalMetricsToRetain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprfrsAdditionalMetricsToRetain :: Lens.Lens' DescribeSecurityProfileResponse (Core.Maybe [Types.BehaviorMetric])
dsprfrsAdditionalMetricsToRetain = Lens.field @"additionalMetricsToRetain"
{-# INLINEABLE dsprfrsAdditionalMetricsToRetain #-}
{-# DEPRECATED additionalMetricsToRetain "Use generic-lens or generic-optics with 'additionalMetricsToRetain' instead"  #-}

-- | A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's behaviors, but it is also retained for any metric specified here.
--
-- /Note:/ Consider using 'additionalMetricsToRetainV2' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprfrsAdditionalMetricsToRetainV2 :: Lens.Lens' DescribeSecurityProfileResponse (Core.Maybe [Types.MetricToRetain])
dsprfrsAdditionalMetricsToRetainV2 = Lens.field @"additionalMetricsToRetainV2"
{-# INLINEABLE dsprfrsAdditionalMetricsToRetainV2 #-}
{-# DEPRECATED additionalMetricsToRetainV2 "Use generic-lens or generic-optics with 'additionalMetricsToRetainV2' instead"  #-}

-- | Where the alerts are sent. (Alerts are always sent to the console.)
--
-- /Note:/ Consider using 'alertTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprfrsAlertTargets :: Lens.Lens' DescribeSecurityProfileResponse (Core.Maybe (Core.HashMap Types.AlertTargetType Types.AlertTarget))
dsprfrsAlertTargets = Lens.field @"alertTargets"
{-# INLINEABLE dsprfrsAlertTargets #-}
{-# DEPRECATED alertTargets "Use generic-lens or generic-optics with 'alertTargets' instead"  #-}

-- | Specifies the behaviors that, when violated by a device (thing), cause an alert.
--
-- /Note:/ Consider using 'behaviors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprfrsBehaviors :: Lens.Lens' DescribeSecurityProfileResponse (Core.Maybe [Types.Behavior])
dsprfrsBehaviors = Lens.field @"behaviors"
{-# INLINEABLE dsprfrsBehaviors #-}
{-# DEPRECATED behaviors "Use generic-lens or generic-optics with 'behaviors' instead"  #-}

-- | The time the security profile was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprfrsCreationDate :: Lens.Lens' DescribeSecurityProfileResponse (Core.Maybe Core.NominalDiffTime)
dsprfrsCreationDate = Lens.field @"creationDate"
{-# INLINEABLE dsprfrsCreationDate #-}
{-# DEPRECATED creationDate "Use generic-lens or generic-optics with 'creationDate' instead"  #-}

-- | The time the security profile was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprfrsLastModifiedDate :: Lens.Lens' DescribeSecurityProfileResponse (Core.Maybe Core.NominalDiffTime)
dsprfrsLastModifiedDate = Lens.field @"lastModifiedDate"
{-# INLINEABLE dsprfrsLastModifiedDate #-}
{-# DEPRECATED lastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead"  #-}

-- | The ARN of the security profile.
--
-- /Note:/ Consider using 'securityProfileArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprfrsSecurityProfileArn :: Lens.Lens' DescribeSecurityProfileResponse (Core.Maybe Types.SecurityProfileArn)
dsprfrsSecurityProfileArn = Lens.field @"securityProfileArn"
{-# INLINEABLE dsprfrsSecurityProfileArn #-}
{-# DEPRECATED securityProfileArn "Use generic-lens or generic-optics with 'securityProfileArn' instead"  #-}

-- | A description of the security profile (associated with the security profile when it was created or updated).
--
-- /Note:/ Consider using 'securityProfileDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprfrsSecurityProfileDescription :: Lens.Lens' DescribeSecurityProfileResponse (Core.Maybe Types.SecurityProfileDescription)
dsprfrsSecurityProfileDescription = Lens.field @"securityProfileDescription"
{-# INLINEABLE dsprfrsSecurityProfileDescription #-}
{-# DEPRECATED securityProfileDescription "Use generic-lens or generic-optics with 'securityProfileDescription' instead"  #-}

-- | The name of the security profile.
--
-- /Note:/ Consider using 'securityProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprfrsSecurityProfileName :: Lens.Lens' DescribeSecurityProfileResponse (Core.Maybe Types.SecurityProfileName)
dsprfrsSecurityProfileName = Lens.field @"securityProfileName"
{-# INLINEABLE dsprfrsSecurityProfileName #-}
{-# DEPRECATED securityProfileName "Use generic-lens or generic-optics with 'securityProfileName' instead"  #-}

-- | The version of the security profile. A new version is generated whenever the security profile is updated.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprfrsVersion :: Lens.Lens' DescribeSecurityProfileResponse (Core.Maybe Core.Integer)
dsprfrsVersion = Lens.field @"version"
{-# INLINEABLE dsprfrsVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprfrsResponseStatus :: Lens.Lens' DescribeSecurityProfileResponse Core.Int
dsprfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsprfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
