{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateSecurityProfile (..),
    mkCreateSecurityProfile,

    -- ** Request lenses
    cspSecurityProfileName,
    cspAdditionalMetricsToRetain,
    cspAdditionalMetricsToRetainV2,
    cspAlertTargets,
    cspBehaviors,
    cspSecurityProfileDescription,
    cspTags,

    -- * Destructuring the response
    CreateSecurityProfileResponse (..),
    mkCreateSecurityProfileResponse,

    -- ** Response lenses
    csprrsSecurityProfileArn,
    csprrsSecurityProfileName,
    csprrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateSecurityProfile' smart constructor.
data CreateSecurityProfile = CreateSecurityProfile'
  { -- | The name you are giving to the security profile.
    securityProfileName :: Types.SecurityProfileName,
    -- | /Please use 'CreateSecurityProfileRequest$additionalMetricsToRetainV2' instead./
    --
    -- A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's @behaviors@ , but it is also retained for any metric specified here.
    additionalMetricsToRetain :: Core.Maybe [Types.BehaviorMetric],
    -- | A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's @behaviors@ , but it is also retained for any metric specified here.
    additionalMetricsToRetainV2 :: Core.Maybe [Types.MetricToRetain],
    -- | Specifies the destinations to which alerts are sent. (Alerts are always sent to the console.) Alerts are generated when a device (thing) violates a behavior.
    alertTargets :: Core.Maybe (Core.HashMap Types.AlertTargetType Types.AlertTarget),
    -- | Specifies the behaviors that, when violated by a device (thing), cause an alert.
    behaviors :: Core.Maybe [Types.Behavior],
    -- | A description of the security profile.
    securityProfileDescription :: Core.Maybe Types.SecurityProfileDescription,
    -- | Metadata that can be used to manage the security profile.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSecurityProfile' value with any optional fields omitted.
mkCreateSecurityProfile ::
  -- | 'securityProfileName'
  Types.SecurityProfileName ->
  CreateSecurityProfile
mkCreateSecurityProfile securityProfileName =
  CreateSecurityProfile'
    { securityProfileName,
      additionalMetricsToRetain = Core.Nothing,
      additionalMetricsToRetainV2 = Core.Nothing,
      alertTargets = Core.Nothing,
      behaviors = Core.Nothing,
      securityProfileDescription = Core.Nothing,
      tags = Core.Nothing
    }

-- | The name you are giving to the security profile.
--
-- /Note:/ Consider using 'securityProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspSecurityProfileName :: Lens.Lens' CreateSecurityProfile Types.SecurityProfileName
cspSecurityProfileName = Lens.field @"securityProfileName"
{-# DEPRECATED cspSecurityProfileName "Use generic-lens or generic-optics with 'securityProfileName' instead." #-}

-- | /Please use 'CreateSecurityProfileRequest$additionalMetricsToRetainV2' instead./
--
-- A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's @behaviors@ , but it is also retained for any metric specified here.
--
-- /Note:/ Consider using 'additionalMetricsToRetain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspAdditionalMetricsToRetain :: Lens.Lens' CreateSecurityProfile (Core.Maybe [Types.BehaviorMetric])
cspAdditionalMetricsToRetain = Lens.field @"additionalMetricsToRetain"
{-# DEPRECATED cspAdditionalMetricsToRetain "Use generic-lens or generic-optics with 'additionalMetricsToRetain' instead." #-}

-- | A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's @behaviors@ , but it is also retained for any metric specified here.
--
-- /Note:/ Consider using 'additionalMetricsToRetainV2' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspAdditionalMetricsToRetainV2 :: Lens.Lens' CreateSecurityProfile (Core.Maybe [Types.MetricToRetain])
cspAdditionalMetricsToRetainV2 = Lens.field @"additionalMetricsToRetainV2"
{-# DEPRECATED cspAdditionalMetricsToRetainV2 "Use generic-lens or generic-optics with 'additionalMetricsToRetainV2' instead." #-}

-- | Specifies the destinations to which alerts are sent. (Alerts are always sent to the console.) Alerts are generated when a device (thing) violates a behavior.
--
-- /Note:/ Consider using 'alertTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspAlertTargets :: Lens.Lens' CreateSecurityProfile (Core.Maybe (Core.HashMap Types.AlertTargetType Types.AlertTarget))
cspAlertTargets = Lens.field @"alertTargets"
{-# DEPRECATED cspAlertTargets "Use generic-lens or generic-optics with 'alertTargets' instead." #-}

-- | Specifies the behaviors that, when violated by a device (thing), cause an alert.
--
-- /Note:/ Consider using 'behaviors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspBehaviors :: Lens.Lens' CreateSecurityProfile (Core.Maybe [Types.Behavior])
cspBehaviors = Lens.field @"behaviors"
{-# DEPRECATED cspBehaviors "Use generic-lens or generic-optics with 'behaviors' instead." #-}

-- | A description of the security profile.
--
-- /Note:/ Consider using 'securityProfileDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspSecurityProfileDescription :: Lens.Lens' CreateSecurityProfile (Core.Maybe Types.SecurityProfileDescription)
cspSecurityProfileDescription = Lens.field @"securityProfileDescription"
{-# DEPRECATED cspSecurityProfileDescription "Use generic-lens or generic-optics with 'securityProfileDescription' instead." #-}

-- | Metadata that can be used to manage the security profile.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspTags :: Lens.Lens' CreateSecurityProfile (Core.Maybe [Types.Tag])
cspTags = Lens.field @"tags"
{-# DEPRECATED cspTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateSecurityProfile where
  toJSON CreateSecurityProfile {..} =
    Core.object
      ( Core.catMaybes
          [ ("additionalMetricsToRetain" Core..=)
              Core.<$> additionalMetricsToRetain,
            ("additionalMetricsToRetainV2" Core..=)
              Core.<$> additionalMetricsToRetainV2,
            ("alertTargets" Core..=) Core.<$> alertTargets,
            ("behaviors" Core..=) Core.<$> behaviors,
            ("securityProfileDescription" Core..=)
              Core.<$> securityProfileDescription,
            ("tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateSecurityProfile where
  type Rs CreateSecurityProfile = CreateSecurityProfileResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ("/security-profiles/" Core.<> (Core.toText securityProfileName)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSecurityProfileResponse'
            Core.<$> (x Core..:? "securityProfileArn")
            Core.<*> (x Core..:? "securityProfileName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateSecurityProfileResponse' smart constructor.
data CreateSecurityProfileResponse = CreateSecurityProfileResponse'
  { -- | The ARN of the security profile.
    securityProfileArn :: Core.Maybe Types.SecurityProfileArn,
    -- | The name you gave to the security profile.
    securityProfileName :: Core.Maybe Types.SecurityProfileName,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSecurityProfileResponse' value with any optional fields omitted.
mkCreateSecurityProfileResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateSecurityProfileResponse
mkCreateSecurityProfileResponse responseStatus =
  CreateSecurityProfileResponse'
    { securityProfileArn = Core.Nothing,
      securityProfileName = Core.Nothing,
      responseStatus
    }

-- | The ARN of the security profile.
--
-- /Note:/ Consider using 'securityProfileArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csprrsSecurityProfileArn :: Lens.Lens' CreateSecurityProfileResponse (Core.Maybe Types.SecurityProfileArn)
csprrsSecurityProfileArn = Lens.field @"securityProfileArn"
{-# DEPRECATED csprrsSecurityProfileArn "Use generic-lens or generic-optics with 'securityProfileArn' instead." #-}

-- | The name you gave to the security profile.
--
-- /Note:/ Consider using 'securityProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csprrsSecurityProfileName :: Lens.Lens' CreateSecurityProfileResponse (Core.Maybe Types.SecurityProfileName)
csprrsSecurityProfileName = Lens.field @"securityProfileName"
{-# DEPRECATED csprrsSecurityProfileName "Use generic-lens or generic-optics with 'securityProfileName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csprrsResponseStatus :: Lens.Lens' CreateSecurityProfileResponse Core.Int
csprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED csprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
