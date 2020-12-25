{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.UpdateSecurityProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a Device Defender security profile.
module Network.AWS.IoT.UpdateSecurityProfile
  ( -- * Creating a request
    UpdateSecurityProfile (..),
    mkUpdateSecurityProfile,

    -- ** Request lenses
    uspSecurityProfileName,
    uspAdditionalMetricsToRetain,
    uspAdditionalMetricsToRetainV2,
    uspAlertTargets,
    uspBehaviors,
    uspDeleteAdditionalMetricsToRetain,
    uspDeleteAlertTargets,
    uspDeleteBehaviors,
    uspExpectedVersion,
    uspSecurityProfileDescription,

    -- * Destructuring the response
    UpdateSecurityProfileResponse (..),
    mkUpdateSecurityProfileResponse,

    -- ** Response lenses
    usprrsAdditionalMetricsToRetain,
    usprrsAdditionalMetricsToRetainV2,
    usprrsAlertTargets,
    usprrsBehaviors,
    usprrsCreationDate,
    usprrsLastModifiedDate,
    usprrsSecurityProfileArn,
    usprrsSecurityProfileDescription,
    usprrsSecurityProfileName,
    usprrsVersion,
    usprrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateSecurityProfile' smart constructor.
data UpdateSecurityProfile = UpdateSecurityProfile'
  { -- | The name of the security profile you want to update.
    securityProfileName :: Types.SecurityProfileName,
    -- | /Please use 'UpdateSecurityProfileRequest$additionalMetricsToRetainV2' instead./
    --
    -- A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's @behaviors@ , but it is also retained for any metric specified here.
    additionalMetricsToRetain :: Core.Maybe [Types.BehaviorMetric],
    -- | A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's behaviors, but it is also retained for any metric specified here.
    additionalMetricsToRetainV2 :: Core.Maybe [Types.MetricToRetain],
    -- | Where the alerts are sent. (Alerts are always sent to the console.)
    alertTargets :: Core.Maybe (Core.HashMap Types.AlertTargetType Types.AlertTarget),
    -- | Specifies the behaviors that, when violated by a device (thing), cause an alert.
    behaviors :: Core.Maybe [Types.Behavior],
    -- | If true, delete all @additionalMetricsToRetain@ defined for this security profile. If any @additionalMetricsToRetain@ are defined in the current invocation, an exception occurs.
    deleteAdditionalMetricsToRetain :: Core.Maybe Core.Bool,
    -- | If true, delete all @alertTargets@ defined for this security profile. If any @alertTargets@ are defined in the current invocation, an exception occurs.
    deleteAlertTargets :: Core.Maybe Core.Bool,
    -- | If true, delete all @behaviors@ defined for this security profile. If any @behaviors@ are defined in the current invocation, an exception occurs.
    deleteBehaviors :: Core.Maybe Core.Bool,
    -- | The expected version of the security profile. A new version is generated whenever the security profile is updated. If you specify a value that is different from the actual version, a @VersionConflictException@ is thrown.
    expectedVersion :: Core.Maybe Core.Integer,
    -- | A description of the security profile.
    securityProfileDescription :: Core.Maybe Types.SecurityProfileDescription
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSecurityProfile' value with any optional fields omitted.
mkUpdateSecurityProfile ::
  -- | 'securityProfileName'
  Types.SecurityProfileName ->
  UpdateSecurityProfile
mkUpdateSecurityProfile securityProfileName =
  UpdateSecurityProfile'
    { securityProfileName,
      additionalMetricsToRetain = Core.Nothing,
      additionalMetricsToRetainV2 = Core.Nothing,
      alertTargets = Core.Nothing,
      behaviors = Core.Nothing,
      deleteAdditionalMetricsToRetain = Core.Nothing,
      deleteAlertTargets = Core.Nothing,
      deleteBehaviors = Core.Nothing,
      expectedVersion = Core.Nothing,
      securityProfileDescription = Core.Nothing
    }

-- | The name of the security profile you want to update.
--
-- /Note:/ Consider using 'securityProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uspSecurityProfileName :: Lens.Lens' UpdateSecurityProfile Types.SecurityProfileName
uspSecurityProfileName = Lens.field @"securityProfileName"
{-# DEPRECATED uspSecurityProfileName "Use generic-lens or generic-optics with 'securityProfileName' instead." #-}

-- | /Please use 'UpdateSecurityProfileRequest$additionalMetricsToRetainV2' instead./
--
-- A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's @behaviors@ , but it is also retained for any metric specified here.
--
-- /Note:/ Consider using 'additionalMetricsToRetain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uspAdditionalMetricsToRetain :: Lens.Lens' UpdateSecurityProfile (Core.Maybe [Types.BehaviorMetric])
uspAdditionalMetricsToRetain = Lens.field @"additionalMetricsToRetain"
{-# DEPRECATED uspAdditionalMetricsToRetain "Use generic-lens or generic-optics with 'additionalMetricsToRetain' instead." #-}

-- | A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's behaviors, but it is also retained for any metric specified here.
--
-- /Note:/ Consider using 'additionalMetricsToRetainV2' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uspAdditionalMetricsToRetainV2 :: Lens.Lens' UpdateSecurityProfile (Core.Maybe [Types.MetricToRetain])
uspAdditionalMetricsToRetainV2 = Lens.field @"additionalMetricsToRetainV2"
{-# DEPRECATED uspAdditionalMetricsToRetainV2 "Use generic-lens or generic-optics with 'additionalMetricsToRetainV2' instead." #-}

-- | Where the alerts are sent. (Alerts are always sent to the console.)
--
-- /Note:/ Consider using 'alertTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uspAlertTargets :: Lens.Lens' UpdateSecurityProfile (Core.Maybe (Core.HashMap Types.AlertTargetType Types.AlertTarget))
uspAlertTargets = Lens.field @"alertTargets"
{-# DEPRECATED uspAlertTargets "Use generic-lens or generic-optics with 'alertTargets' instead." #-}

-- | Specifies the behaviors that, when violated by a device (thing), cause an alert.
--
-- /Note:/ Consider using 'behaviors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uspBehaviors :: Lens.Lens' UpdateSecurityProfile (Core.Maybe [Types.Behavior])
uspBehaviors = Lens.field @"behaviors"
{-# DEPRECATED uspBehaviors "Use generic-lens or generic-optics with 'behaviors' instead." #-}

-- | If true, delete all @additionalMetricsToRetain@ defined for this security profile. If any @additionalMetricsToRetain@ are defined in the current invocation, an exception occurs.
--
-- /Note:/ Consider using 'deleteAdditionalMetricsToRetain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uspDeleteAdditionalMetricsToRetain :: Lens.Lens' UpdateSecurityProfile (Core.Maybe Core.Bool)
uspDeleteAdditionalMetricsToRetain = Lens.field @"deleteAdditionalMetricsToRetain"
{-# DEPRECATED uspDeleteAdditionalMetricsToRetain "Use generic-lens or generic-optics with 'deleteAdditionalMetricsToRetain' instead." #-}

-- | If true, delete all @alertTargets@ defined for this security profile. If any @alertTargets@ are defined in the current invocation, an exception occurs.
--
-- /Note:/ Consider using 'deleteAlertTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uspDeleteAlertTargets :: Lens.Lens' UpdateSecurityProfile (Core.Maybe Core.Bool)
uspDeleteAlertTargets = Lens.field @"deleteAlertTargets"
{-# DEPRECATED uspDeleteAlertTargets "Use generic-lens or generic-optics with 'deleteAlertTargets' instead." #-}

-- | If true, delete all @behaviors@ defined for this security profile. If any @behaviors@ are defined in the current invocation, an exception occurs.
--
-- /Note:/ Consider using 'deleteBehaviors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uspDeleteBehaviors :: Lens.Lens' UpdateSecurityProfile (Core.Maybe Core.Bool)
uspDeleteBehaviors = Lens.field @"deleteBehaviors"
{-# DEPRECATED uspDeleteBehaviors "Use generic-lens or generic-optics with 'deleteBehaviors' instead." #-}

-- | The expected version of the security profile. A new version is generated whenever the security profile is updated. If you specify a value that is different from the actual version, a @VersionConflictException@ is thrown.
--
-- /Note:/ Consider using 'expectedVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uspExpectedVersion :: Lens.Lens' UpdateSecurityProfile (Core.Maybe Core.Integer)
uspExpectedVersion = Lens.field @"expectedVersion"
{-# DEPRECATED uspExpectedVersion "Use generic-lens or generic-optics with 'expectedVersion' instead." #-}

-- | A description of the security profile.
--
-- /Note:/ Consider using 'securityProfileDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uspSecurityProfileDescription :: Lens.Lens' UpdateSecurityProfile (Core.Maybe Types.SecurityProfileDescription)
uspSecurityProfileDescription = Lens.field @"securityProfileDescription"
{-# DEPRECATED uspSecurityProfileDescription "Use generic-lens or generic-optics with 'securityProfileDescription' instead." #-}

instance Core.FromJSON UpdateSecurityProfile where
  toJSON UpdateSecurityProfile {..} =
    Core.object
      ( Core.catMaybes
          [ ("additionalMetricsToRetain" Core..=)
              Core.<$> additionalMetricsToRetain,
            ("additionalMetricsToRetainV2" Core..=)
              Core.<$> additionalMetricsToRetainV2,
            ("alertTargets" Core..=) Core.<$> alertTargets,
            ("behaviors" Core..=) Core.<$> behaviors,
            ("deleteAdditionalMetricsToRetain" Core..=)
              Core.<$> deleteAdditionalMetricsToRetain,
            ("deleteAlertTargets" Core..=) Core.<$> deleteAlertTargets,
            ("deleteBehaviors" Core..=) Core.<$> deleteBehaviors,
            ("securityProfileDescription" Core..=)
              Core.<$> securityProfileDescription
          ]
      )

instance Core.AWSRequest UpdateSecurityProfile where
  type Rs UpdateSecurityProfile = UpdateSecurityProfileResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PATCH,
        Core._rqPath =
          Core.rawPath
            ("/security-profiles/" Core.<> (Core.toText securityProfileName)),
        Core._rqQuery =
          Core.toQueryValue "expectedVersion" Core.<$> expectedVersion,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSecurityProfileResponse'
            Core.<$> (x Core..:? "additionalMetricsToRetain")
            Core.<*> (x Core..:? "additionalMetricsToRetainV2")
            Core.<*> (x Core..:? "alertTargets")
            Core.<*> (x Core..:? "behaviors")
            Core.<*> (x Core..:? "creationDate")
            Core.<*> (x Core..:? "lastModifiedDate")
            Core.<*> (x Core..:? "securityProfileArn")
            Core.<*> (x Core..:? "securityProfileDescription")
            Core.<*> (x Core..:? "securityProfileName")
            Core.<*> (x Core..:? "version")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateSecurityProfileResponse' smart constructor.
data UpdateSecurityProfileResponse = UpdateSecurityProfileResponse'
  { -- | /Please use 'UpdateSecurityProfileResponse$additionalMetricsToRetainV2' instead./
    --
    -- A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the security profile's @behaviors@ , but it is also retained for any metric specified here.
    additionalMetricsToRetain :: Core.Maybe [Types.BehaviorMetric],
    -- | A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's behaviors, but it is also retained for any metric specified here.
    additionalMetricsToRetainV2 :: Core.Maybe [Types.MetricToRetain],
    -- | Where the alerts are sent. (Alerts are always sent to the console.)
    alertTargets :: Core.Maybe (Core.HashMap Types.AlertTargetType Types.AlertTarget),
    -- | Specifies the behaviors that, when violated by a device (thing), cause an alert.
    behaviors :: Core.Maybe [Types.Behavior],
    -- | The time the security profile was created.
    creationDate :: Core.Maybe Core.NominalDiffTime,
    -- | The time the security profile was last modified.
    lastModifiedDate :: Core.Maybe Core.NominalDiffTime,
    -- | The ARN of the security profile that was updated.
    securityProfileArn :: Core.Maybe Types.SecurityProfileArn,
    -- | The description of the security profile.
    securityProfileDescription :: Core.Maybe Types.SecurityProfileDescription,
    -- | The name of the security profile that was updated.
    securityProfileName :: Core.Maybe Types.SecurityProfileName,
    -- | The updated version of the security profile.
    version :: Core.Maybe Core.Integer,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateSecurityProfileResponse' value with any optional fields omitted.
mkUpdateSecurityProfileResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateSecurityProfileResponse
mkUpdateSecurityProfileResponse responseStatus =
  UpdateSecurityProfileResponse'
    { additionalMetricsToRetain =
        Core.Nothing,
      additionalMetricsToRetainV2 = Core.Nothing,
      alertTargets = Core.Nothing,
      behaviors = Core.Nothing,
      creationDate = Core.Nothing,
      lastModifiedDate = Core.Nothing,
      securityProfileArn = Core.Nothing,
      securityProfileDescription = Core.Nothing,
      securityProfileName = Core.Nothing,
      version = Core.Nothing,
      responseStatus
    }

-- | /Please use 'UpdateSecurityProfileResponse$additionalMetricsToRetainV2' instead./
--
-- A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the security profile's @behaviors@ , but it is also retained for any metric specified here.
--
-- /Note:/ Consider using 'additionalMetricsToRetain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usprrsAdditionalMetricsToRetain :: Lens.Lens' UpdateSecurityProfileResponse (Core.Maybe [Types.BehaviorMetric])
usprrsAdditionalMetricsToRetain = Lens.field @"additionalMetricsToRetain"
{-# DEPRECATED usprrsAdditionalMetricsToRetain "Use generic-lens or generic-optics with 'additionalMetricsToRetain' instead." #-}

-- | A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's behaviors, but it is also retained for any metric specified here.
--
-- /Note:/ Consider using 'additionalMetricsToRetainV2' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usprrsAdditionalMetricsToRetainV2 :: Lens.Lens' UpdateSecurityProfileResponse (Core.Maybe [Types.MetricToRetain])
usprrsAdditionalMetricsToRetainV2 = Lens.field @"additionalMetricsToRetainV2"
{-# DEPRECATED usprrsAdditionalMetricsToRetainV2 "Use generic-lens or generic-optics with 'additionalMetricsToRetainV2' instead." #-}

-- | Where the alerts are sent. (Alerts are always sent to the console.)
--
-- /Note:/ Consider using 'alertTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usprrsAlertTargets :: Lens.Lens' UpdateSecurityProfileResponse (Core.Maybe (Core.HashMap Types.AlertTargetType Types.AlertTarget))
usprrsAlertTargets = Lens.field @"alertTargets"
{-# DEPRECATED usprrsAlertTargets "Use generic-lens or generic-optics with 'alertTargets' instead." #-}

-- | Specifies the behaviors that, when violated by a device (thing), cause an alert.
--
-- /Note:/ Consider using 'behaviors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usprrsBehaviors :: Lens.Lens' UpdateSecurityProfileResponse (Core.Maybe [Types.Behavior])
usprrsBehaviors = Lens.field @"behaviors"
{-# DEPRECATED usprrsBehaviors "Use generic-lens or generic-optics with 'behaviors' instead." #-}

-- | The time the security profile was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usprrsCreationDate :: Lens.Lens' UpdateSecurityProfileResponse (Core.Maybe Core.NominalDiffTime)
usprrsCreationDate = Lens.field @"creationDate"
{-# DEPRECATED usprrsCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The time the security profile was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usprrsLastModifiedDate :: Lens.Lens' UpdateSecurityProfileResponse (Core.Maybe Core.NominalDiffTime)
usprrsLastModifiedDate = Lens.field @"lastModifiedDate"
{-# DEPRECATED usprrsLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The ARN of the security profile that was updated.
--
-- /Note:/ Consider using 'securityProfileArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usprrsSecurityProfileArn :: Lens.Lens' UpdateSecurityProfileResponse (Core.Maybe Types.SecurityProfileArn)
usprrsSecurityProfileArn = Lens.field @"securityProfileArn"
{-# DEPRECATED usprrsSecurityProfileArn "Use generic-lens or generic-optics with 'securityProfileArn' instead." #-}

-- | The description of the security profile.
--
-- /Note:/ Consider using 'securityProfileDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usprrsSecurityProfileDescription :: Lens.Lens' UpdateSecurityProfileResponse (Core.Maybe Types.SecurityProfileDescription)
usprrsSecurityProfileDescription = Lens.field @"securityProfileDescription"
{-# DEPRECATED usprrsSecurityProfileDescription "Use generic-lens or generic-optics with 'securityProfileDescription' instead." #-}

-- | The name of the security profile that was updated.
--
-- /Note:/ Consider using 'securityProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usprrsSecurityProfileName :: Lens.Lens' UpdateSecurityProfileResponse (Core.Maybe Types.SecurityProfileName)
usprrsSecurityProfileName = Lens.field @"securityProfileName"
{-# DEPRECATED usprrsSecurityProfileName "Use generic-lens or generic-optics with 'securityProfileName' instead." #-}

-- | The updated version of the security profile.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usprrsVersion :: Lens.Lens' UpdateSecurityProfileResponse (Core.Maybe Core.Integer)
usprrsVersion = Lens.field @"version"
{-# DEPRECATED usprrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usprrsResponseStatus :: Lens.Lens' UpdateSecurityProfileResponse Core.Int
usprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED usprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
