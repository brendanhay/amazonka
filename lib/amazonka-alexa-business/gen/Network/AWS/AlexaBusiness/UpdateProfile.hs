{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.UpdateProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing room profile by room profile ARN.
module Network.AWS.AlexaBusiness.UpdateProfile
  ( -- * Creating a request
    UpdateProfile (..),
    mkUpdateProfile,

    -- ** Request lenses
    upAddress,
    upDistanceUnit,
    upIsDefault,
    upLocale,
    upMaxVolumeLimit,
    upMeetingRoomConfiguration,
    upPSTNEnabled,
    upProfileArn,
    upProfileName,
    upSetupModeDisabled,
    upTemperatureUnit,
    upTimezone,
    upWakeWord,

    -- * Destructuring the response
    UpdateProfileResponse (..),
    mkUpdateProfileResponse,

    -- ** Response lenses
    uprrsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateProfile' smart constructor.
data UpdateProfile = UpdateProfile'
  { -- | The updated address for the room profile.
    address :: Core.Maybe Types.Address,
    -- | The updated distance unit for the room profile.
    distanceUnit :: Core.Maybe Types.DistanceUnit,
    -- | Sets the profile as default if selected. If this is missing, no update is done to the default status.
    isDefault :: Core.Maybe Core.Bool,
    -- | The updated locale for the room profile. (This is currently only available to a limited preview audience.)
    locale :: Core.Maybe Types.DeviceLocale,
    -- | The updated maximum volume limit for the room profile.
    maxVolumeLimit :: Core.Maybe Core.Int,
    -- | The updated meeting room settings of a room profile.
    meetingRoomConfiguration :: Core.Maybe Types.UpdateMeetingRoomConfiguration,
    -- | Whether the PSTN setting of the room profile is enabled.
    pSTNEnabled :: Core.Maybe Core.Bool,
    -- | The ARN of the room profile to update. Required.
    profileArn :: Core.Maybe Types.Arn,
    -- | The updated name for the room profile.
    profileName :: Core.Maybe Types.ProfileName,
    -- | Whether the setup mode of the profile is enabled.
    setupModeDisabled :: Core.Maybe Core.Bool,
    -- | The updated temperature unit for the room profile.
    temperatureUnit :: Core.Maybe Types.TemperatureUnit,
    -- | The updated timezone for the room profile.
    timezone :: Core.Maybe Types.Timezone,
    -- | The updated wake word for the room profile.
    wakeWord :: Core.Maybe Types.WakeWord
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateProfile' value with any optional fields omitted.
mkUpdateProfile ::
  UpdateProfile
mkUpdateProfile =
  UpdateProfile'
    { address = Core.Nothing,
      distanceUnit = Core.Nothing,
      isDefault = Core.Nothing,
      locale = Core.Nothing,
      maxVolumeLimit = Core.Nothing,
      meetingRoomConfiguration = Core.Nothing,
      pSTNEnabled = Core.Nothing,
      profileArn = Core.Nothing,
      profileName = Core.Nothing,
      setupModeDisabled = Core.Nothing,
      temperatureUnit = Core.Nothing,
      timezone = Core.Nothing,
      wakeWord = Core.Nothing
    }

-- | The updated address for the room profile.
--
-- /Note:/ Consider using 'address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upAddress :: Lens.Lens' UpdateProfile (Core.Maybe Types.Address)
upAddress = Lens.field @"address"
{-# DEPRECATED upAddress "Use generic-lens or generic-optics with 'address' instead." #-}

-- | The updated distance unit for the room profile.
--
-- /Note:/ Consider using 'distanceUnit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upDistanceUnit :: Lens.Lens' UpdateProfile (Core.Maybe Types.DistanceUnit)
upDistanceUnit = Lens.field @"distanceUnit"
{-# DEPRECATED upDistanceUnit "Use generic-lens or generic-optics with 'distanceUnit' instead." #-}

-- | Sets the profile as default if selected. If this is missing, no update is done to the default status.
--
-- /Note:/ Consider using 'isDefault' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upIsDefault :: Lens.Lens' UpdateProfile (Core.Maybe Core.Bool)
upIsDefault = Lens.field @"isDefault"
{-# DEPRECATED upIsDefault "Use generic-lens or generic-optics with 'isDefault' instead." #-}

-- | The updated locale for the room profile. (This is currently only available to a limited preview audience.)
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upLocale :: Lens.Lens' UpdateProfile (Core.Maybe Types.DeviceLocale)
upLocale = Lens.field @"locale"
{-# DEPRECATED upLocale "Use generic-lens or generic-optics with 'locale' instead." #-}

-- | The updated maximum volume limit for the room profile.
--
-- /Note:/ Consider using 'maxVolumeLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upMaxVolumeLimit :: Lens.Lens' UpdateProfile (Core.Maybe Core.Int)
upMaxVolumeLimit = Lens.field @"maxVolumeLimit"
{-# DEPRECATED upMaxVolumeLimit "Use generic-lens or generic-optics with 'maxVolumeLimit' instead." #-}

-- | The updated meeting room settings of a room profile.
--
-- /Note:/ Consider using 'meetingRoomConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upMeetingRoomConfiguration :: Lens.Lens' UpdateProfile (Core.Maybe Types.UpdateMeetingRoomConfiguration)
upMeetingRoomConfiguration = Lens.field @"meetingRoomConfiguration"
{-# DEPRECATED upMeetingRoomConfiguration "Use generic-lens or generic-optics with 'meetingRoomConfiguration' instead." #-}

-- | Whether the PSTN setting of the room profile is enabled.
--
-- /Note:/ Consider using 'pSTNEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upPSTNEnabled :: Lens.Lens' UpdateProfile (Core.Maybe Core.Bool)
upPSTNEnabled = Lens.field @"pSTNEnabled"
{-# DEPRECATED upPSTNEnabled "Use generic-lens or generic-optics with 'pSTNEnabled' instead." #-}

-- | The ARN of the room profile to update. Required.
--
-- /Note:/ Consider using 'profileArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upProfileArn :: Lens.Lens' UpdateProfile (Core.Maybe Types.Arn)
upProfileArn = Lens.field @"profileArn"
{-# DEPRECATED upProfileArn "Use generic-lens or generic-optics with 'profileArn' instead." #-}

-- | The updated name for the room profile.
--
-- /Note:/ Consider using 'profileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upProfileName :: Lens.Lens' UpdateProfile (Core.Maybe Types.ProfileName)
upProfileName = Lens.field @"profileName"
{-# DEPRECATED upProfileName "Use generic-lens or generic-optics with 'profileName' instead." #-}

-- | Whether the setup mode of the profile is enabled.
--
-- /Note:/ Consider using 'setupModeDisabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upSetupModeDisabled :: Lens.Lens' UpdateProfile (Core.Maybe Core.Bool)
upSetupModeDisabled = Lens.field @"setupModeDisabled"
{-# DEPRECATED upSetupModeDisabled "Use generic-lens or generic-optics with 'setupModeDisabled' instead." #-}

-- | The updated temperature unit for the room profile.
--
-- /Note:/ Consider using 'temperatureUnit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upTemperatureUnit :: Lens.Lens' UpdateProfile (Core.Maybe Types.TemperatureUnit)
upTemperatureUnit = Lens.field @"temperatureUnit"
{-# DEPRECATED upTemperatureUnit "Use generic-lens or generic-optics with 'temperatureUnit' instead." #-}

-- | The updated timezone for the room profile.
--
-- /Note:/ Consider using 'timezone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upTimezone :: Lens.Lens' UpdateProfile (Core.Maybe Types.Timezone)
upTimezone = Lens.field @"timezone"
{-# DEPRECATED upTimezone "Use generic-lens or generic-optics with 'timezone' instead." #-}

-- | The updated wake word for the room profile.
--
-- /Note:/ Consider using 'wakeWord' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upWakeWord :: Lens.Lens' UpdateProfile (Core.Maybe Types.WakeWord)
upWakeWord = Lens.field @"wakeWord"
{-# DEPRECATED upWakeWord "Use generic-lens or generic-optics with 'wakeWord' instead." #-}

instance Core.FromJSON UpdateProfile where
  toJSON UpdateProfile {..} =
    Core.object
      ( Core.catMaybes
          [ ("Address" Core..=) Core.<$> address,
            ("DistanceUnit" Core..=) Core.<$> distanceUnit,
            ("IsDefault" Core..=) Core.<$> isDefault,
            ("Locale" Core..=) Core.<$> locale,
            ("MaxVolumeLimit" Core..=) Core.<$> maxVolumeLimit,
            ("MeetingRoomConfiguration" Core..=)
              Core.<$> meetingRoomConfiguration,
            ("PSTNEnabled" Core..=) Core.<$> pSTNEnabled,
            ("ProfileArn" Core..=) Core.<$> profileArn,
            ("ProfileName" Core..=) Core.<$> profileName,
            ("SetupModeDisabled" Core..=) Core.<$> setupModeDisabled,
            ("TemperatureUnit" Core..=) Core.<$> temperatureUnit,
            ("Timezone" Core..=) Core.<$> timezone,
            ("WakeWord" Core..=) Core.<$> wakeWord
          ]
      )

instance Core.AWSRequest UpdateProfile where
  type Rs UpdateProfile = UpdateProfileResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AlexaForBusiness.UpdateProfile")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateProfileResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateProfileResponse' smart constructor.
newtype UpdateProfileResponse = UpdateProfileResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateProfileResponse' value with any optional fields omitted.
mkUpdateProfileResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateProfileResponse
mkUpdateProfileResponse responseStatus =
  UpdateProfileResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprrsResponseStatus :: Lens.Lens' UpdateProfileResponse Core.Int
uprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
