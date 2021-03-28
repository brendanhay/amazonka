{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      UpdateProfile (..)
    , mkUpdateProfile
    -- ** Request lenses
    , upAddress
    , upDistanceUnit
    , upIsDefault
    , upLocale
    , upMaxVolumeLimit
    , upMeetingRoomConfiguration
    , upPSTNEnabled
    , upProfileArn
    , upProfileName
    , upSetupModeDisabled
    , upTemperatureUnit
    , upTimezone
    , upWakeWord

    -- * Destructuring the response
    , UpdateProfileResponse (..)
    , mkUpdateProfileResponse
    -- ** Response lenses
    , uprrsResponseStatus
    ) where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateProfile' smart constructor.
data UpdateProfile = UpdateProfile'
  { address :: Core.Maybe Types.Address
    -- ^ The updated address for the room profile.
  , distanceUnit :: Core.Maybe Types.DistanceUnit
    -- ^ The updated distance unit for the room profile.
  , isDefault :: Core.Maybe Core.Bool
    -- ^ Sets the profile as default if selected. If this is missing, no update is done to the default status.
  , locale :: Core.Maybe Types.DeviceLocale
    -- ^ The updated locale for the room profile. (This is currently only available to a limited preview audience.)
  , maxVolumeLimit :: Core.Maybe Core.Int
    -- ^ The updated maximum volume limit for the room profile.
  , meetingRoomConfiguration :: Core.Maybe Types.UpdateMeetingRoomConfiguration
    -- ^ The updated meeting room settings of a room profile.
  , pSTNEnabled :: Core.Maybe Core.Bool
    -- ^ Whether the PSTN setting of the room profile is enabled.
  , profileArn :: Core.Maybe Types.Arn
    -- ^ The ARN of the room profile to update. Required.
  , profileName :: Core.Maybe Types.ProfileName
    -- ^ The updated name for the room profile.
  , setupModeDisabled :: Core.Maybe Core.Bool
    -- ^ Whether the setup mode of the profile is enabled.
  , temperatureUnit :: Core.Maybe Types.TemperatureUnit
    -- ^ The updated temperature unit for the room profile.
  , timezone :: Core.Maybe Types.Timezone
    -- ^ The updated timezone for the room profile.
  , wakeWord :: Core.Maybe Types.WakeWord
    -- ^ The updated wake word for the room profile.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateProfile' value with any optional fields omitted.
mkUpdateProfile
    :: UpdateProfile
mkUpdateProfile
  = UpdateProfile'{address = Core.Nothing,
                   distanceUnit = Core.Nothing, isDefault = Core.Nothing,
                   locale = Core.Nothing, maxVolumeLimit = Core.Nothing,
                   meetingRoomConfiguration = Core.Nothing,
                   pSTNEnabled = Core.Nothing, profileArn = Core.Nothing,
                   profileName = Core.Nothing, setupModeDisabled = Core.Nothing,
                   temperatureUnit = Core.Nothing, timezone = Core.Nothing,
                   wakeWord = Core.Nothing}

-- | The updated address for the room profile.
--
-- /Note:/ Consider using 'address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upAddress :: Lens.Lens' UpdateProfile (Core.Maybe Types.Address)
upAddress = Lens.field @"address"
{-# INLINEABLE upAddress #-}
{-# DEPRECATED address "Use generic-lens or generic-optics with 'address' instead"  #-}

-- | The updated distance unit for the room profile.
--
-- /Note:/ Consider using 'distanceUnit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upDistanceUnit :: Lens.Lens' UpdateProfile (Core.Maybe Types.DistanceUnit)
upDistanceUnit = Lens.field @"distanceUnit"
{-# INLINEABLE upDistanceUnit #-}
{-# DEPRECATED distanceUnit "Use generic-lens or generic-optics with 'distanceUnit' instead"  #-}

-- | Sets the profile as default if selected. If this is missing, no update is done to the default status.
--
-- /Note:/ Consider using 'isDefault' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upIsDefault :: Lens.Lens' UpdateProfile (Core.Maybe Core.Bool)
upIsDefault = Lens.field @"isDefault"
{-# INLINEABLE upIsDefault #-}
{-# DEPRECATED isDefault "Use generic-lens or generic-optics with 'isDefault' instead"  #-}

-- | The updated locale for the room profile. (This is currently only available to a limited preview audience.)
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upLocale :: Lens.Lens' UpdateProfile (Core.Maybe Types.DeviceLocale)
upLocale = Lens.field @"locale"
{-# INLINEABLE upLocale #-}
{-# DEPRECATED locale "Use generic-lens or generic-optics with 'locale' instead"  #-}

-- | The updated maximum volume limit for the room profile.
--
-- /Note:/ Consider using 'maxVolumeLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upMaxVolumeLimit :: Lens.Lens' UpdateProfile (Core.Maybe Core.Int)
upMaxVolumeLimit = Lens.field @"maxVolumeLimit"
{-# INLINEABLE upMaxVolumeLimit #-}
{-# DEPRECATED maxVolumeLimit "Use generic-lens or generic-optics with 'maxVolumeLimit' instead"  #-}

-- | The updated meeting room settings of a room profile.
--
-- /Note:/ Consider using 'meetingRoomConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upMeetingRoomConfiguration :: Lens.Lens' UpdateProfile (Core.Maybe Types.UpdateMeetingRoomConfiguration)
upMeetingRoomConfiguration = Lens.field @"meetingRoomConfiguration"
{-# INLINEABLE upMeetingRoomConfiguration #-}
{-# DEPRECATED meetingRoomConfiguration "Use generic-lens or generic-optics with 'meetingRoomConfiguration' instead"  #-}

-- | Whether the PSTN setting of the room profile is enabled.
--
-- /Note:/ Consider using 'pSTNEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upPSTNEnabled :: Lens.Lens' UpdateProfile (Core.Maybe Core.Bool)
upPSTNEnabled = Lens.field @"pSTNEnabled"
{-# INLINEABLE upPSTNEnabled #-}
{-# DEPRECATED pSTNEnabled "Use generic-lens or generic-optics with 'pSTNEnabled' instead"  #-}

-- | The ARN of the room profile to update. Required.
--
-- /Note:/ Consider using 'profileArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upProfileArn :: Lens.Lens' UpdateProfile (Core.Maybe Types.Arn)
upProfileArn = Lens.field @"profileArn"
{-# INLINEABLE upProfileArn #-}
{-# DEPRECATED profileArn "Use generic-lens or generic-optics with 'profileArn' instead"  #-}

-- | The updated name for the room profile.
--
-- /Note:/ Consider using 'profileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upProfileName :: Lens.Lens' UpdateProfile (Core.Maybe Types.ProfileName)
upProfileName = Lens.field @"profileName"
{-# INLINEABLE upProfileName #-}
{-# DEPRECATED profileName "Use generic-lens or generic-optics with 'profileName' instead"  #-}

-- | Whether the setup mode of the profile is enabled.
--
-- /Note:/ Consider using 'setupModeDisabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upSetupModeDisabled :: Lens.Lens' UpdateProfile (Core.Maybe Core.Bool)
upSetupModeDisabled = Lens.field @"setupModeDisabled"
{-# INLINEABLE upSetupModeDisabled #-}
{-# DEPRECATED setupModeDisabled "Use generic-lens or generic-optics with 'setupModeDisabled' instead"  #-}

-- | The updated temperature unit for the room profile.
--
-- /Note:/ Consider using 'temperatureUnit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upTemperatureUnit :: Lens.Lens' UpdateProfile (Core.Maybe Types.TemperatureUnit)
upTemperatureUnit = Lens.field @"temperatureUnit"
{-# INLINEABLE upTemperatureUnit #-}
{-# DEPRECATED temperatureUnit "Use generic-lens or generic-optics with 'temperatureUnit' instead"  #-}

-- | The updated timezone for the room profile.
--
-- /Note:/ Consider using 'timezone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upTimezone :: Lens.Lens' UpdateProfile (Core.Maybe Types.Timezone)
upTimezone = Lens.field @"timezone"
{-# INLINEABLE upTimezone #-}
{-# DEPRECATED timezone "Use generic-lens or generic-optics with 'timezone' instead"  #-}

-- | The updated wake word for the room profile.
--
-- /Note:/ Consider using 'wakeWord' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upWakeWord :: Lens.Lens' UpdateProfile (Core.Maybe Types.WakeWord)
upWakeWord = Lens.field @"wakeWord"
{-# INLINEABLE upWakeWord #-}
{-# DEPRECATED wakeWord "Use generic-lens or generic-optics with 'wakeWord' instead"  #-}

instance Core.ToQuery UpdateProfile where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateProfile where
        toHeaders UpdateProfile{..}
          = Core.pure ("X-Amz-Target", "AlexaForBusiness.UpdateProfile")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateProfile where
        toJSON UpdateProfile{..}
          = Core.object
              (Core.catMaybes
                 [("Address" Core..=) Core.<$> address,
                  ("DistanceUnit" Core..=) Core.<$> distanceUnit,
                  ("IsDefault" Core..=) Core.<$> isDefault,
                  ("Locale" Core..=) Core.<$> locale,
                  ("MaxVolumeLimit" Core..=) Core.<$> maxVolumeLimit,
                  ("MeetingRoomConfiguration" Core..=) Core.<$>
                    meetingRoomConfiguration,
                  ("PSTNEnabled" Core..=) Core.<$> pSTNEnabled,
                  ("ProfileArn" Core..=) Core.<$> profileArn,
                  ("ProfileName" Core..=) Core.<$> profileName,
                  ("SetupModeDisabled" Core..=) Core.<$> setupModeDisabled,
                  ("TemperatureUnit" Core..=) Core.<$> temperatureUnit,
                  ("Timezone" Core..=) Core.<$> timezone,
                  ("WakeWord" Core..=) Core.<$> wakeWord])

instance Core.AWSRequest UpdateProfile where
        type Rs UpdateProfile = UpdateProfileResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateProfileResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateProfileResponse' smart constructor.
newtype UpdateProfileResponse = UpdateProfileResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateProfileResponse' value with any optional fields omitted.
mkUpdateProfileResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateProfileResponse
mkUpdateProfileResponse responseStatus
  = UpdateProfileResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprrsResponseStatus :: Lens.Lens' UpdateProfileResponse Core.Int
uprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
