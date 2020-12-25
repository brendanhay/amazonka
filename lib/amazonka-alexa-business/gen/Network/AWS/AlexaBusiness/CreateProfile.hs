{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.CreateProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new room profile with the specified details.
module Network.AWS.AlexaBusiness.CreateProfile
  ( -- * Creating a request
    CreateProfile (..),
    mkCreateProfile,

    -- ** Request lenses
    cpProfileName,
    cpTimezone,
    cpAddress,
    cpDistanceUnit,
    cpTemperatureUnit,
    cpWakeWord,
    cpClientRequestToken,
    cpLocale,
    cpMaxVolumeLimit,
    cpMeetingRoomConfiguration,
    cpPSTNEnabled,
    cpSetupModeDisabled,
    cpTags,

    -- * Destructuring the response
    CreateProfileResponse (..),
    mkCreateProfileResponse,

    -- ** Response lenses
    cprrsProfileArn,
    cprrsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateProfile' smart constructor.
data CreateProfile = CreateProfile'
  { -- | The name of a room profile.
    profileName :: Types.ProfileName,
    -- | The time zone used by a room profile.
    timezone :: Types.Timezone,
    -- | The valid address for the room.
    address :: Types.Address,
    -- | The distance unit to be used by devices in the profile.
    distanceUnit :: Types.DistanceUnit,
    -- | The temperature unit to be used by devices in the profile.
    temperatureUnit :: Types.TemperatureUnit,
    -- | A wake word for Alexa, Echo, Amazon, or a computer.
    wakeWord :: Types.WakeWord,
    -- | The user-specified token that is used during the creation of a profile.
    clientRequestToken :: Core.Maybe Types.ClientRequestToken,
    -- | The locale of the room profile. (This is currently only available to a limited preview audience.)
    locale :: Core.Maybe Types.DeviceLocale,
    -- | The maximum volume limit for a room profile.
    maxVolumeLimit :: Core.Maybe Core.Int,
    -- | The meeting room settings of a room profile.
    meetingRoomConfiguration :: Core.Maybe Types.CreateMeetingRoomConfiguration,
    -- | Whether PSTN calling is enabled.
    pSTNEnabled :: Core.Maybe Core.Bool,
    -- | Whether room profile setup is enabled.
    setupModeDisabled :: Core.Maybe Core.Bool,
    -- | The tags for the profile.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateProfile' value with any optional fields omitted.
mkCreateProfile ::
  -- | 'profileName'
  Types.ProfileName ->
  -- | 'timezone'
  Types.Timezone ->
  -- | 'address'
  Types.Address ->
  -- | 'distanceUnit'
  Types.DistanceUnit ->
  -- | 'temperatureUnit'
  Types.TemperatureUnit ->
  -- | 'wakeWord'
  Types.WakeWord ->
  CreateProfile
mkCreateProfile
  profileName
  timezone
  address
  distanceUnit
  temperatureUnit
  wakeWord =
    CreateProfile'
      { profileName,
        timezone,
        address,
        distanceUnit,
        temperatureUnit,
        wakeWord,
        clientRequestToken = Core.Nothing,
        locale = Core.Nothing,
        maxVolumeLimit = Core.Nothing,
        meetingRoomConfiguration = Core.Nothing,
        pSTNEnabled = Core.Nothing,
        setupModeDisabled = Core.Nothing,
        tags = Core.Nothing
      }

-- | The name of a room profile.
--
-- /Note:/ Consider using 'profileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpProfileName :: Lens.Lens' CreateProfile Types.ProfileName
cpProfileName = Lens.field @"profileName"
{-# DEPRECATED cpProfileName "Use generic-lens or generic-optics with 'profileName' instead." #-}

-- | The time zone used by a room profile.
--
-- /Note:/ Consider using 'timezone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpTimezone :: Lens.Lens' CreateProfile Types.Timezone
cpTimezone = Lens.field @"timezone"
{-# DEPRECATED cpTimezone "Use generic-lens or generic-optics with 'timezone' instead." #-}

-- | The valid address for the room.
--
-- /Note:/ Consider using 'address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpAddress :: Lens.Lens' CreateProfile Types.Address
cpAddress = Lens.field @"address"
{-# DEPRECATED cpAddress "Use generic-lens or generic-optics with 'address' instead." #-}

-- | The distance unit to be used by devices in the profile.
--
-- /Note:/ Consider using 'distanceUnit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpDistanceUnit :: Lens.Lens' CreateProfile Types.DistanceUnit
cpDistanceUnit = Lens.field @"distanceUnit"
{-# DEPRECATED cpDistanceUnit "Use generic-lens or generic-optics with 'distanceUnit' instead." #-}

-- | The temperature unit to be used by devices in the profile.
--
-- /Note:/ Consider using 'temperatureUnit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpTemperatureUnit :: Lens.Lens' CreateProfile Types.TemperatureUnit
cpTemperatureUnit = Lens.field @"temperatureUnit"
{-# DEPRECATED cpTemperatureUnit "Use generic-lens or generic-optics with 'temperatureUnit' instead." #-}

-- | A wake word for Alexa, Echo, Amazon, or a computer.
--
-- /Note:/ Consider using 'wakeWord' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpWakeWord :: Lens.Lens' CreateProfile Types.WakeWord
cpWakeWord = Lens.field @"wakeWord"
{-# DEPRECATED cpWakeWord "Use generic-lens or generic-optics with 'wakeWord' instead." #-}

-- | The user-specified token that is used during the creation of a profile.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpClientRequestToken :: Lens.Lens' CreateProfile (Core.Maybe Types.ClientRequestToken)
cpClientRequestToken = Lens.field @"clientRequestToken"
{-# DEPRECATED cpClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | The locale of the room profile. (This is currently only available to a limited preview audience.)
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpLocale :: Lens.Lens' CreateProfile (Core.Maybe Types.DeviceLocale)
cpLocale = Lens.field @"locale"
{-# DEPRECATED cpLocale "Use generic-lens or generic-optics with 'locale' instead." #-}

-- | The maximum volume limit for a room profile.
--
-- /Note:/ Consider using 'maxVolumeLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpMaxVolumeLimit :: Lens.Lens' CreateProfile (Core.Maybe Core.Int)
cpMaxVolumeLimit = Lens.field @"maxVolumeLimit"
{-# DEPRECATED cpMaxVolumeLimit "Use generic-lens or generic-optics with 'maxVolumeLimit' instead." #-}

-- | The meeting room settings of a room profile.
--
-- /Note:/ Consider using 'meetingRoomConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpMeetingRoomConfiguration :: Lens.Lens' CreateProfile (Core.Maybe Types.CreateMeetingRoomConfiguration)
cpMeetingRoomConfiguration = Lens.field @"meetingRoomConfiguration"
{-# DEPRECATED cpMeetingRoomConfiguration "Use generic-lens or generic-optics with 'meetingRoomConfiguration' instead." #-}

-- | Whether PSTN calling is enabled.
--
-- /Note:/ Consider using 'pSTNEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpPSTNEnabled :: Lens.Lens' CreateProfile (Core.Maybe Core.Bool)
cpPSTNEnabled = Lens.field @"pSTNEnabled"
{-# DEPRECATED cpPSTNEnabled "Use generic-lens or generic-optics with 'pSTNEnabled' instead." #-}

-- | Whether room profile setup is enabled.
--
-- /Note:/ Consider using 'setupModeDisabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpSetupModeDisabled :: Lens.Lens' CreateProfile (Core.Maybe Core.Bool)
cpSetupModeDisabled = Lens.field @"setupModeDisabled"
{-# DEPRECATED cpSetupModeDisabled "Use generic-lens or generic-optics with 'setupModeDisabled' instead." #-}

-- | The tags for the profile.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpTags :: Lens.Lens' CreateProfile (Core.Maybe [Types.Tag])
cpTags = Lens.field @"tags"
{-# DEPRECATED cpTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateProfile where
  toJSON CreateProfile {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ProfileName" Core..= profileName),
            Core.Just ("Timezone" Core..= timezone),
            Core.Just ("Address" Core..= address),
            Core.Just ("DistanceUnit" Core..= distanceUnit),
            Core.Just ("TemperatureUnit" Core..= temperatureUnit),
            Core.Just ("WakeWord" Core..= wakeWord),
            ("ClientRequestToken" Core..=) Core.<$> clientRequestToken,
            ("Locale" Core..=) Core.<$> locale,
            ("MaxVolumeLimit" Core..=) Core.<$> maxVolumeLimit,
            ("MeetingRoomConfiguration" Core..=)
              Core.<$> meetingRoomConfiguration,
            ("PSTNEnabled" Core..=) Core.<$> pSTNEnabled,
            ("SetupModeDisabled" Core..=) Core.<$> setupModeDisabled,
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateProfile where
  type Rs CreateProfile = CreateProfileResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AlexaForBusiness.CreateProfile")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateProfileResponse'
            Core.<$> (x Core..:? "ProfileArn") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateProfileResponse' smart constructor.
data CreateProfileResponse = CreateProfileResponse'
  { -- | The ARN of the newly created room profile in the response.
    profileArn :: Core.Maybe Types.Arn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateProfileResponse' value with any optional fields omitted.
mkCreateProfileResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateProfileResponse
mkCreateProfileResponse responseStatus =
  CreateProfileResponse' {profileArn = Core.Nothing, responseStatus}

-- | The ARN of the newly created room profile in the response.
--
-- /Note:/ Consider using 'profileArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsProfileArn :: Lens.Lens' CreateProfileResponse (Core.Maybe Types.Arn)
cprrsProfileArn = Lens.field @"profileArn"
{-# DEPRECATED cprrsProfileArn "Use generic-lens or generic-optics with 'profileArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsResponseStatus :: Lens.Lens' CreateProfileResponse Core.Int
cprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
