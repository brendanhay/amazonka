{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    upSetupModeDisabled,
    upPSTNEnabled,
    upDistanceUnit,
    upLocale,
    upAddress,
    upProfileARN,
    upWakeWord,
    upMeetingRoomConfiguration,
    upProfileName,
    upTemperatureUnit,
    upTimezone,
    upMaxVolumeLimit,
    upIsDefault,

    -- * Destructuring the response
    UpdateProfileResponse (..),
    mkUpdateProfileResponse,

    -- ** Response lenses
    uprsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateProfile' smart constructor.
data UpdateProfile = UpdateProfile'
  { setupModeDisabled ::
      Lude.Maybe Lude.Bool,
    pSTNEnabled :: Lude.Maybe Lude.Bool,
    distanceUnit :: Lude.Maybe DistanceUnit,
    locale :: Lude.Maybe Lude.Text,
    address :: Lude.Maybe Lude.Text,
    profileARN :: Lude.Maybe Lude.Text,
    wakeWord :: Lude.Maybe WakeWord,
    meetingRoomConfiguration ::
      Lude.Maybe UpdateMeetingRoomConfiguration,
    profileName :: Lude.Maybe Lude.Text,
    temperatureUnit :: Lude.Maybe TemperatureUnit,
    timezone :: Lude.Maybe Lude.Text,
    maxVolumeLimit :: Lude.Maybe Lude.Int,
    isDefault :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateProfile' with the minimum fields required to make a request.
--
-- * 'address' - The updated address for the room profile.
-- * 'distanceUnit' - The updated distance unit for the room profile.
-- * 'isDefault' - Sets the profile as default if selected. If this is missing, no update is done to the default status.
-- * 'locale' - The updated locale for the room profile. (This is currently only available to a limited preview audience.)
-- * 'maxVolumeLimit' - The updated maximum volume limit for the room profile.
-- * 'meetingRoomConfiguration' - The updated meeting room settings of a room profile.
-- * 'pSTNEnabled' - Whether the PSTN setting of the room profile is enabled.
-- * 'profileARN' - The ARN of the room profile to update. Required.
-- * 'profileName' - The updated name for the room profile.
-- * 'setupModeDisabled' - Whether the setup mode of the profile is enabled.
-- * 'temperatureUnit' - The updated temperature unit for the room profile.
-- * 'timezone' - The updated timezone for the room profile.
-- * 'wakeWord' - The updated wake word for the room profile.
mkUpdateProfile ::
  UpdateProfile
mkUpdateProfile =
  UpdateProfile'
    { setupModeDisabled = Lude.Nothing,
      pSTNEnabled = Lude.Nothing,
      distanceUnit = Lude.Nothing,
      locale = Lude.Nothing,
      address = Lude.Nothing,
      profileARN = Lude.Nothing,
      wakeWord = Lude.Nothing,
      meetingRoomConfiguration = Lude.Nothing,
      profileName = Lude.Nothing,
      temperatureUnit = Lude.Nothing,
      timezone = Lude.Nothing,
      maxVolumeLimit = Lude.Nothing,
      isDefault = Lude.Nothing
    }

-- | Whether the setup mode of the profile is enabled.
--
-- /Note:/ Consider using 'setupModeDisabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upSetupModeDisabled :: Lens.Lens' UpdateProfile (Lude.Maybe Lude.Bool)
upSetupModeDisabled = Lens.lens (setupModeDisabled :: UpdateProfile -> Lude.Maybe Lude.Bool) (\s a -> s {setupModeDisabled = a} :: UpdateProfile)
{-# DEPRECATED upSetupModeDisabled "Use generic-lens or generic-optics with 'setupModeDisabled' instead." #-}

-- | Whether the PSTN setting of the room profile is enabled.
--
-- /Note:/ Consider using 'pSTNEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upPSTNEnabled :: Lens.Lens' UpdateProfile (Lude.Maybe Lude.Bool)
upPSTNEnabled = Lens.lens (pSTNEnabled :: UpdateProfile -> Lude.Maybe Lude.Bool) (\s a -> s {pSTNEnabled = a} :: UpdateProfile)
{-# DEPRECATED upPSTNEnabled "Use generic-lens or generic-optics with 'pSTNEnabled' instead." #-}

-- | The updated distance unit for the room profile.
--
-- /Note:/ Consider using 'distanceUnit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upDistanceUnit :: Lens.Lens' UpdateProfile (Lude.Maybe DistanceUnit)
upDistanceUnit = Lens.lens (distanceUnit :: UpdateProfile -> Lude.Maybe DistanceUnit) (\s a -> s {distanceUnit = a} :: UpdateProfile)
{-# DEPRECATED upDistanceUnit "Use generic-lens or generic-optics with 'distanceUnit' instead." #-}

-- | The updated locale for the room profile. (This is currently only available to a limited preview audience.)
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upLocale :: Lens.Lens' UpdateProfile (Lude.Maybe Lude.Text)
upLocale = Lens.lens (locale :: UpdateProfile -> Lude.Maybe Lude.Text) (\s a -> s {locale = a} :: UpdateProfile)
{-# DEPRECATED upLocale "Use generic-lens or generic-optics with 'locale' instead." #-}

-- | The updated address for the room profile.
--
-- /Note:/ Consider using 'address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upAddress :: Lens.Lens' UpdateProfile (Lude.Maybe Lude.Text)
upAddress = Lens.lens (address :: UpdateProfile -> Lude.Maybe Lude.Text) (\s a -> s {address = a} :: UpdateProfile)
{-# DEPRECATED upAddress "Use generic-lens or generic-optics with 'address' instead." #-}

-- | The ARN of the room profile to update. Required.
--
-- /Note:/ Consider using 'profileARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upProfileARN :: Lens.Lens' UpdateProfile (Lude.Maybe Lude.Text)
upProfileARN = Lens.lens (profileARN :: UpdateProfile -> Lude.Maybe Lude.Text) (\s a -> s {profileARN = a} :: UpdateProfile)
{-# DEPRECATED upProfileARN "Use generic-lens or generic-optics with 'profileARN' instead." #-}

-- | The updated wake word for the room profile.
--
-- /Note:/ Consider using 'wakeWord' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upWakeWord :: Lens.Lens' UpdateProfile (Lude.Maybe WakeWord)
upWakeWord = Lens.lens (wakeWord :: UpdateProfile -> Lude.Maybe WakeWord) (\s a -> s {wakeWord = a} :: UpdateProfile)
{-# DEPRECATED upWakeWord "Use generic-lens or generic-optics with 'wakeWord' instead." #-}

-- | The updated meeting room settings of a room profile.
--
-- /Note:/ Consider using 'meetingRoomConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upMeetingRoomConfiguration :: Lens.Lens' UpdateProfile (Lude.Maybe UpdateMeetingRoomConfiguration)
upMeetingRoomConfiguration = Lens.lens (meetingRoomConfiguration :: UpdateProfile -> Lude.Maybe UpdateMeetingRoomConfiguration) (\s a -> s {meetingRoomConfiguration = a} :: UpdateProfile)
{-# DEPRECATED upMeetingRoomConfiguration "Use generic-lens or generic-optics with 'meetingRoomConfiguration' instead." #-}

-- | The updated name for the room profile.
--
-- /Note:/ Consider using 'profileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upProfileName :: Lens.Lens' UpdateProfile (Lude.Maybe Lude.Text)
upProfileName = Lens.lens (profileName :: UpdateProfile -> Lude.Maybe Lude.Text) (\s a -> s {profileName = a} :: UpdateProfile)
{-# DEPRECATED upProfileName "Use generic-lens or generic-optics with 'profileName' instead." #-}

-- | The updated temperature unit for the room profile.
--
-- /Note:/ Consider using 'temperatureUnit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upTemperatureUnit :: Lens.Lens' UpdateProfile (Lude.Maybe TemperatureUnit)
upTemperatureUnit = Lens.lens (temperatureUnit :: UpdateProfile -> Lude.Maybe TemperatureUnit) (\s a -> s {temperatureUnit = a} :: UpdateProfile)
{-# DEPRECATED upTemperatureUnit "Use generic-lens or generic-optics with 'temperatureUnit' instead." #-}

-- | The updated timezone for the room profile.
--
-- /Note:/ Consider using 'timezone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upTimezone :: Lens.Lens' UpdateProfile (Lude.Maybe Lude.Text)
upTimezone = Lens.lens (timezone :: UpdateProfile -> Lude.Maybe Lude.Text) (\s a -> s {timezone = a} :: UpdateProfile)
{-# DEPRECATED upTimezone "Use generic-lens or generic-optics with 'timezone' instead." #-}

-- | The updated maximum volume limit for the room profile.
--
-- /Note:/ Consider using 'maxVolumeLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upMaxVolumeLimit :: Lens.Lens' UpdateProfile (Lude.Maybe Lude.Int)
upMaxVolumeLimit = Lens.lens (maxVolumeLimit :: UpdateProfile -> Lude.Maybe Lude.Int) (\s a -> s {maxVolumeLimit = a} :: UpdateProfile)
{-# DEPRECATED upMaxVolumeLimit "Use generic-lens or generic-optics with 'maxVolumeLimit' instead." #-}

-- | Sets the profile as default if selected. If this is missing, no update is done to the default status.
--
-- /Note:/ Consider using 'isDefault' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upIsDefault :: Lens.Lens' UpdateProfile (Lude.Maybe Lude.Bool)
upIsDefault = Lens.lens (isDefault :: UpdateProfile -> Lude.Maybe Lude.Bool) (\s a -> s {isDefault = a} :: UpdateProfile)
{-# DEPRECATED upIsDefault "Use generic-lens or generic-optics with 'isDefault' instead." #-}

instance Lude.AWSRequest UpdateProfile where
  type Rs UpdateProfile = UpdateProfileResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateProfileResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateProfile where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.UpdateProfile" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateProfile where
  toJSON UpdateProfile' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SetupModeDisabled" Lude..=) Lude.<$> setupModeDisabled,
            ("PSTNEnabled" Lude..=) Lude.<$> pSTNEnabled,
            ("DistanceUnit" Lude..=) Lude.<$> distanceUnit,
            ("Locale" Lude..=) Lude.<$> locale,
            ("Address" Lude..=) Lude.<$> address,
            ("ProfileArn" Lude..=) Lude.<$> profileARN,
            ("WakeWord" Lude..=) Lude.<$> wakeWord,
            ("MeetingRoomConfiguration" Lude..=)
              Lude.<$> meetingRoomConfiguration,
            ("ProfileName" Lude..=) Lude.<$> profileName,
            ("TemperatureUnit" Lude..=) Lude.<$> temperatureUnit,
            ("Timezone" Lude..=) Lude.<$> timezone,
            ("MaxVolumeLimit" Lude..=) Lude.<$> maxVolumeLimit,
            ("IsDefault" Lude..=) Lude.<$> isDefault
          ]
      )

instance Lude.ToPath UpdateProfile where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateProfile where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateProfileResponse' smart constructor.
newtype UpdateProfileResponse = UpdateProfileResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateProfileResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateProfileResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateProfileResponse
mkUpdateProfileResponse pResponseStatus_ =
  UpdateProfileResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprsResponseStatus :: Lens.Lens' UpdateProfileResponse Lude.Int
uprsResponseStatus = Lens.lens (responseStatus :: UpdateProfileResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateProfileResponse)
{-# DEPRECATED uprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
