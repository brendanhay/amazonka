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
    cpSetupModeDisabled,
    cpPSTNEnabled,
    cpDistanceUnit,
    cpLocale,
    cpAddress,
    cpWakeWord,
    cpMeetingRoomConfiguration,
    cpProfileName,
    cpTemperatureUnit,
    cpTimezone,
    cpClientRequestToken,
    cpMaxVolumeLimit,
    cpTags,

    -- * Destructuring the response
    CreateProfileResponse (..),
    mkCreateProfileResponse,

    -- ** Response lenses
    cprsProfileARN,
    cprsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateProfile' smart constructor.
data CreateProfile = CreateProfile'
  { -- | Whether room profile setup is enabled.
    setupModeDisabled :: Lude.Maybe Lude.Bool,
    -- | Whether PSTN calling is enabled.
    pSTNEnabled :: Lude.Maybe Lude.Bool,
    -- | The distance unit to be used by devices in the profile.
    distanceUnit :: DistanceUnit,
    -- | The locale of the room profile. (This is currently only available to a limited preview audience.)
    locale :: Lude.Maybe Lude.Text,
    -- | The valid address for the room.
    address :: Lude.Text,
    -- | A wake word for Alexa, Echo, Amazon, or a computer.
    wakeWord :: WakeWord,
    -- | The meeting room settings of a room profile.
    meetingRoomConfiguration :: Lude.Maybe CreateMeetingRoomConfiguration,
    -- | The name of a room profile.
    profileName :: Lude.Text,
    -- | The temperature unit to be used by devices in the profile.
    temperatureUnit :: TemperatureUnit,
    -- | The time zone used by a room profile.
    timezone :: Lude.Text,
    -- | The user-specified token that is used during the creation of a profile.
    clientRequestToken :: Lude.Maybe Lude.Text,
    -- | The maximum volume limit for a room profile.
    maxVolumeLimit :: Lude.Maybe Lude.Int,
    -- | The tags for the profile.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateProfile' with the minimum fields required to make a request.
--
-- * 'setupModeDisabled' - Whether room profile setup is enabled.
-- * 'pSTNEnabled' - Whether PSTN calling is enabled.
-- * 'distanceUnit' - The distance unit to be used by devices in the profile.
-- * 'locale' - The locale of the room profile. (This is currently only available to a limited preview audience.)
-- * 'address' - The valid address for the room.
-- * 'wakeWord' - A wake word for Alexa, Echo, Amazon, or a computer.
-- * 'meetingRoomConfiguration' - The meeting room settings of a room profile.
-- * 'profileName' - The name of a room profile.
-- * 'temperatureUnit' - The temperature unit to be used by devices in the profile.
-- * 'timezone' - The time zone used by a room profile.
-- * 'clientRequestToken' - The user-specified token that is used during the creation of a profile.
-- * 'maxVolumeLimit' - The maximum volume limit for a room profile.
-- * 'tags' - The tags for the profile.
mkCreateProfile ::
  -- | 'distanceUnit'
  DistanceUnit ->
  -- | 'address'
  Lude.Text ->
  -- | 'wakeWord'
  WakeWord ->
  -- | 'profileName'
  Lude.Text ->
  -- | 'temperatureUnit'
  TemperatureUnit ->
  -- | 'timezone'
  Lude.Text ->
  CreateProfile
mkCreateProfile
  pDistanceUnit_
  pAddress_
  pWakeWord_
  pProfileName_
  pTemperatureUnit_
  pTimezone_ =
    CreateProfile'
      { setupModeDisabled = Lude.Nothing,
        pSTNEnabled = Lude.Nothing,
        distanceUnit = pDistanceUnit_,
        locale = Lude.Nothing,
        address = pAddress_,
        wakeWord = pWakeWord_,
        meetingRoomConfiguration = Lude.Nothing,
        profileName = pProfileName_,
        temperatureUnit = pTemperatureUnit_,
        timezone = pTimezone_,
        clientRequestToken = Lude.Nothing,
        maxVolumeLimit = Lude.Nothing,
        tags = Lude.Nothing
      }

-- | Whether room profile setup is enabled.
--
-- /Note:/ Consider using 'setupModeDisabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpSetupModeDisabled :: Lens.Lens' CreateProfile (Lude.Maybe Lude.Bool)
cpSetupModeDisabled = Lens.lens (setupModeDisabled :: CreateProfile -> Lude.Maybe Lude.Bool) (\s a -> s {setupModeDisabled = a} :: CreateProfile)
{-# DEPRECATED cpSetupModeDisabled "Use generic-lens or generic-optics with 'setupModeDisabled' instead." #-}

-- | Whether PSTN calling is enabled.
--
-- /Note:/ Consider using 'pSTNEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpPSTNEnabled :: Lens.Lens' CreateProfile (Lude.Maybe Lude.Bool)
cpPSTNEnabled = Lens.lens (pSTNEnabled :: CreateProfile -> Lude.Maybe Lude.Bool) (\s a -> s {pSTNEnabled = a} :: CreateProfile)
{-# DEPRECATED cpPSTNEnabled "Use generic-lens or generic-optics with 'pSTNEnabled' instead." #-}

-- | The distance unit to be used by devices in the profile.
--
-- /Note:/ Consider using 'distanceUnit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpDistanceUnit :: Lens.Lens' CreateProfile DistanceUnit
cpDistanceUnit = Lens.lens (distanceUnit :: CreateProfile -> DistanceUnit) (\s a -> s {distanceUnit = a} :: CreateProfile)
{-# DEPRECATED cpDistanceUnit "Use generic-lens or generic-optics with 'distanceUnit' instead." #-}

-- | The locale of the room profile. (This is currently only available to a limited preview audience.)
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpLocale :: Lens.Lens' CreateProfile (Lude.Maybe Lude.Text)
cpLocale = Lens.lens (locale :: CreateProfile -> Lude.Maybe Lude.Text) (\s a -> s {locale = a} :: CreateProfile)
{-# DEPRECATED cpLocale "Use generic-lens or generic-optics with 'locale' instead." #-}

-- | The valid address for the room.
--
-- /Note:/ Consider using 'address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpAddress :: Lens.Lens' CreateProfile Lude.Text
cpAddress = Lens.lens (address :: CreateProfile -> Lude.Text) (\s a -> s {address = a} :: CreateProfile)
{-# DEPRECATED cpAddress "Use generic-lens or generic-optics with 'address' instead." #-}

-- | A wake word for Alexa, Echo, Amazon, or a computer.
--
-- /Note:/ Consider using 'wakeWord' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpWakeWord :: Lens.Lens' CreateProfile WakeWord
cpWakeWord = Lens.lens (wakeWord :: CreateProfile -> WakeWord) (\s a -> s {wakeWord = a} :: CreateProfile)
{-# DEPRECATED cpWakeWord "Use generic-lens or generic-optics with 'wakeWord' instead." #-}

-- | The meeting room settings of a room profile.
--
-- /Note:/ Consider using 'meetingRoomConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpMeetingRoomConfiguration :: Lens.Lens' CreateProfile (Lude.Maybe CreateMeetingRoomConfiguration)
cpMeetingRoomConfiguration = Lens.lens (meetingRoomConfiguration :: CreateProfile -> Lude.Maybe CreateMeetingRoomConfiguration) (\s a -> s {meetingRoomConfiguration = a} :: CreateProfile)
{-# DEPRECATED cpMeetingRoomConfiguration "Use generic-lens or generic-optics with 'meetingRoomConfiguration' instead." #-}

-- | The name of a room profile.
--
-- /Note:/ Consider using 'profileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpProfileName :: Lens.Lens' CreateProfile Lude.Text
cpProfileName = Lens.lens (profileName :: CreateProfile -> Lude.Text) (\s a -> s {profileName = a} :: CreateProfile)
{-# DEPRECATED cpProfileName "Use generic-lens or generic-optics with 'profileName' instead." #-}

-- | The temperature unit to be used by devices in the profile.
--
-- /Note:/ Consider using 'temperatureUnit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpTemperatureUnit :: Lens.Lens' CreateProfile TemperatureUnit
cpTemperatureUnit = Lens.lens (temperatureUnit :: CreateProfile -> TemperatureUnit) (\s a -> s {temperatureUnit = a} :: CreateProfile)
{-# DEPRECATED cpTemperatureUnit "Use generic-lens or generic-optics with 'temperatureUnit' instead." #-}

-- | The time zone used by a room profile.
--
-- /Note:/ Consider using 'timezone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpTimezone :: Lens.Lens' CreateProfile Lude.Text
cpTimezone = Lens.lens (timezone :: CreateProfile -> Lude.Text) (\s a -> s {timezone = a} :: CreateProfile)
{-# DEPRECATED cpTimezone "Use generic-lens or generic-optics with 'timezone' instead." #-}

-- | The user-specified token that is used during the creation of a profile.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpClientRequestToken :: Lens.Lens' CreateProfile (Lude.Maybe Lude.Text)
cpClientRequestToken = Lens.lens (clientRequestToken :: CreateProfile -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: CreateProfile)
{-# DEPRECATED cpClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | The maximum volume limit for a room profile.
--
-- /Note:/ Consider using 'maxVolumeLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpMaxVolumeLimit :: Lens.Lens' CreateProfile (Lude.Maybe Lude.Int)
cpMaxVolumeLimit = Lens.lens (maxVolumeLimit :: CreateProfile -> Lude.Maybe Lude.Int) (\s a -> s {maxVolumeLimit = a} :: CreateProfile)
{-# DEPRECATED cpMaxVolumeLimit "Use generic-lens or generic-optics with 'maxVolumeLimit' instead." #-}

-- | The tags for the profile.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpTags :: Lens.Lens' CreateProfile (Lude.Maybe [Tag])
cpTags = Lens.lens (tags :: CreateProfile -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateProfile)
{-# DEPRECATED cpTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateProfile where
  type Rs CreateProfile = CreateProfileResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateProfileResponse'
            Lude.<$> (x Lude..?> "ProfileArn") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateProfile where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.CreateProfile" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateProfile where
  toJSON CreateProfile' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SetupModeDisabled" Lude..=) Lude.<$> setupModeDisabled,
            ("PSTNEnabled" Lude..=) Lude.<$> pSTNEnabled,
            Lude.Just ("DistanceUnit" Lude..= distanceUnit),
            ("Locale" Lude..=) Lude.<$> locale,
            Lude.Just ("Address" Lude..= address),
            Lude.Just ("WakeWord" Lude..= wakeWord),
            ("MeetingRoomConfiguration" Lude..=)
              Lude.<$> meetingRoomConfiguration,
            Lude.Just ("ProfileName" Lude..= profileName),
            Lude.Just ("TemperatureUnit" Lude..= temperatureUnit),
            Lude.Just ("Timezone" Lude..= timezone),
            ("ClientRequestToken" Lude..=) Lude.<$> clientRequestToken,
            ("MaxVolumeLimit" Lude..=) Lude.<$> maxVolumeLimit,
            ("Tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateProfile where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateProfile where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateProfileResponse' smart constructor.
data CreateProfileResponse = CreateProfileResponse'
  { -- | The ARN of the newly created room profile in the response.
    profileARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateProfileResponse' with the minimum fields required to make a request.
--
-- * 'profileARN' - The ARN of the newly created room profile in the response.
-- * 'responseStatus' - The response status code.
mkCreateProfileResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateProfileResponse
mkCreateProfileResponse pResponseStatus_ =
  CreateProfileResponse'
    { profileARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the newly created room profile in the response.
--
-- /Note:/ Consider using 'profileARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprsProfileARN :: Lens.Lens' CreateProfileResponse (Lude.Maybe Lude.Text)
cprsProfileARN = Lens.lens (profileARN :: CreateProfileResponse -> Lude.Maybe Lude.Text) (\s a -> s {profileARN = a} :: CreateProfileResponse)
{-# DEPRECATED cprsProfileARN "Use generic-lens or generic-optics with 'profileARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprsResponseStatus :: Lens.Lens' CreateProfileResponse Lude.Int
cprsResponseStatus = Lens.lens (responseStatus :: CreateProfileResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateProfileResponse)
{-# DEPRECATED cprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
