-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.Profile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.Profile
  ( Profile (..),

    -- * Smart constructor
    mkProfile,

    -- * Lenses
    pSetupModeDisabled,
    pPSTNEnabled,
    pAddressBookARN,
    pDistanceUnit,
    pLocale,
    pAddress,
    pProfileARN,
    pWakeWord,
    pMeetingRoomConfiguration,
    pProfileName,
    pTemperatureUnit,
    pTimezone,
    pMaxVolumeLimit,
    pIsDefault,
  )
where

import Network.AWS.AlexaBusiness.Types.DistanceUnit
import Network.AWS.AlexaBusiness.Types.MeetingRoomConfiguration
import Network.AWS.AlexaBusiness.Types.TemperatureUnit
import Network.AWS.AlexaBusiness.Types.WakeWord
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A room profile with attributes.
--
-- /See:/ 'mkProfile' smart constructor.
data Profile = Profile'
  { setupModeDisabled :: Lude.Maybe Lude.Bool,
    pSTNEnabled :: Lude.Maybe Lude.Bool,
    addressBookARN :: Lude.Maybe Lude.Text,
    distanceUnit :: Lude.Maybe DistanceUnit,
    locale :: Lude.Maybe Lude.Text,
    address :: Lude.Maybe Lude.Text,
    profileARN :: Lude.Maybe Lude.Text,
    wakeWord :: Lude.Maybe WakeWord,
    meetingRoomConfiguration :: Lude.Maybe MeetingRoomConfiguration,
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

-- | Creates a value of 'Profile' with the minimum fields required to make a request.
--
-- * 'address' - The address of a room profile.
-- * 'addressBookARN' - The ARN of the address book.
-- * 'distanceUnit' - The distance unit of a room profile.
-- * 'isDefault' - Retrieves if the profile is default or not.
-- * 'locale' - The locale of a room profile. (This is currently available only to a limited preview audience.)
-- * 'maxVolumeLimit' - The max volume limit of a room profile.
-- * 'meetingRoomConfiguration' - Meeting room settings of a room profile.
-- * 'pSTNEnabled' - The PSTN setting of a room profile.
-- * 'profileARN' - The ARN of a room profile.
-- * 'profileName' - The name of a room profile.
-- * 'setupModeDisabled' - The setup mode of a room profile.
-- * 'temperatureUnit' - The temperature unit of a room profile.
-- * 'timezone' - The time zone of a room profile.
-- * 'wakeWord' - The wake word of a room profile.
mkProfile ::
  Profile
mkProfile =
  Profile'
    { setupModeDisabled = Lude.Nothing,
      pSTNEnabled = Lude.Nothing,
      addressBookARN = Lude.Nothing,
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

-- | The setup mode of a room profile.
--
-- /Note:/ Consider using 'setupModeDisabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pSetupModeDisabled :: Lens.Lens' Profile (Lude.Maybe Lude.Bool)
pSetupModeDisabled = Lens.lens (setupModeDisabled :: Profile -> Lude.Maybe Lude.Bool) (\s a -> s {setupModeDisabled = a} :: Profile)
{-# DEPRECATED pSetupModeDisabled "Use generic-lens or generic-optics with 'setupModeDisabled' instead." #-}

-- | The PSTN setting of a room profile.
--
-- /Note:/ Consider using 'pSTNEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPSTNEnabled :: Lens.Lens' Profile (Lude.Maybe Lude.Bool)
pPSTNEnabled = Lens.lens (pSTNEnabled :: Profile -> Lude.Maybe Lude.Bool) (\s a -> s {pSTNEnabled = a} :: Profile)
{-# DEPRECATED pPSTNEnabled "Use generic-lens or generic-optics with 'pSTNEnabled' instead." #-}

-- | The ARN of the address book.
--
-- /Note:/ Consider using 'addressBookARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pAddressBookARN :: Lens.Lens' Profile (Lude.Maybe Lude.Text)
pAddressBookARN = Lens.lens (addressBookARN :: Profile -> Lude.Maybe Lude.Text) (\s a -> s {addressBookARN = a} :: Profile)
{-# DEPRECATED pAddressBookARN "Use generic-lens or generic-optics with 'addressBookARN' instead." #-}

-- | The distance unit of a room profile.
--
-- /Note:/ Consider using 'distanceUnit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pDistanceUnit :: Lens.Lens' Profile (Lude.Maybe DistanceUnit)
pDistanceUnit = Lens.lens (distanceUnit :: Profile -> Lude.Maybe DistanceUnit) (\s a -> s {distanceUnit = a} :: Profile)
{-# DEPRECATED pDistanceUnit "Use generic-lens or generic-optics with 'distanceUnit' instead." #-}

-- | The locale of a room profile. (This is currently available only to a limited preview audience.)
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pLocale :: Lens.Lens' Profile (Lude.Maybe Lude.Text)
pLocale = Lens.lens (locale :: Profile -> Lude.Maybe Lude.Text) (\s a -> s {locale = a} :: Profile)
{-# DEPRECATED pLocale "Use generic-lens or generic-optics with 'locale' instead." #-}

-- | The address of a room profile.
--
-- /Note:/ Consider using 'address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pAddress :: Lens.Lens' Profile (Lude.Maybe Lude.Text)
pAddress = Lens.lens (address :: Profile -> Lude.Maybe Lude.Text) (\s a -> s {address = a} :: Profile)
{-# DEPRECATED pAddress "Use generic-lens or generic-optics with 'address' instead." #-}

-- | The ARN of a room profile.
--
-- /Note:/ Consider using 'profileARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pProfileARN :: Lens.Lens' Profile (Lude.Maybe Lude.Text)
pProfileARN = Lens.lens (profileARN :: Profile -> Lude.Maybe Lude.Text) (\s a -> s {profileARN = a} :: Profile)
{-# DEPRECATED pProfileARN "Use generic-lens or generic-optics with 'profileARN' instead." #-}

-- | The wake word of a room profile.
--
-- /Note:/ Consider using 'wakeWord' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pWakeWord :: Lens.Lens' Profile (Lude.Maybe WakeWord)
pWakeWord = Lens.lens (wakeWord :: Profile -> Lude.Maybe WakeWord) (\s a -> s {wakeWord = a} :: Profile)
{-# DEPRECATED pWakeWord "Use generic-lens or generic-optics with 'wakeWord' instead." #-}

-- | Meeting room settings of a room profile.
--
-- /Note:/ Consider using 'meetingRoomConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pMeetingRoomConfiguration :: Lens.Lens' Profile (Lude.Maybe MeetingRoomConfiguration)
pMeetingRoomConfiguration = Lens.lens (meetingRoomConfiguration :: Profile -> Lude.Maybe MeetingRoomConfiguration) (\s a -> s {meetingRoomConfiguration = a} :: Profile)
{-# DEPRECATED pMeetingRoomConfiguration "Use generic-lens or generic-optics with 'meetingRoomConfiguration' instead." #-}

-- | The name of a room profile.
--
-- /Note:/ Consider using 'profileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pProfileName :: Lens.Lens' Profile (Lude.Maybe Lude.Text)
pProfileName = Lens.lens (profileName :: Profile -> Lude.Maybe Lude.Text) (\s a -> s {profileName = a} :: Profile)
{-# DEPRECATED pProfileName "Use generic-lens or generic-optics with 'profileName' instead." #-}

-- | The temperature unit of a room profile.
--
-- /Note:/ Consider using 'temperatureUnit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pTemperatureUnit :: Lens.Lens' Profile (Lude.Maybe TemperatureUnit)
pTemperatureUnit = Lens.lens (temperatureUnit :: Profile -> Lude.Maybe TemperatureUnit) (\s a -> s {temperatureUnit = a} :: Profile)
{-# DEPRECATED pTemperatureUnit "Use generic-lens or generic-optics with 'temperatureUnit' instead." #-}

-- | The time zone of a room profile.
--
-- /Note:/ Consider using 'timezone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pTimezone :: Lens.Lens' Profile (Lude.Maybe Lude.Text)
pTimezone = Lens.lens (timezone :: Profile -> Lude.Maybe Lude.Text) (\s a -> s {timezone = a} :: Profile)
{-# DEPRECATED pTimezone "Use generic-lens or generic-optics with 'timezone' instead." #-}

-- | The max volume limit of a room profile.
--
-- /Note:/ Consider using 'maxVolumeLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pMaxVolumeLimit :: Lens.Lens' Profile (Lude.Maybe Lude.Int)
pMaxVolumeLimit = Lens.lens (maxVolumeLimit :: Profile -> Lude.Maybe Lude.Int) (\s a -> s {maxVolumeLimit = a} :: Profile)
{-# DEPRECATED pMaxVolumeLimit "Use generic-lens or generic-optics with 'maxVolumeLimit' instead." #-}

-- | Retrieves if the profile is default or not.
--
-- /Note:/ Consider using 'isDefault' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pIsDefault :: Lens.Lens' Profile (Lude.Maybe Lude.Bool)
pIsDefault = Lens.lens (isDefault :: Profile -> Lude.Maybe Lude.Bool) (\s a -> s {isDefault = a} :: Profile)
{-# DEPRECATED pIsDefault "Use generic-lens or generic-optics with 'isDefault' instead." #-}

instance Lude.FromJSON Profile where
  parseJSON =
    Lude.withObject
      "Profile"
      ( \x ->
          Profile'
            Lude.<$> (x Lude..:? "SetupModeDisabled")
            Lude.<*> (x Lude..:? "PSTNEnabled")
            Lude.<*> (x Lude..:? "AddressBookArn")
            Lude.<*> (x Lude..:? "DistanceUnit")
            Lude.<*> (x Lude..:? "Locale")
            Lude.<*> (x Lude..:? "Address")
            Lude.<*> (x Lude..:? "ProfileArn")
            Lude.<*> (x Lude..:? "WakeWord")
            Lude.<*> (x Lude..:? "MeetingRoomConfiguration")
            Lude.<*> (x Lude..:? "ProfileName")
            Lude.<*> (x Lude..:? "TemperatureUnit")
            Lude.<*> (x Lude..:? "Timezone")
            Lude.<*> (x Lude..:? "MaxVolumeLimit")
            Lude.<*> (x Lude..:? "IsDefault")
      )
