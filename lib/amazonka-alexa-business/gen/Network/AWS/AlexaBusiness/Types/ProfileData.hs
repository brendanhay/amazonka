-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.ProfileData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.ProfileData
  ( ProfileData (..),

    -- * Smart constructor
    mkProfileData,

    -- * Lenses
    pdDistanceUnit,
    pdLocale,
    pdAddress,
    pdProfileARN,
    pdWakeWord,
    pdProfileName,
    pdTemperatureUnit,
    pdTimezone,
    pdIsDefault,
  )
where

import Network.AWS.AlexaBusiness.Types.DistanceUnit
import Network.AWS.AlexaBusiness.Types.TemperatureUnit
import Network.AWS.AlexaBusiness.Types.WakeWord
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The data of a room profile.
--
-- /See:/ 'mkProfileData' smart constructor.
data ProfileData = ProfileData'
  { distanceUnit ::
      Lude.Maybe DistanceUnit,
    locale :: Lude.Maybe Lude.Text,
    address :: Lude.Maybe Lude.Text,
    profileARN :: Lude.Maybe Lude.Text,
    wakeWord :: Lude.Maybe WakeWord,
    profileName :: Lude.Maybe Lude.Text,
    temperatureUnit :: Lude.Maybe TemperatureUnit,
    timezone :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ProfileData' with the minimum fields required to make a request.
--
-- * 'address' - The address of a room profile.
-- * 'distanceUnit' - The distance unit of a room profile.
-- * 'isDefault' - Retrieves if the profile data is default or not.
-- * 'locale' - The locale of a room profile. (This is currently available only to a limited preview audience.)
-- * 'profileARN' - The ARN of a room profile.
-- * 'profileName' - The name of a room profile.
-- * 'temperatureUnit' - The temperature unit of a room profile.
-- * 'timezone' - The time zone of a room profile.
-- * 'wakeWord' - The wake word of a room profile.
mkProfileData ::
  ProfileData
mkProfileData =
  ProfileData'
    { distanceUnit = Lude.Nothing,
      locale = Lude.Nothing,
      address = Lude.Nothing,
      profileARN = Lude.Nothing,
      wakeWord = Lude.Nothing,
      profileName = Lude.Nothing,
      temperatureUnit = Lude.Nothing,
      timezone = Lude.Nothing,
      isDefault = Lude.Nothing
    }

-- | The distance unit of a room profile.
--
-- /Note:/ Consider using 'distanceUnit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdDistanceUnit :: Lens.Lens' ProfileData (Lude.Maybe DistanceUnit)
pdDistanceUnit = Lens.lens (distanceUnit :: ProfileData -> Lude.Maybe DistanceUnit) (\s a -> s {distanceUnit = a} :: ProfileData)
{-# DEPRECATED pdDistanceUnit "Use generic-lens or generic-optics with 'distanceUnit' instead." #-}

-- | The locale of a room profile. (This is currently available only to a limited preview audience.)
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdLocale :: Lens.Lens' ProfileData (Lude.Maybe Lude.Text)
pdLocale = Lens.lens (locale :: ProfileData -> Lude.Maybe Lude.Text) (\s a -> s {locale = a} :: ProfileData)
{-# DEPRECATED pdLocale "Use generic-lens or generic-optics with 'locale' instead." #-}

-- | The address of a room profile.
--
-- /Note:/ Consider using 'address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdAddress :: Lens.Lens' ProfileData (Lude.Maybe Lude.Text)
pdAddress = Lens.lens (address :: ProfileData -> Lude.Maybe Lude.Text) (\s a -> s {address = a} :: ProfileData)
{-# DEPRECATED pdAddress "Use generic-lens or generic-optics with 'address' instead." #-}

-- | The ARN of a room profile.
--
-- /Note:/ Consider using 'profileARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdProfileARN :: Lens.Lens' ProfileData (Lude.Maybe Lude.Text)
pdProfileARN = Lens.lens (profileARN :: ProfileData -> Lude.Maybe Lude.Text) (\s a -> s {profileARN = a} :: ProfileData)
{-# DEPRECATED pdProfileARN "Use generic-lens or generic-optics with 'profileARN' instead." #-}

-- | The wake word of a room profile.
--
-- /Note:/ Consider using 'wakeWord' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdWakeWord :: Lens.Lens' ProfileData (Lude.Maybe WakeWord)
pdWakeWord = Lens.lens (wakeWord :: ProfileData -> Lude.Maybe WakeWord) (\s a -> s {wakeWord = a} :: ProfileData)
{-# DEPRECATED pdWakeWord "Use generic-lens or generic-optics with 'wakeWord' instead." #-}

-- | The name of a room profile.
--
-- /Note:/ Consider using 'profileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdProfileName :: Lens.Lens' ProfileData (Lude.Maybe Lude.Text)
pdProfileName = Lens.lens (profileName :: ProfileData -> Lude.Maybe Lude.Text) (\s a -> s {profileName = a} :: ProfileData)
{-# DEPRECATED pdProfileName "Use generic-lens or generic-optics with 'profileName' instead." #-}

-- | The temperature unit of a room profile.
--
-- /Note:/ Consider using 'temperatureUnit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdTemperatureUnit :: Lens.Lens' ProfileData (Lude.Maybe TemperatureUnit)
pdTemperatureUnit = Lens.lens (temperatureUnit :: ProfileData -> Lude.Maybe TemperatureUnit) (\s a -> s {temperatureUnit = a} :: ProfileData)
{-# DEPRECATED pdTemperatureUnit "Use generic-lens or generic-optics with 'temperatureUnit' instead." #-}

-- | The time zone of a room profile.
--
-- /Note:/ Consider using 'timezone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdTimezone :: Lens.Lens' ProfileData (Lude.Maybe Lude.Text)
pdTimezone = Lens.lens (timezone :: ProfileData -> Lude.Maybe Lude.Text) (\s a -> s {timezone = a} :: ProfileData)
{-# DEPRECATED pdTimezone "Use generic-lens or generic-optics with 'timezone' instead." #-}

-- | Retrieves if the profile data is default or not.
--
-- /Note:/ Consider using 'isDefault' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdIsDefault :: Lens.Lens' ProfileData (Lude.Maybe Lude.Bool)
pdIsDefault = Lens.lens (isDefault :: ProfileData -> Lude.Maybe Lude.Bool) (\s a -> s {isDefault = a} :: ProfileData)
{-# DEPRECATED pdIsDefault "Use generic-lens or generic-optics with 'isDefault' instead." #-}

instance Lude.FromJSON ProfileData where
  parseJSON =
    Lude.withObject
      "ProfileData"
      ( \x ->
          ProfileData'
            Lude.<$> (x Lude..:? "DistanceUnit")
            Lude.<*> (x Lude..:? "Locale")
            Lude.<*> (x Lude..:? "Address")
            Lude.<*> (x Lude..:? "ProfileArn")
            Lude.<*> (x Lude..:? "WakeWord")
            Lude.<*> (x Lude..:? "ProfileName")
            Lude.<*> (x Lude..:? "TemperatureUnit")
            Lude.<*> (x Lude..:? "Timezone")
            Lude.<*> (x Lude..:? "IsDefault")
      )
