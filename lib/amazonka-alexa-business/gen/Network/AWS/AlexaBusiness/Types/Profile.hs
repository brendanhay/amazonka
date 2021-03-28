{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.Profile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.Profile
  ( Profile (..)
  -- * Smart constructor
  , mkProfile
  -- * Lenses
  , pAddress
  , pAddressBookArn
  , pDistanceUnit
  , pIsDefault
  , pLocale
  , pMaxVolumeLimit
  , pMeetingRoomConfiguration
  , pPSTNEnabled
  , pProfileArn
  , pProfileName
  , pSetupModeDisabled
  , pTemperatureUnit
  , pTimezone
  , pWakeWord
  ) where

import qualified Network.AWS.AlexaBusiness.Types.Address as Types
import qualified Network.AWS.AlexaBusiness.Types.Arn as Types
import qualified Network.AWS.AlexaBusiness.Types.DeviceLocale as Types
import qualified Network.AWS.AlexaBusiness.Types.DistanceUnit as Types
import qualified Network.AWS.AlexaBusiness.Types.MeetingRoomConfiguration as Types
import qualified Network.AWS.AlexaBusiness.Types.ProfileName as Types
import qualified Network.AWS.AlexaBusiness.Types.TemperatureUnit as Types
import qualified Network.AWS.AlexaBusiness.Types.Timezone as Types
import qualified Network.AWS.AlexaBusiness.Types.WakeWord as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A room profile with attributes.
--
-- /See:/ 'mkProfile' smart constructor.
data Profile = Profile'
  { address :: Core.Maybe Types.Address
    -- ^ The address of a room profile.
  , addressBookArn :: Core.Maybe Types.Arn
    -- ^ The ARN of the address book.
  , distanceUnit :: Core.Maybe Types.DistanceUnit
    -- ^ The distance unit of a room profile.
  , isDefault :: Core.Maybe Core.Bool
    -- ^ Retrieves if the profile is default or not.
  , locale :: Core.Maybe Types.DeviceLocale
    -- ^ The locale of a room profile. (This is currently available only to a limited preview audience.)
  , maxVolumeLimit :: Core.Maybe Core.Int
    -- ^ The max volume limit of a room profile.
  , meetingRoomConfiguration :: Core.Maybe Types.MeetingRoomConfiguration
    -- ^ Meeting room settings of a room profile.
  , pSTNEnabled :: Core.Maybe Core.Bool
    -- ^ The PSTN setting of a room profile.
  , profileArn :: Core.Maybe Types.Arn
    -- ^ The ARN of a room profile.
  , profileName :: Core.Maybe Types.ProfileName
    -- ^ The name of a room profile.
  , setupModeDisabled :: Core.Maybe Core.Bool
    -- ^ The setup mode of a room profile.
  , temperatureUnit :: Core.Maybe Types.TemperatureUnit
    -- ^ The temperature unit of a room profile.
  , timezone :: Core.Maybe Types.Timezone
    -- ^ The time zone of a room profile.
  , wakeWord :: Core.Maybe Types.WakeWord
    -- ^ The wake word of a room profile.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Profile' value with any optional fields omitted.
mkProfile
    :: Profile
mkProfile
  = Profile'{address = Core.Nothing, addressBookArn = Core.Nothing,
             distanceUnit = Core.Nothing, isDefault = Core.Nothing,
             locale = Core.Nothing, maxVolumeLimit = Core.Nothing,
             meetingRoomConfiguration = Core.Nothing,
             pSTNEnabled = Core.Nothing, profileArn = Core.Nothing,
             profileName = Core.Nothing, setupModeDisabled = Core.Nothing,
             temperatureUnit = Core.Nothing, timezone = Core.Nothing,
             wakeWord = Core.Nothing}

-- | The address of a room profile.
--
-- /Note:/ Consider using 'address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pAddress :: Lens.Lens' Profile (Core.Maybe Types.Address)
pAddress = Lens.field @"address"
{-# INLINEABLE pAddress #-}
{-# DEPRECATED address "Use generic-lens or generic-optics with 'address' instead"  #-}

-- | The ARN of the address book.
--
-- /Note:/ Consider using 'addressBookArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pAddressBookArn :: Lens.Lens' Profile (Core.Maybe Types.Arn)
pAddressBookArn = Lens.field @"addressBookArn"
{-# INLINEABLE pAddressBookArn #-}
{-# DEPRECATED addressBookArn "Use generic-lens or generic-optics with 'addressBookArn' instead"  #-}

-- | The distance unit of a room profile.
--
-- /Note:/ Consider using 'distanceUnit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pDistanceUnit :: Lens.Lens' Profile (Core.Maybe Types.DistanceUnit)
pDistanceUnit = Lens.field @"distanceUnit"
{-# INLINEABLE pDistanceUnit #-}
{-# DEPRECATED distanceUnit "Use generic-lens or generic-optics with 'distanceUnit' instead"  #-}

-- | Retrieves if the profile is default or not.
--
-- /Note:/ Consider using 'isDefault' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pIsDefault :: Lens.Lens' Profile (Core.Maybe Core.Bool)
pIsDefault = Lens.field @"isDefault"
{-# INLINEABLE pIsDefault #-}
{-# DEPRECATED isDefault "Use generic-lens or generic-optics with 'isDefault' instead"  #-}

-- | The locale of a room profile. (This is currently available only to a limited preview audience.)
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pLocale :: Lens.Lens' Profile (Core.Maybe Types.DeviceLocale)
pLocale = Lens.field @"locale"
{-# INLINEABLE pLocale #-}
{-# DEPRECATED locale "Use generic-lens or generic-optics with 'locale' instead"  #-}

-- | The max volume limit of a room profile.
--
-- /Note:/ Consider using 'maxVolumeLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pMaxVolumeLimit :: Lens.Lens' Profile (Core.Maybe Core.Int)
pMaxVolumeLimit = Lens.field @"maxVolumeLimit"
{-# INLINEABLE pMaxVolumeLimit #-}
{-# DEPRECATED maxVolumeLimit "Use generic-lens or generic-optics with 'maxVolumeLimit' instead"  #-}

-- | Meeting room settings of a room profile.
--
-- /Note:/ Consider using 'meetingRoomConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pMeetingRoomConfiguration :: Lens.Lens' Profile (Core.Maybe Types.MeetingRoomConfiguration)
pMeetingRoomConfiguration = Lens.field @"meetingRoomConfiguration"
{-# INLINEABLE pMeetingRoomConfiguration #-}
{-# DEPRECATED meetingRoomConfiguration "Use generic-lens or generic-optics with 'meetingRoomConfiguration' instead"  #-}

-- | The PSTN setting of a room profile.
--
-- /Note:/ Consider using 'pSTNEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPSTNEnabled :: Lens.Lens' Profile (Core.Maybe Core.Bool)
pPSTNEnabled = Lens.field @"pSTNEnabled"
{-# INLINEABLE pPSTNEnabled #-}
{-# DEPRECATED pSTNEnabled "Use generic-lens or generic-optics with 'pSTNEnabled' instead"  #-}

-- | The ARN of a room profile.
--
-- /Note:/ Consider using 'profileArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pProfileArn :: Lens.Lens' Profile (Core.Maybe Types.Arn)
pProfileArn = Lens.field @"profileArn"
{-# INLINEABLE pProfileArn #-}
{-# DEPRECATED profileArn "Use generic-lens or generic-optics with 'profileArn' instead"  #-}

-- | The name of a room profile.
--
-- /Note:/ Consider using 'profileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pProfileName :: Lens.Lens' Profile (Core.Maybe Types.ProfileName)
pProfileName = Lens.field @"profileName"
{-# INLINEABLE pProfileName #-}
{-# DEPRECATED profileName "Use generic-lens or generic-optics with 'profileName' instead"  #-}

-- | The setup mode of a room profile.
--
-- /Note:/ Consider using 'setupModeDisabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pSetupModeDisabled :: Lens.Lens' Profile (Core.Maybe Core.Bool)
pSetupModeDisabled = Lens.field @"setupModeDisabled"
{-# INLINEABLE pSetupModeDisabled #-}
{-# DEPRECATED setupModeDisabled "Use generic-lens or generic-optics with 'setupModeDisabled' instead"  #-}

-- | The temperature unit of a room profile.
--
-- /Note:/ Consider using 'temperatureUnit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pTemperatureUnit :: Lens.Lens' Profile (Core.Maybe Types.TemperatureUnit)
pTemperatureUnit = Lens.field @"temperatureUnit"
{-# INLINEABLE pTemperatureUnit #-}
{-# DEPRECATED temperatureUnit "Use generic-lens or generic-optics with 'temperatureUnit' instead"  #-}

-- | The time zone of a room profile.
--
-- /Note:/ Consider using 'timezone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pTimezone :: Lens.Lens' Profile (Core.Maybe Types.Timezone)
pTimezone = Lens.field @"timezone"
{-# INLINEABLE pTimezone #-}
{-# DEPRECATED timezone "Use generic-lens or generic-optics with 'timezone' instead"  #-}

-- | The wake word of a room profile.
--
-- /Note:/ Consider using 'wakeWord' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pWakeWord :: Lens.Lens' Profile (Core.Maybe Types.WakeWord)
pWakeWord = Lens.field @"wakeWord"
{-# INLINEABLE pWakeWord #-}
{-# DEPRECATED wakeWord "Use generic-lens or generic-optics with 'wakeWord' instead"  #-}

instance Core.FromJSON Profile where
        parseJSON
          = Core.withObject "Profile" Core.$
              \ x ->
                Profile' Core.<$>
                  (x Core..:? "Address") Core.<*> x Core..:? "AddressBookArn"
                    Core.<*> x Core..:? "DistanceUnit"
                    Core.<*> x Core..:? "IsDefault"
                    Core.<*> x Core..:? "Locale"
                    Core.<*> x Core..:? "MaxVolumeLimit"
                    Core.<*> x Core..:? "MeetingRoomConfiguration"
                    Core.<*> x Core..:? "PSTNEnabled"
                    Core.<*> x Core..:? "ProfileArn"
                    Core.<*> x Core..:? "ProfileName"
                    Core.<*> x Core..:? "SetupModeDisabled"
                    Core.<*> x Core..:? "TemperatureUnit"
                    Core.<*> x Core..:? "Timezone"
                    Core.<*> x Core..:? "WakeWord"
