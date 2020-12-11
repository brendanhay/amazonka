-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.EventContextDataType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.EventContextDataType
  ( EventContextDataType (..),

    -- * Smart constructor
    mkEventContextDataType,

    -- * Lenses
    ecdtIPAddress,
    ecdtCountry,
    ecdtCity,
    ecdtDeviceName,
    ecdtTimezone,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the user context data captured at the time of an event request.
--
-- /See:/ 'mkEventContextDataType' smart constructor.
data EventContextDataType = EventContextDataType'
  { ipAddress ::
      Lude.Maybe Lude.Text,
    country :: Lude.Maybe Lude.Text,
    city :: Lude.Maybe Lude.Text,
    deviceName :: Lude.Maybe Lude.Text,
    timezone :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EventContextDataType' with the minimum fields required to make a request.
--
-- * 'city' - The user's city.
-- * 'country' - The user's country.
-- * 'deviceName' - The user's device name.
-- * 'ipAddress' - The user's IP address.
-- * 'timezone' - The user's time zone.
mkEventContextDataType ::
  EventContextDataType
mkEventContextDataType =
  EventContextDataType'
    { ipAddress = Lude.Nothing,
      country = Lude.Nothing,
      city = Lude.Nothing,
      deviceName = Lude.Nothing,
      timezone = Lude.Nothing
    }

-- | The user's IP address.
--
-- /Note:/ Consider using 'ipAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecdtIPAddress :: Lens.Lens' EventContextDataType (Lude.Maybe Lude.Text)
ecdtIPAddress = Lens.lens (ipAddress :: EventContextDataType -> Lude.Maybe Lude.Text) (\s a -> s {ipAddress = a} :: EventContextDataType)
{-# DEPRECATED ecdtIPAddress "Use generic-lens or generic-optics with 'ipAddress' instead." #-}

-- | The user's country.
--
-- /Note:/ Consider using 'country' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecdtCountry :: Lens.Lens' EventContextDataType (Lude.Maybe Lude.Text)
ecdtCountry = Lens.lens (country :: EventContextDataType -> Lude.Maybe Lude.Text) (\s a -> s {country = a} :: EventContextDataType)
{-# DEPRECATED ecdtCountry "Use generic-lens or generic-optics with 'country' instead." #-}

-- | The user's city.
--
-- /Note:/ Consider using 'city' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecdtCity :: Lens.Lens' EventContextDataType (Lude.Maybe Lude.Text)
ecdtCity = Lens.lens (city :: EventContextDataType -> Lude.Maybe Lude.Text) (\s a -> s {city = a} :: EventContextDataType)
{-# DEPRECATED ecdtCity "Use generic-lens or generic-optics with 'city' instead." #-}

-- | The user's device name.
--
-- /Note:/ Consider using 'deviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecdtDeviceName :: Lens.Lens' EventContextDataType (Lude.Maybe Lude.Text)
ecdtDeviceName = Lens.lens (deviceName :: EventContextDataType -> Lude.Maybe Lude.Text) (\s a -> s {deviceName = a} :: EventContextDataType)
{-# DEPRECATED ecdtDeviceName "Use generic-lens or generic-optics with 'deviceName' instead." #-}

-- | The user's time zone.
--
-- /Note:/ Consider using 'timezone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecdtTimezone :: Lens.Lens' EventContextDataType (Lude.Maybe Lude.Text)
ecdtTimezone = Lens.lens (timezone :: EventContextDataType -> Lude.Maybe Lude.Text) (\s a -> s {timezone = a} :: EventContextDataType)
{-# DEPRECATED ecdtTimezone "Use generic-lens or generic-optics with 'timezone' instead." #-}

instance Lude.FromJSON EventContextDataType where
  parseJSON =
    Lude.withObject
      "EventContextDataType"
      ( \x ->
          EventContextDataType'
            Lude.<$> (x Lude..:? "IpAddress")
            Lude.<*> (x Lude..:? "Country")
            Lude.<*> (x Lude..:? "City")
            Lude.<*> (x Lude..:? "DeviceName")
            Lude.<*> (x Lude..:? "Timezone")
      )
