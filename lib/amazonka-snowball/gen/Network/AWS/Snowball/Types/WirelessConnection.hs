{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.WirelessConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.WirelessConnection
  ( WirelessConnection (..),

    -- * Smart constructor
    mkWirelessConnection,

    -- * Lenses
    wcIsWifiEnabled,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Configures the wireless connection on an AWS Snowcone device.
--
-- /See:/ 'mkWirelessConnection' smart constructor.
newtype WirelessConnection = WirelessConnection'
  { isWifiEnabled ::
      Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WirelessConnection' with the minimum fields required to make a request.
--
-- * 'isWifiEnabled' - Enables the Wi-Fi adapter on an AWS Snowcone device.
mkWirelessConnection ::
  WirelessConnection
mkWirelessConnection =
  WirelessConnection' {isWifiEnabled = Lude.Nothing}

-- | Enables the Wi-Fi adapter on an AWS Snowcone device.
--
-- /Note:/ Consider using 'isWifiEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wcIsWifiEnabled :: Lens.Lens' WirelessConnection (Lude.Maybe Lude.Bool)
wcIsWifiEnabled = Lens.lens (isWifiEnabled :: WirelessConnection -> Lude.Maybe Lude.Bool) (\s a -> s {isWifiEnabled = a} :: WirelessConnection)
{-# DEPRECATED wcIsWifiEnabled "Use generic-lens or generic-optics with 'isWifiEnabled' instead." #-}

instance Lude.FromJSON WirelessConnection where
  parseJSON =
    Lude.withObject
      "WirelessConnection"
      (\x -> WirelessConnection' Lude.<$> (x Lude..:? "IsWifiEnabled"))

instance Lude.ToJSON WirelessConnection where
  toJSON WirelessConnection' {..} =
    Lude.object
      (Lude.catMaybes [("IsWifiEnabled" Lude..=) Lude.<$> isWifiEnabled])
