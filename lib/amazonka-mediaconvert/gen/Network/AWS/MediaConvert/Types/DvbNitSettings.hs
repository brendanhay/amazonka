{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DvbNitSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DvbNitSettings
  ( DvbNitSettings (..),

    -- * Smart constructor
    mkDvbNitSettings,

    -- * Lenses
    dnsNetworkId,
    dnsNetworkName,
    dnsNitInterval,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Inserts DVB Network Information Table (NIT) at the specified table repetition interval.
--
-- /See:/ 'mkDvbNitSettings' smart constructor.
data DvbNitSettings = DvbNitSettings'
  { networkId ::
      Lude.Maybe Lude.Natural,
    networkName :: Lude.Maybe Lude.Text,
    nitInterval :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DvbNitSettings' with the minimum fields required to make a request.
--
-- * 'networkId' - The numeric value placed in the Network Information Table (NIT).
-- * 'networkName' - The network name text placed in the network_name_descriptor inside the Network Information Table. Maximum length is 256 characters.
-- * 'nitInterval' - The number of milliseconds between instances of this table in the output transport stream.
mkDvbNitSettings ::
  DvbNitSettings
mkDvbNitSettings =
  DvbNitSettings'
    { networkId = Lude.Nothing,
      networkName = Lude.Nothing,
      nitInterval = Lude.Nothing
    }

-- | The numeric value placed in the Network Information Table (NIT).
--
-- /Note:/ Consider using 'networkId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnsNetworkId :: Lens.Lens' DvbNitSettings (Lude.Maybe Lude.Natural)
dnsNetworkId = Lens.lens (networkId :: DvbNitSettings -> Lude.Maybe Lude.Natural) (\s a -> s {networkId = a} :: DvbNitSettings)
{-# DEPRECATED dnsNetworkId "Use generic-lens or generic-optics with 'networkId' instead." #-}

-- | The network name text placed in the network_name_descriptor inside the Network Information Table. Maximum length is 256 characters.
--
-- /Note:/ Consider using 'networkName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnsNetworkName :: Lens.Lens' DvbNitSettings (Lude.Maybe Lude.Text)
dnsNetworkName = Lens.lens (networkName :: DvbNitSettings -> Lude.Maybe Lude.Text) (\s a -> s {networkName = a} :: DvbNitSettings)
{-# DEPRECATED dnsNetworkName "Use generic-lens or generic-optics with 'networkName' instead." #-}

-- | The number of milliseconds between instances of this table in the output transport stream.
--
-- /Note:/ Consider using 'nitInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnsNitInterval :: Lens.Lens' DvbNitSettings (Lude.Maybe Lude.Natural)
dnsNitInterval = Lens.lens (nitInterval :: DvbNitSettings -> Lude.Maybe Lude.Natural) (\s a -> s {nitInterval = a} :: DvbNitSettings)
{-# DEPRECATED dnsNitInterval "Use generic-lens or generic-optics with 'nitInterval' instead." #-}

instance Lude.FromJSON DvbNitSettings where
  parseJSON =
    Lude.withObject
      "DvbNitSettings"
      ( \x ->
          DvbNitSettings'
            Lude.<$> (x Lude..:? "networkId")
            Lude.<*> (x Lude..:? "networkName")
            Lude.<*> (x Lude..:? "nitInterval")
      )

instance Lude.ToJSON DvbNitSettings where
  toJSON DvbNitSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("networkId" Lude..=) Lude.<$> networkId,
            ("networkName" Lude..=) Lude.<$> networkName,
            ("nitInterval" Lude..=) Lude.<$> nitInterval
          ]
      )
