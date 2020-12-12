{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.DvbNitSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.DvbNitSettings
  ( DvbNitSettings (..),

    -- * Smart constructor
    mkDvbNitSettings,

    -- * Lenses
    dnsRepInterval,
    dnsNetworkName,
    dnsNetworkId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | DVB Network Information Table (NIT)
--
-- /See:/ 'mkDvbNitSettings' smart constructor.
data DvbNitSettings = DvbNitSettings'
  { repInterval ::
      Lude.Maybe Lude.Natural,
    networkName :: Lude.Text,
    networkId :: Lude.Natural
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
-- * 'networkName' - The network name text placed in the networkNameDescriptor inside the Network Information Table. Maximum length is 256 characters.
-- * 'repInterval' - The number of milliseconds between instances of this table in the output transport stream.
mkDvbNitSettings ::
  -- | 'networkName'
  Lude.Text ->
  -- | 'networkId'
  Lude.Natural ->
  DvbNitSettings
mkDvbNitSettings pNetworkName_ pNetworkId_ =
  DvbNitSettings'
    { repInterval = Lude.Nothing,
      networkName = pNetworkName_,
      networkId = pNetworkId_
    }

-- | The number of milliseconds between instances of this table in the output transport stream.
--
-- /Note:/ Consider using 'repInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnsRepInterval :: Lens.Lens' DvbNitSettings (Lude.Maybe Lude.Natural)
dnsRepInterval = Lens.lens (repInterval :: DvbNitSettings -> Lude.Maybe Lude.Natural) (\s a -> s {repInterval = a} :: DvbNitSettings)
{-# DEPRECATED dnsRepInterval "Use generic-lens or generic-optics with 'repInterval' instead." #-}

-- | The network name text placed in the networkNameDescriptor inside the Network Information Table. Maximum length is 256 characters.
--
-- /Note:/ Consider using 'networkName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnsNetworkName :: Lens.Lens' DvbNitSettings Lude.Text
dnsNetworkName = Lens.lens (networkName :: DvbNitSettings -> Lude.Text) (\s a -> s {networkName = a} :: DvbNitSettings)
{-# DEPRECATED dnsNetworkName "Use generic-lens or generic-optics with 'networkName' instead." #-}

-- | The numeric value placed in the Network Information Table (NIT).
--
-- /Note:/ Consider using 'networkId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnsNetworkId :: Lens.Lens' DvbNitSettings Lude.Natural
dnsNetworkId = Lens.lens (networkId :: DvbNitSettings -> Lude.Natural) (\s a -> s {networkId = a} :: DvbNitSettings)
{-# DEPRECATED dnsNetworkId "Use generic-lens or generic-optics with 'networkId' instead." #-}

instance Lude.FromJSON DvbNitSettings where
  parseJSON =
    Lude.withObject
      "DvbNitSettings"
      ( \x ->
          DvbNitSettings'
            Lude.<$> (x Lude..:? "repInterval")
            Lude.<*> (x Lude..: "networkName")
            Lude.<*> (x Lude..: "networkId")
      )

instance Lude.ToJSON DvbNitSettings where
  toJSON DvbNitSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("repInterval" Lude..=) Lude.<$> repInterval,
            Lude.Just ("networkName" Lude..= networkName),
            Lude.Just ("networkId" Lude..= networkId)
          ]
      )
