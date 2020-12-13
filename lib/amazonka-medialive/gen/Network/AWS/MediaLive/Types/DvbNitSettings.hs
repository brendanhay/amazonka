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
    dnsNetworkId,
    dnsNetworkName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | DVB Network Information Table (NIT)
--
-- /See:/ 'mkDvbNitSettings' smart constructor.
data DvbNitSettings = DvbNitSettings'
  { -- | The number of milliseconds between instances of this table in the output transport stream.
    repInterval :: Lude.Maybe Lude.Natural,
    -- | The numeric value placed in the Network Information Table (NIT).
    networkId :: Lude.Natural,
    -- | The network name text placed in the networkNameDescriptor inside the Network Information Table. Maximum length is 256 characters.
    networkName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DvbNitSettings' with the minimum fields required to make a request.
--
-- * 'repInterval' - The number of milliseconds between instances of this table in the output transport stream.
-- * 'networkId' - The numeric value placed in the Network Information Table (NIT).
-- * 'networkName' - The network name text placed in the networkNameDescriptor inside the Network Information Table. Maximum length is 256 characters.
mkDvbNitSettings ::
  -- | 'networkId'
  Lude.Natural ->
  -- | 'networkName'
  Lude.Text ->
  DvbNitSettings
mkDvbNitSettings pNetworkId_ pNetworkName_ =
  DvbNitSettings'
    { repInterval = Lude.Nothing,
      networkId = pNetworkId_,
      networkName = pNetworkName_
    }

-- | The number of milliseconds between instances of this table in the output transport stream.
--
-- /Note:/ Consider using 'repInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnsRepInterval :: Lens.Lens' DvbNitSettings (Lude.Maybe Lude.Natural)
dnsRepInterval = Lens.lens (repInterval :: DvbNitSettings -> Lude.Maybe Lude.Natural) (\s a -> s {repInterval = a} :: DvbNitSettings)
{-# DEPRECATED dnsRepInterval "Use generic-lens or generic-optics with 'repInterval' instead." #-}

-- | The numeric value placed in the Network Information Table (NIT).
--
-- /Note:/ Consider using 'networkId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnsNetworkId :: Lens.Lens' DvbNitSettings Lude.Natural
dnsNetworkId = Lens.lens (networkId :: DvbNitSettings -> Lude.Natural) (\s a -> s {networkId = a} :: DvbNitSettings)
{-# DEPRECATED dnsNetworkId "Use generic-lens or generic-optics with 'networkId' instead." #-}

-- | The network name text placed in the networkNameDescriptor inside the Network Information Table. Maximum length is 256 characters.
--
-- /Note:/ Consider using 'networkName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnsNetworkName :: Lens.Lens' DvbNitSettings Lude.Text
dnsNetworkName = Lens.lens (networkName :: DvbNitSettings -> Lude.Text) (\s a -> s {networkName = a} :: DvbNitSettings)
{-# DEPRECATED dnsNetworkName "Use generic-lens or generic-optics with 'networkName' instead." #-}

instance Lude.FromJSON DvbNitSettings where
  parseJSON =
    Lude.withObject
      "DvbNitSettings"
      ( \x ->
          DvbNitSettings'
            Lude.<$> (x Lude..:? "repInterval")
            Lude.<*> (x Lude..: "networkId")
            Lude.<*> (x Lude..: "networkName")
      )

instance Lude.ToJSON DvbNitSettings where
  toJSON DvbNitSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("repInterval" Lude..=) Lude.<$> repInterval,
            Lude.Just ("networkId" Lude..= networkId),
            Lude.Just ("networkName" Lude..= networkName)
          ]
      )
