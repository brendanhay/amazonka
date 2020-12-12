{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.BlackoutSlate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.BlackoutSlate
  ( BlackoutSlate (..),

    -- * Smart constructor
    mkBlackoutSlate,

    -- * Lenses
    bsNetworkEndBlackoutImage,
    bsState,
    bsNetworkEndBlackout,
    bsNetworkId,
    bsBlackoutSlateImage,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.BlackoutSlateNetworkEndBlackout
import Network.AWS.MediaLive.Types.BlackoutSlateState
import Network.AWS.MediaLive.Types.InputLocation
import qualified Network.AWS.Prelude as Lude

-- | Blackout Slate
--
-- /See:/ 'mkBlackoutSlate' smart constructor.
data BlackoutSlate = BlackoutSlate'
  { networkEndBlackoutImage ::
      Lude.Maybe InputLocation,
    state :: Lude.Maybe BlackoutSlateState,
    networkEndBlackout ::
      Lude.Maybe BlackoutSlateNetworkEndBlackout,
    networkId :: Lude.Maybe Lude.Text,
    blackoutSlateImage :: Lude.Maybe InputLocation
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BlackoutSlate' with the minimum fields required to make a request.
--
-- * 'blackoutSlateImage' - Blackout slate image to be used. Leave empty for solid black. Only bmp and png images are supported.
-- * 'networkEndBlackout' - Setting to enabled causes the encoder to blackout the video, audio, and captions, and raise the "Network Blackout Image" slate when an SCTE104/35 Network End Segmentation Descriptor is encountered. The blackout will be lifted when the Network Start Segmentation Descriptor is encountered. The Network End and Network Start descriptors must contain a network ID that matches the value entered in "Network ID".
-- * 'networkEndBlackoutImage' - Path to local file to use as Network End Blackout image. Image will be scaled to fill the entire output raster.
-- * 'networkId' - Provides Network ID that matches EIDR ID format (e.g., "10.XXXX/XXXX-XXXX-XXXX-XXXX-XXXX-C").
-- * 'state' - When set to enabled, causes video, audio and captions to be blanked when indicated by program metadata.
mkBlackoutSlate ::
  BlackoutSlate
mkBlackoutSlate =
  BlackoutSlate'
    { networkEndBlackoutImage = Lude.Nothing,
      state = Lude.Nothing,
      networkEndBlackout = Lude.Nothing,
      networkId = Lude.Nothing,
      blackoutSlateImage = Lude.Nothing
    }

-- | Path to local file to use as Network End Blackout image. Image will be scaled to fill the entire output raster.
--
-- /Note:/ Consider using 'networkEndBlackoutImage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsNetworkEndBlackoutImage :: Lens.Lens' BlackoutSlate (Lude.Maybe InputLocation)
bsNetworkEndBlackoutImage = Lens.lens (networkEndBlackoutImage :: BlackoutSlate -> Lude.Maybe InputLocation) (\s a -> s {networkEndBlackoutImage = a} :: BlackoutSlate)
{-# DEPRECATED bsNetworkEndBlackoutImage "Use generic-lens or generic-optics with 'networkEndBlackoutImage' instead." #-}

-- | When set to enabled, causes video, audio and captions to be blanked when indicated by program metadata.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsState :: Lens.Lens' BlackoutSlate (Lude.Maybe BlackoutSlateState)
bsState = Lens.lens (state :: BlackoutSlate -> Lude.Maybe BlackoutSlateState) (\s a -> s {state = a} :: BlackoutSlate)
{-# DEPRECATED bsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | Setting to enabled causes the encoder to blackout the video, audio, and captions, and raise the "Network Blackout Image" slate when an SCTE104/35 Network End Segmentation Descriptor is encountered. The blackout will be lifted when the Network Start Segmentation Descriptor is encountered. The Network End and Network Start descriptors must contain a network ID that matches the value entered in "Network ID".
--
-- /Note:/ Consider using 'networkEndBlackout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsNetworkEndBlackout :: Lens.Lens' BlackoutSlate (Lude.Maybe BlackoutSlateNetworkEndBlackout)
bsNetworkEndBlackout = Lens.lens (networkEndBlackout :: BlackoutSlate -> Lude.Maybe BlackoutSlateNetworkEndBlackout) (\s a -> s {networkEndBlackout = a} :: BlackoutSlate)
{-# DEPRECATED bsNetworkEndBlackout "Use generic-lens or generic-optics with 'networkEndBlackout' instead." #-}

-- | Provides Network ID that matches EIDR ID format (e.g., "10.XXXX/XXXX-XXXX-XXXX-XXXX-XXXX-C").
--
-- /Note:/ Consider using 'networkId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsNetworkId :: Lens.Lens' BlackoutSlate (Lude.Maybe Lude.Text)
bsNetworkId = Lens.lens (networkId :: BlackoutSlate -> Lude.Maybe Lude.Text) (\s a -> s {networkId = a} :: BlackoutSlate)
{-# DEPRECATED bsNetworkId "Use generic-lens or generic-optics with 'networkId' instead." #-}

-- | Blackout slate image to be used. Leave empty for solid black. Only bmp and png images are supported.
--
-- /Note:/ Consider using 'blackoutSlateImage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsBlackoutSlateImage :: Lens.Lens' BlackoutSlate (Lude.Maybe InputLocation)
bsBlackoutSlateImage = Lens.lens (blackoutSlateImage :: BlackoutSlate -> Lude.Maybe InputLocation) (\s a -> s {blackoutSlateImage = a} :: BlackoutSlate)
{-# DEPRECATED bsBlackoutSlateImage "Use generic-lens or generic-optics with 'blackoutSlateImage' instead." #-}

instance Lude.FromJSON BlackoutSlate where
  parseJSON =
    Lude.withObject
      "BlackoutSlate"
      ( \x ->
          BlackoutSlate'
            Lude.<$> (x Lude..:? "networkEndBlackoutImage")
            Lude.<*> (x Lude..:? "state")
            Lude.<*> (x Lude..:? "networkEndBlackout")
            Lude.<*> (x Lude..:? "networkId")
            Lude.<*> (x Lude..:? "blackoutSlateImage")
      )

instance Lude.ToJSON BlackoutSlate where
  toJSON BlackoutSlate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("networkEndBlackoutImage" Lude..=)
              Lude.<$> networkEndBlackoutImage,
            ("state" Lude..=) Lude.<$> state,
            ("networkEndBlackout" Lude..=) Lude.<$> networkEndBlackout,
            ("networkId" Lude..=) Lude.<$> networkId,
            ("blackoutSlateImage" Lude..=) Lude.<$> blackoutSlateImage
          ]
      )
