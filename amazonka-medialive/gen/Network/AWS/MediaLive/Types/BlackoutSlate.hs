{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.BlackoutSlate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.BlackoutSlate where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.BlackoutSlateNetworkEndBlackout
import Network.AWS.MediaLive.Types.BlackoutSlateState
import Network.AWS.MediaLive.Types.InputLocation
import qualified Network.AWS.Prelude as Prelude

-- | Blackout Slate
--
-- /See:/ 'newBlackoutSlate' smart constructor.
data BlackoutSlate = BlackoutSlate'
  { -- | Blackout slate image to be used. Leave empty for solid black. Only bmp
    -- and png images are supported.
    blackoutSlateImage :: Prelude.Maybe InputLocation,
    -- | Setting to enabled causes the encoder to blackout the video, audio, and
    -- captions, and raise the \"Network Blackout Image\" slate when an
    -- SCTE104\/35 Network End Segmentation Descriptor is encountered. The
    -- blackout will be lifted when the Network Start Segmentation Descriptor
    -- is encountered. The Network End and Network Start descriptors must
    -- contain a network ID that matches the value entered in \"Network ID\".
    networkEndBlackout :: Prelude.Maybe BlackoutSlateNetworkEndBlackout,
    -- | When set to enabled, causes video, audio and captions to be blanked when
    -- indicated by program metadata.
    state :: Prelude.Maybe BlackoutSlateState,
    -- | Path to local file to use as Network End Blackout image. Image will be
    -- scaled to fill the entire output raster.
    networkEndBlackoutImage :: Prelude.Maybe InputLocation,
    -- | Provides Network ID that matches EIDR ID format (e.g.,
    -- \"10.XXXX\/XXXX-XXXX-XXXX-XXXX-XXXX-C\").
    networkId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BlackoutSlate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blackoutSlateImage', 'blackoutSlate_blackoutSlateImage' - Blackout slate image to be used. Leave empty for solid black. Only bmp
-- and png images are supported.
--
-- 'networkEndBlackout', 'blackoutSlate_networkEndBlackout' - Setting to enabled causes the encoder to blackout the video, audio, and
-- captions, and raise the \"Network Blackout Image\" slate when an
-- SCTE104\/35 Network End Segmentation Descriptor is encountered. The
-- blackout will be lifted when the Network Start Segmentation Descriptor
-- is encountered. The Network End and Network Start descriptors must
-- contain a network ID that matches the value entered in \"Network ID\".
--
-- 'state', 'blackoutSlate_state' - When set to enabled, causes video, audio and captions to be blanked when
-- indicated by program metadata.
--
-- 'networkEndBlackoutImage', 'blackoutSlate_networkEndBlackoutImage' - Path to local file to use as Network End Blackout image. Image will be
-- scaled to fill the entire output raster.
--
-- 'networkId', 'blackoutSlate_networkId' - Provides Network ID that matches EIDR ID format (e.g.,
-- \"10.XXXX\/XXXX-XXXX-XXXX-XXXX-XXXX-C\").
newBlackoutSlate ::
  BlackoutSlate
newBlackoutSlate =
  BlackoutSlate'
    { blackoutSlateImage =
        Prelude.Nothing,
      networkEndBlackout = Prelude.Nothing,
      state = Prelude.Nothing,
      networkEndBlackoutImage = Prelude.Nothing,
      networkId = Prelude.Nothing
    }

-- | Blackout slate image to be used. Leave empty for solid black. Only bmp
-- and png images are supported.
blackoutSlate_blackoutSlateImage :: Lens.Lens' BlackoutSlate (Prelude.Maybe InputLocation)
blackoutSlate_blackoutSlateImage = Lens.lens (\BlackoutSlate' {blackoutSlateImage} -> blackoutSlateImage) (\s@BlackoutSlate' {} a -> s {blackoutSlateImage = a} :: BlackoutSlate)

-- | Setting to enabled causes the encoder to blackout the video, audio, and
-- captions, and raise the \"Network Blackout Image\" slate when an
-- SCTE104\/35 Network End Segmentation Descriptor is encountered. The
-- blackout will be lifted when the Network Start Segmentation Descriptor
-- is encountered. The Network End and Network Start descriptors must
-- contain a network ID that matches the value entered in \"Network ID\".
blackoutSlate_networkEndBlackout :: Lens.Lens' BlackoutSlate (Prelude.Maybe BlackoutSlateNetworkEndBlackout)
blackoutSlate_networkEndBlackout = Lens.lens (\BlackoutSlate' {networkEndBlackout} -> networkEndBlackout) (\s@BlackoutSlate' {} a -> s {networkEndBlackout = a} :: BlackoutSlate)

-- | When set to enabled, causes video, audio and captions to be blanked when
-- indicated by program metadata.
blackoutSlate_state :: Lens.Lens' BlackoutSlate (Prelude.Maybe BlackoutSlateState)
blackoutSlate_state = Lens.lens (\BlackoutSlate' {state} -> state) (\s@BlackoutSlate' {} a -> s {state = a} :: BlackoutSlate)

-- | Path to local file to use as Network End Blackout image. Image will be
-- scaled to fill the entire output raster.
blackoutSlate_networkEndBlackoutImage :: Lens.Lens' BlackoutSlate (Prelude.Maybe InputLocation)
blackoutSlate_networkEndBlackoutImage = Lens.lens (\BlackoutSlate' {networkEndBlackoutImage} -> networkEndBlackoutImage) (\s@BlackoutSlate' {} a -> s {networkEndBlackoutImage = a} :: BlackoutSlate)

-- | Provides Network ID that matches EIDR ID format (e.g.,
-- \"10.XXXX\/XXXX-XXXX-XXXX-XXXX-XXXX-C\").
blackoutSlate_networkId :: Lens.Lens' BlackoutSlate (Prelude.Maybe Prelude.Text)
blackoutSlate_networkId = Lens.lens (\BlackoutSlate' {networkId} -> networkId) (\s@BlackoutSlate' {} a -> s {networkId = a} :: BlackoutSlate)

instance Prelude.FromJSON BlackoutSlate where
  parseJSON =
    Prelude.withObject
      "BlackoutSlate"
      ( \x ->
          BlackoutSlate'
            Prelude.<$> (x Prelude..:? "blackoutSlateImage")
            Prelude.<*> (x Prelude..:? "networkEndBlackout")
            Prelude.<*> (x Prelude..:? "state")
            Prelude.<*> (x Prelude..:? "networkEndBlackoutImage")
            Prelude.<*> (x Prelude..:? "networkId")
      )

instance Prelude.Hashable BlackoutSlate

instance Prelude.NFData BlackoutSlate

instance Prelude.ToJSON BlackoutSlate where
  toJSON BlackoutSlate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("blackoutSlateImage" Prelude..=)
              Prelude.<$> blackoutSlateImage,
            ("networkEndBlackout" Prelude..=)
              Prelude.<$> networkEndBlackout,
            ("state" Prelude..=) Prelude.<$> state,
            ("networkEndBlackoutImage" Prelude..=)
              Prelude.<$> networkEndBlackoutImage,
            ("networkId" Prelude..=) Prelude.<$> networkId
          ]
      )
