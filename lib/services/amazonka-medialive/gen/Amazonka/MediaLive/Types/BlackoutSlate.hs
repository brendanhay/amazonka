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
-- Module      : Amazonka.MediaLive.Types.BlackoutSlate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.BlackoutSlate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.BlackoutSlateNetworkEndBlackout
import Amazonka.MediaLive.Types.BlackoutSlateState
import Amazonka.MediaLive.Types.InputLocation
import qualified Amazonka.Prelude as Prelude

-- | Blackout Slate
--
-- /See:/ 'newBlackoutSlate' smart constructor.
data BlackoutSlate = BlackoutSlate'
  { -- | Blackout slate image to be used. Leave empty for solid black. Only bmp
    -- and png images are supported.
    blackoutSlateImage :: Prelude.Maybe InputLocation,
    -- | When set to enabled, causes video, audio and captions to be blanked when
    -- indicated by program metadata.
    state :: Prelude.Maybe BlackoutSlateState,
    -- | Path to local file to use as Network End Blackout image. Image will be
    -- scaled to fill the entire output raster.
    networkEndBlackoutImage :: Prelude.Maybe InputLocation,
    -- | Setting to enabled causes the encoder to blackout the video, audio, and
    -- captions, and raise the \"Network Blackout Image\" slate when an
    -- SCTE104\/35 Network End Segmentation Descriptor is encountered. The
    -- blackout will be lifted when the Network Start Segmentation Descriptor
    -- is encountered. The Network End and Network Start descriptors must
    -- contain a network ID that matches the value entered in \"Network ID\".
    networkEndBlackout :: Prelude.Maybe BlackoutSlateNetworkEndBlackout,
    -- | Provides Network ID that matches EIDR ID format (e.g.,
    -- \"10.XXXX\/XXXX-XXXX-XXXX-XXXX-XXXX-C\").
    networkId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'state', 'blackoutSlate_state' - When set to enabled, causes video, audio and captions to be blanked when
-- indicated by program metadata.
--
-- 'networkEndBlackoutImage', 'blackoutSlate_networkEndBlackoutImage' - Path to local file to use as Network End Blackout image. Image will be
-- scaled to fill the entire output raster.
--
-- 'networkEndBlackout', 'blackoutSlate_networkEndBlackout' - Setting to enabled causes the encoder to blackout the video, audio, and
-- captions, and raise the \"Network Blackout Image\" slate when an
-- SCTE104\/35 Network End Segmentation Descriptor is encountered. The
-- blackout will be lifted when the Network Start Segmentation Descriptor
-- is encountered. The Network End and Network Start descriptors must
-- contain a network ID that matches the value entered in \"Network ID\".
--
-- 'networkId', 'blackoutSlate_networkId' - Provides Network ID that matches EIDR ID format (e.g.,
-- \"10.XXXX\/XXXX-XXXX-XXXX-XXXX-XXXX-C\").
newBlackoutSlate ::
  BlackoutSlate
newBlackoutSlate =
  BlackoutSlate'
    { blackoutSlateImage =
        Prelude.Nothing,
      state = Prelude.Nothing,
      networkEndBlackoutImage = Prelude.Nothing,
      networkEndBlackout = Prelude.Nothing,
      networkId = Prelude.Nothing
    }

-- | Blackout slate image to be used. Leave empty for solid black. Only bmp
-- and png images are supported.
blackoutSlate_blackoutSlateImage :: Lens.Lens' BlackoutSlate (Prelude.Maybe InputLocation)
blackoutSlate_blackoutSlateImage = Lens.lens (\BlackoutSlate' {blackoutSlateImage} -> blackoutSlateImage) (\s@BlackoutSlate' {} a -> s {blackoutSlateImage = a} :: BlackoutSlate)

-- | When set to enabled, causes video, audio and captions to be blanked when
-- indicated by program metadata.
blackoutSlate_state :: Lens.Lens' BlackoutSlate (Prelude.Maybe BlackoutSlateState)
blackoutSlate_state = Lens.lens (\BlackoutSlate' {state} -> state) (\s@BlackoutSlate' {} a -> s {state = a} :: BlackoutSlate)

-- | Path to local file to use as Network End Blackout image. Image will be
-- scaled to fill the entire output raster.
blackoutSlate_networkEndBlackoutImage :: Lens.Lens' BlackoutSlate (Prelude.Maybe InputLocation)
blackoutSlate_networkEndBlackoutImage = Lens.lens (\BlackoutSlate' {networkEndBlackoutImage} -> networkEndBlackoutImage) (\s@BlackoutSlate' {} a -> s {networkEndBlackoutImage = a} :: BlackoutSlate)

-- | Setting to enabled causes the encoder to blackout the video, audio, and
-- captions, and raise the \"Network Blackout Image\" slate when an
-- SCTE104\/35 Network End Segmentation Descriptor is encountered. The
-- blackout will be lifted when the Network Start Segmentation Descriptor
-- is encountered. The Network End and Network Start descriptors must
-- contain a network ID that matches the value entered in \"Network ID\".
blackoutSlate_networkEndBlackout :: Lens.Lens' BlackoutSlate (Prelude.Maybe BlackoutSlateNetworkEndBlackout)
blackoutSlate_networkEndBlackout = Lens.lens (\BlackoutSlate' {networkEndBlackout} -> networkEndBlackout) (\s@BlackoutSlate' {} a -> s {networkEndBlackout = a} :: BlackoutSlate)

-- | Provides Network ID that matches EIDR ID format (e.g.,
-- \"10.XXXX\/XXXX-XXXX-XXXX-XXXX-XXXX-C\").
blackoutSlate_networkId :: Lens.Lens' BlackoutSlate (Prelude.Maybe Prelude.Text)
blackoutSlate_networkId = Lens.lens (\BlackoutSlate' {networkId} -> networkId) (\s@BlackoutSlate' {} a -> s {networkId = a} :: BlackoutSlate)

instance Data.FromJSON BlackoutSlate where
  parseJSON =
    Data.withObject
      "BlackoutSlate"
      ( \x ->
          BlackoutSlate'
            Prelude.<$> (x Data..:? "blackoutSlateImage")
            Prelude.<*> (x Data..:? "state")
            Prelude.<*> (x Data..:? "networkEndBlackoutImage")
            Prelude.<*> (x Data..:? "networkEndBlackout")
            Prelude.<*> (x Data..:? "networkId")
      )

instance Prelude.Hashable BlackoutSlate where
  hashWithSalt _salt BlackoutSlate' {..} =
    _salt `Prelude.hashWithSalt` blackoutSlateImage
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` networkEndBlackoutImage
      `Prelude.hashWithSalt` networkEndBlackout
      `Prelude.hashWithSalt` networkId

instance Prelude.NFData BlackoutSlate where
  rnf BlackoutSlate' {..} =
    Prelude.rnf blackoutSlateImage
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf networkEndBlackoutImage
      `Prelude.seq` Prelude.rnf networkEndBlackout
      `Prelude.seq` Prelude.rnf networkId

instance Data.ToJSON BlackoutSlate where
  toJSON BlackoutSlate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("blackoutSlateImage" Data..=)
              Prelude.<$> blackoutSlateImage,
            ("state" Data..=) Prelude.<$> state,
            ("networkEndBlackoutImage" Data..=)
              Prelude.<$> networkEndBlackoutImage,
            ("networkEndBlackout" Data..=)
              Prelude.<$> networkEndBlackout,
            ("networkId" Data..=) Prelude.<$> networkId
          ]
      )
