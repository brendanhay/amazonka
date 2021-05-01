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
-- Module      : Network.AWS.MediaConvert.Types.TrackSourceSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.TrackSourceSettings where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Settings specific to caption sources that are specified by track number.
-- Currently, this is only IMSC captions in an IMF package. If your caption
-- source is IMSC 1.1 in a separate xml file, use FileSourceSettings
-- instead of TrackSourceSettings.
--
-- /See:/ 'newTrackSourceSettings' smart constructor.
data TrackSourceSettings = TrackSourceSettings'
  { -- | Use this setting to select a single captions track from a source. Track
    -- numbers correspond to the order in the captions source file. For IMF
    -- sources, track numbering is based on the order that the captions appear
    -- in the CPL. For example, use 1 to select the captions asset that is
    -- listed first in the CPL. To include more than one captions track in your
    -- job outputs, create multiple input captions selectors. Specify one track
    -- per selector.
    trackNumber :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TrackSourceSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trackNumber', 'trackSourceSettings_trackNumber' - Use this setting to select a single captions track from a source. Track
-- numbers correspond to the order in the captions source file. For IMF
-- sources, track numbering is based on the order that the captions appear
-- in the CPL. For example, use 1 to select the captions asset that is
-- listed first in the CPL. To include more than one captions track in your
-- job outputs, create multiple input captions selectors. Specify one track
-- per selector.
newTrackSourceSettings ::
  TrackSourceSettings
newTrackSourceSettings =
  TrackSourceSettings' {trackNumber = Prelude.Nothing}

-- | Use this setting to select a single captions track from a source. Track
-- numbers correspond to the order in the captions source file. For IMF
-- sources, track numbering is based on the order that the captions appear
-- in the CPL. For example, use 1 to select the captions asset that is
-- listed first in the CPL. To include more than one captions track in your
-- job outputs, create multiple input captions selectors. Specify one track
-- per selector.
trackSourceSettings_trackNumber :: Lens.Lens' TrackSourceSettings (Prelude.Maybe Prelude.Natural)
trackSourceSettings_trackNumber = Lens.lens (\TrackSourceSettings' {trackNumber} -> trackNumber) (\s@TrackSourceSettings' {} a -> s {trackNumber = a} :: TrackSourceSettings)

instance Prelude.FromJSON TrackSourceSettings where
  parseJSON =
    Prelude.withObject
      "TrackSourceSettings"
      ( \x ->
          TrackSourceSettings'
            Prelude.<$> (x Prelude..:? "trackNumber")
      )

instance Prelude.Hashable TrackSourceSettings

instance Prelude.NFData TrackSourceSettings

instance Prelude.ToJSON TrackSourceSettings where
  toJSON TrackSourceSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("trackNumber" Prelude..=) Prelude.<$> trackNumber]
      )
