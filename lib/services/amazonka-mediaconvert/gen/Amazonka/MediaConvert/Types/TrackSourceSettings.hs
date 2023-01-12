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
-- Module      : Amazonka.MediaConvert.Types.TrackSourceSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.TrackSourceSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON TrackSourceSettings where
  parseJSON =
    Data.withObject
      "TrackSourceSettings"
      ( \x ->
          TrackSourceSettings'
            Prelude.<$> (x Data..:? "trackNumber")
      )

instance Prelude.Hashable TrackSourceSettings where
  hashWithSalt _salt TrackSourceSettings' {..} =
    _salt `Prelude.hashWithSalt` trackNumber

instance Prelude.NFData TrackSourceSettings where
  rnf TrackSourceSettings' {..} =
    Prelude.rnf trackNumber

instance Data.ToJSON TrackSourceSettings where
  toJSON TrackSourceSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [("trackNumber" Data..=) Prelude.<$> trackNumber]
      )
