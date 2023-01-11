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
-- Module      : Amazonka.MediaLive.Types.HlsTimedMetadataScheduleActionSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.HlsTimedMetadataScheduleActionSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Settings for the action to emit HLS metadata
--
-- /See:/ 'newHlsTimedMetadataScheduleActionSettings' smart constructor.
data HlsTimedMetadataScheduleActionSettings = HlsTimedMetadataScheduleActionSettings'
  { -- | Base64 string formatted according to the ID3 specification:
    -- http:\/\/id3.org\/id3v2.4.0-structure
    id3 :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HlsTimedMetadataScheduleActionSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id3', 'hlsTimedMetadataScheduleActionSettings_id3' - Base64 string formatted according to the ID3 specification:
-- http:\/\/id3.org\/id3v2.4.0-structure
newHlsTimedMetadataScheduleActionSettings ::
  -- | 'id3'
  Prelude.Text ->
  HlsTimedMetadataScheduleActionSettings
newHlsTimedMetadataScheduleActionSettings pId3_ =
  HlsTimedMetadataScheduleActionSettings'
    { id3 =
        pId3_
    }

-- | Base64 string formatted according to the ID3 specification:
-- http:\/\/id3.org\/id3v2.4.0-structure
hlsTimedMetadataScheduleActionSettings_id3 :: Lens.Lens' HlsTimedMetadataScheduleActionSettings Prelude.Text
hlsTimedMetadataScheduleActionSettings_id3 = Lens.lens (\HlsTimedMetadataScheduleActionSettings' {id3} -> id3) (\s@HlsTimedMetadataScheduleActionSettings' {} a -> s {id3 = a} :: HlsTimedMetadataScheduleActionSettings)

instance
  Data.FromJSON
    HlsTimedMetadataScheduleActionSettings
  where
  parseJSON =
    Data.withObject
      "HlsTimedMetadataScheduleActionSettings"
      ( \x ->
          HlsTimedMetadataScheduleActionSettings'
            Prelude.<$> (x Data..: "id3")
      )

instance
  Prelude.Hashable
    HlsTimedMetadataScheduleActionSettings
  where
  hashWithSalt
    _salt
    HlsTimedMetadataScheduleActionSettings' {..} =
      _salt `Prelude.hashWithSalt` id3

instance
  Prelude.NFData
    HlsTimedMetadataScheduleActionSettings
  where
  rnf HlsTimedMetadataScheduleActionSettings' {..} =
    Prelude.rnf id3

instance
  Data.ToJSON
    HlsTimedMetadataScheduleActionSettings
  where
  toJSON HlsTimedMetadataScheduleActionSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("id3" Data..= id3)]
      )
