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
-- Module      : Network.AWS.MediaLive.Types.HlsTimedMetadataScheduleActionSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsTimedMetadataScheduleActionSettings where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Settings for the action to emit HLS metadata
--
-- /See:/ 'newHlsTimedMetadataScheduleActionSettings' smart constructor.
data HlsTimedMetadataScheduleActionSettings = HlsTimedMetadataScheduleActionSettings'
  { -- | Base64 string formatted according to the ID3 specification:
    -- http:\/\/id3.org\/id3v2.4.0-structure
    id3 :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.FromJSON
    HlsTimedMetadataScheduleActionSettings
  where
  parseJSON =
    Prelude.withObject
      "HlsTimedMetadataScheduleActionSettings"
      ( \x ->
          HlsTimedMetadataScheduleActionSettings'
            Prelude.<$> (x Prelude..: "id3")
      )

instance
  Prelude.Hashable
    HlsTimedMetadataScheduleActionSettings

instance
  Prelude.NFData
    HlsTimedMetadataScheduleActionSettings

instance
  Prelude.ToJSON
    HlsTimedMetadataScheduleActionSettings
  where
  toJSON HlsTimedMetadataScheduleActionSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("id3" Prelude..= id3)]
      )
