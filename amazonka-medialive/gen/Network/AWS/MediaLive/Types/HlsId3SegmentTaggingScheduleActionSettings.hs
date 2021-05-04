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
-- Module      : Network.AWS.MediaLive.Types.HlsId3SegmentTaggingScheduleActionSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsId3SegmentTaggingScheduleActionSettings where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Settings for the action to insert a user-defined ID3 tag in each HLS
-- segment
--
-- /See:/ 'newHlsId3SegmentTaggingScheduleActionSettings' smart constructor.
data HlsId3SegmentTaggingScheduleActionSettings = HlsId3SegmentTaggingScheduleActionSettings'
  { -- | ID3 tag to insert into each segment. Supports special keyword
    -- identifiers to substitute in segment-related values.\\nSupported keyword
    -- identifiers:
    -- https:\/\/docs.aws.amazon.com\/medialive\/latest\/ug\/variable-data-identifiers.html
    tag :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'HlsId3SegmentTaggingScheduleActionSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tag', 'hlsId3SegmentTaggingScheduleActionSettings_tag' - ID3 tag to insert into each segment. Supports special keyword
-- identifiers to substitute in segment-related values.\\nSupported keyword
-- identifiers:
-- https:\/\/docs.aws.amazon.com\/medialive\/latest\/ug\/variable-data-identifiers.html
newHlsId3SegmentTaggingScheduleActionSettings ::
  -- | 'tag'
  Prelude.Text ->
  HlsId3SegmentTaggingScheduleActionSettings
newHlsId3SegmentTaggingScheduleActionSettings pTag_ =
  HlsId3SegmentTaggingScheduleActionSettings'
    { tag =
        pTag_
    }

-- | ID3 tag to insert into each segment. Supports special keyword
-- identifiers to substitute in segment-related values.\\nSupported keyword
-- identifiers:
-- https:\/\/docs.aws.amazon.com\/medialive\/latest\/ug\/variable-data-identifiers.html
hlsId3SegmentTaggingScheduleActionSettings_tag :: Lens.Lens' HlsId3SegmentTaggingScheduleActionSettings Prelude.Text
hlsId3SegmentTaggingScheduleActionSettings_tag = Lens.lens (\HlsId3SegmentTaggingScheduleActionSettings' {tag} -> tag) (\s@HlsId3SegmentTaggingScheduleActionSettings' {} a -> s {tag = a} :: HlsId3SegmentTaggingScheduleActionSettings)

instance
  Prelude.FromJSON
    HlsId3SegmentTaggingScheduleActionSettings
  where
  parseJSON =
    Prelude.withObject
      "HlsId3SegmentTaggingScheduleActionSettings"
      ( \x ->
          HlsId3SegmentTaggingScheduleActionSettings'
            Prelude.<$> (x Prelude..: "tag")
      )

instance
  Prelude.Hashable
    HlsId3SegmentTaggingScheduleActionSettings

instance
  Prelude.NFData
    HlsId3SegmentTaggingScheduleActionSettings

instance
  Prelude.ToJSON
    HlsId3SegmentTaggingScheduleActionSettings
  where
  toJSON
    HlsId3SegmentTaggingScheduleActionSettings' {..} =
      Prelude.object
        ( Prelude.catMaybes
            [Prelude.Just ("tag" Prelude..= tag)]
        )
