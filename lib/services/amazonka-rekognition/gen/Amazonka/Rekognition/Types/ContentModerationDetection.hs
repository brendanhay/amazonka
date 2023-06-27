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
-- Module      : Amazonka.Rekognition.Types.ContentModerationDetection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.ContentModerationDetection where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.ModerationLabel

-- | Information about an inappropriate, unwanted, or offensive content label
-- detection in a stored video.
--
-- /See:/ 'newContentModerationDetection' smart constructor.
data ContentModerationDetection = ContentModerationDetection'
  { -- | The time duration of a segment in milliseconds, I.e. time elapsed from
    -- StartTimestampMillis to EndTimestampMillis.
    durationMillis :: Prelude.Maybe Prelude.Natural,
    -- | The time in milliseconds defining the end of the timeline segment
    -- containing a continuously detected moderation label.
    endTimestampMillis :: Prelude.Maybe Prelude.Natural,
    -- | The content moderation label detected by in the stored video.
    moderationLabel :: Prelude.Maybe ModerationLabel,
    -- | The time in milliseconds defining the start of the timeline segment
    -- containing a continuously detected moderation label.
    startTimestampMillis :: Prelude.Maybe Prelude.Natural,
    -- | Time, in milliseconds from the beginning of the video, that the content
    -- moderation label was detected. Note that @Timestamp@ is not guaranteed
    -- to be accurate to the individual frame where the moderated content first
    -- appears.
    timestamp :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContentModerationDetection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'durationMillis', 'contentModerationDetection_durationMillis' - The time duration of a segment in milliseconds, I.e. time elapsed from
-- StartTimestampMillis to EndTimestampMillis.
--
-- 'endTimestampMillis', 'contentModerationDetection_endTimestampMillis' - The time in milliseconds defining the end of the timeline segment
-- containing a continuously detected moderation label.
--
-- 'moderationLabel', 'contentModerationDetection_moderationLabel' - The content moderation label detected by in the stored video.
--
-- 'startTimestampMillis', 'contentModerationDetection_startTimestampMillis' - The time in milliseconds defining the start of the timeline segment
-- containing a continuously detected moderation label.
--
-- 'timestamp', 'contentModerationDetection_timestamp' - Time, in milliseconds from the beginning of the video, that the content
-- moderation label was detected. Note that @Timestamp@ is not guaranteed
-- to be accurate to the individual frame where the moderated content first
-- appears.
newContentModerationDetection ::
  ContentModerationDetection
newContentModerationDetection =
  ContentModerationDetection'
    { durationMillis =
        Prelude.Nothing,
      endTimestampMillis = Prelude.Nothing,
      moderationLabel = Prelude.Nothing,
      startTimestampMillis = Prelude.Nothing,
      timestamp = Prelude.Nothing
    }

-- | The time duration of a segment in milliseconds, I.e. time elapsed from
-- StartTimestampMillis to EndTimestampMillis.
contentModerationDetection_durationMillis :: Lens.Lens' ContentModerationDetection (Prelude.Maybe Prelude.Natural)
contentModerationDetection_durationMillis = Lens.lens (\ContentModerationDetection' {durationMillis} -> durationMillis) (\s@ContentModerationDetection' {} a -> s {durationMillis = a} :: ContentModerationDetection)

-- | The time in milliseconds defining the end of the timeline segment
-- containing a continuously detected moderation label.
contentModerationDetection_endTimestampMillis :: Lens.Lens' ContentModerationDetection (Prelude.Maybe Prelude.Natural)
contentModerationDetection_endTimestampMillis = Lens.lens (\ContentModerationDetection' {endTimestampMillis} -> endTimestampMillis) (\s@ContentModerationDetection' {} a -> s {endTimestampMillis = a} :: ContentModerationDetection)

-- | The content moderation label detected by in the stored video.
contentModerationDetection_moderationLabel :: Lens.Lens' ContentModerationDetection (Prelude.Maybe ModerationLabel)
contentModerationDetection_moderationLabel = Lens.lens (\ContentModerationDetection' {moderationLabel} -> moderationLabel) (\s@ContentModerationDetection' {} a -> s {moderationLabel = a} :: ContentModerationDetection)

-- | The time in milliseconds defining the start of the timeline segment
-- containing a continuously detected moderation label.
contentModerationDetection_startTimestampMillis :: Lens.Lens' ContentModerationDetection (Prelude.Maybe Prelude.Natural)
contentModerationDetection_startTimestampMillis = Lens.lens (\ContentModerationDetection' {startTimestampMillis} -> startTimestampMillis) (\s@ContentModerationDetection' {} a -> s {startTimestampMillis = a} :: ContentModerationDetection)

-- | Time, in milliseconds from the beginning of the video, that the content
-- moderation label was detected. Note that @Timestamp@ is not guaranteed
-- to be accurate to the individual frame where the moderated content first
-- appears.
contentModerationDetection_timestamp :: Lens.Lens' ContentModerationDetection (Prelude.Maybe Prelude.Integer)
contentModerationDetection_timestamp = Lens.lens (\ContentModerationDetection' {timestamp} -> timestamp) (\s@ContentModerationDetection' {} a -> s {timestamp = a} :: ContentModerationDetection)

instance Data.FromJSON ContentModerationDetection where
  parseJSON =
    Data.withObject
      "ContentModerationDetection"
      ( \x ->
          ContentModerationDetection'
            Prelude.<$> (x Data..:? "DurationMillis")
            Prelude.<*> (x Data..:? "EndTimestampMillis")
            Prelude.<*> (x Data..:? "ModerationLabel")
            Prelude.<*> (x Data..:? "StartTimestampMillis")
            Prelude.<*> (x Data..:? "Timestamp")
      )

instance Prelude.Hashable ContentModerationDetection where
  hashWithSalt _salt ContentModerationDetection' {..} =
    _salt
      `Prelude.hashWithSalt` durationMillis
      `Prelude.hashWithSalt` endTimestampMillis
      `Prelude.hashWithSalt` moderationLabel
      `Prelude.hashWithSalt` startTimestampMillis
      `Prelude.hashWithSalt` timestamp

instance Prelude.NFData ContentModerationDetection where
  rnf ContentModerationDetection' {..} =
    Prelude.rnf durationMillis
      `Prelude.seq` Prelude.rnf endTimestampMillis
      `Prelude.seq` Prelude.rnf moderationLabel
      `Prelude.seq` Prelude.rnf startTimestampMillis
      `Prelude.seq` Prelude.rnf timestamp
