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
-- Module      : Amazonka.Rekognition.Types.LabelDetection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.LabelDetection where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.Label

-- | Information about a label detected in a video analysis request and the
-- time the label was detected in the video.
--
-- /See:/ 'newLabelDetection' smart constructor.
data LabelDetection = LabelDetection'
  { -- | The time duration of a segment in milliseconds, I.e. time elapsed from
    -- StartTimestampMillis to EndTimestampMillis.
    durationMillis :: Prelude.Maybe Prelude.Natural,
    -- | The time in milliseconds defining the end of the timeline segment
    -- containing a continuously detected label.
    endTimestampMillis :: Prelude.Maybe Prelude.Natural,
    -- | Details about the detected label.
    label :: Prelude.Maybe Label,
    -- | The time in milliseconds defining the start of the timeline segment
    -- containing a continuously detected label.
    startTimestampMillis :: Prelude.Maybe Prelude.Natural,
    -- | Time, in milliseconds from the start of the video, that the label was
    -- detected. Note that @Timestamp@ is not guaranteed to be accurate to the
    -- individual frame where the label first appears.
    timestamp :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LabelDetection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'durationMillis', 'labelDetection_durationMillis' - The time duration of a segment in milliseconds, I.e. time elapsed from
-- StartTimestampMillis to EndTimestampMillis.
--
-- 'endTimestampMillis', 'labelDetection_endTimestampMillis' - The time in milliseconds defining the end of the timeline segment
-- containing a continuously detected label.
--
-- 'label', 'labelDetection_label' - Details about the detected label.
--
-- 'startTimestampMillis', 'labelDetection_startTimestampMillis' - The time in milliseconds defining the start of the timeline segment
-- containing a continuously detected label.
--
-- 'timestamp', 'labelDetection_timestamp' - Time, in milliseconds from the start of the video, that the label was
-- detected. Note that @Timestamp@ is not guaranteed to be accurate to the
-- individual frame where the label first appears.
newLabelDetection ::
  LabelDetection
newLabelDetection =
  LabelDetection'
    { durationMillis = Prelude.Nothing,
      endTimestampMillis = Prelude.Nothing,
      label = Prelude.Nothing,
      startTimestampMillis = Prelude.Nothing,
      timestamp = Prelude.Nothing
    }

-- | The time duration of a segment in milliseconds, I.e. time elapsed from
-- StartTimestampMillis to EndTimestampMillis.
labelDetection_durationMillis :: Lens.Lens' LabelDetection (Prelude.Maybe Prelude.Natural)
labelDetection_durationMillis = Lens.lens (\LabelDetection' {durationMillis} -> durationMillis) (\s@LabelDetection' {} a -> s {durationMillis = a} :: LabelDetection)

-- | The time in milliseconds defining the end of the timeline segment
-- containing a continuously detected label.
labelDetection_endTimestampMillis :: Lens.Lens' LabelDetection (Prelude.Maybe Prelude.Natural)
labelDetection_endTimestampMillis = Lens.lens (\LabelDetection' {endTimestampMillis} -> endTimestampMillis) (\s@LabelDetection' {} a -> s {endTimestampMillis = a} :: LabelDetection)

-- | Details about the detected label.
labelDetection_label :: Lens.Lens' LabelDetection (Prelude.Maybe Label)
labelDetection_label = Lens.lens (\LabelDetection' {label} -> label) (\s@LabelDetection' {} a -> s {label = a} :: LabelDetection)

-- | The time in milliseconds defining the start of the timeline segment
-- containing a continuously detected label.
labelDetection_startTimestampMillis :: Lens.Lens' LabelDetection (Prelude.Maybe Prelude.Natural)
labelDetection_startTimestampMillis = Lens.lens (\LabelDetection' {startTimestampMillis} -> startTimestampMillis) (\s@LabelDetection' {} a -> s {startTimestampMillis = a} :: LabelDetection)

-- | Time, in milliseconds from the start of the video, that the label was
-- detected. Note that @Timestamp@ is not guaranteed to be accurate to the
-- individual frame where the label first appears.
labelDetection_timestamp :: Lens.Lens' LabelDetection (Prelude.Maybe Prelude.Integer)
labelDetection_timestamp = Lens.lens (\LabelDetection' {timestamp} -> timestamp) (\s@LabelDetection' {} a -> s {timestamp = a} :: LabelDetection)

instance Data.FromJSON LabelDetection where
  parseJSON =
    Data.withObject
      "LabelDetection"
      ( \x ->
          LabelDetection'
            Prelude.<$> (x Data..:? "DurationMillis")
            Prelude.<*> (x Data..:? "EndTimestampMillis")
            Prelude.<*> (x Data..:? "Label")
            Prelude.<*> (x Data..:? "StartTimestampMillis")
            Prelude.<*> (x Data..:? "Timestamp")
      )

instance Prelude.Hashable LabelDetection where
  hashWithSalt _salt LabelDetection' {..} =
    _salt `Prelude.hashWithSalt` durationMillis
      `Prelude.hashWithSalt` endTimestampMillis
      `Prelude.hashWithSalt` label
      `Prelude.hashWithSalt` startTimestampMillis
      `Prelude.hashWithSalt` timestamp

instance Prelude.NFData LabelDetection where
  rnf LabelDetection' {..} =
    Prelude.rnf durationMillis
      `Prelude.seq` Prelude.rnf endTimestampMillis
      `Prelude.seq` Prelude.rnf label
      `Prelude.seq` Prelude.rnf startTimestampMillis
      `Prelude.seq` Prelude.rnf timestamp
