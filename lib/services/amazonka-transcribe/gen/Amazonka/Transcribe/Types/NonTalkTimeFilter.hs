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
-- Module      : Amazonka.Transcribe.Types.NonTalkTimeFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types.NonTalkTimeFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transcribe.Types.AbsoluteTimeRange
import Amazonka.Transcribe.Types.RelativeTimeRange

-- | Flag the presence or absence of periods of silence in your Call
-- Analytics transcription output.
--
-- Rules using @NonTalkTimeFilter@ are designed to match:
--
-- -   The presence of silence at specified periods throughout the call
--
-- -   The presence of speech at specified periods throughout the call
--
-- See
-- <https://docs.aws.amazon.com/transcribe/latest/dg/tca-categories-batch.html#tca-rules-batch Rule criteria for post-call categories>
-- for usage examples.
--
-- /See:/ 'newNonTalkTimeFilter' smart constructor.
data NonTalkTimeFilter = NonTalkTimeFilter'
  { -- | Makes it possible to specify a time range (in milliseconds) in your
    -- audio, during which you want to search for a period of silence. See for
    -- more detail.
    absoluteTimeRange :: Prelude.Maybe AbsoluteTimeRange,
    -- | Set to @TRUE@ to flag periods of speech. Set to @FALSE@ to flag periods
    -- of silence
    negate :: Prelude.Maybe Prelude.Bool,
    -- | Makes it possible to specify a time range (in percentage) in your media
    -- file, during which you want to search for a period of silence. See for
    -- more detail.
    relativeTimeRange :: Prelude.Maybe RelativeTimeRange,
    -- | Specify the duration, in milliseconds, of the period of silence that you
    -- want to flag. For example, you can flag a silent period that lasts
    -- 30,000 milliseconds.
    threshold :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NonTalkTimeFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'absoluteTimeRange', 'nonTalkTimeFilter_absoluteTimeRange' - Makes it possible to specify a time range (in milliseconds) in your
-- audio, during which you want to search for a period of silence. See for
-- more detail.
--
-- 'negate', 'nonTalkTimeFilter_negate' - Set to @TRUE@ to flag periods of speech. Set to @FALSE@ to flag periods
-- of silence
--
-- 'relativeTimeRange', 'nonTalkTimeFilter_relativeTimeRange' - Makes it possible to specify a time range (in percentage) in your media
-- file, during which you want to search for a period of silence. See for
-- more detail.
--
-- 'threshold', 'nonTalkTimeFilter_threshold' - Specify the duration, in milliseconds, of the period of silence that you
-- want to flag. For example, you can flag a silent period that lasts
-- 30,000 milliseconds.
newNonTalkTimeFilter ::
  NonTalkTimeFilter
newNonTalkTimeFilter =
  NonTalkTimeFilter'
    { absoluteTimeRange =
        Prelude.Nothing,
      negate = Prelude.Nothing,
      relativeTimeRange = Prelude.Nothing,
      threshold = Prelude.Nothing
    }

-- | Makes it possible to specify a time range (in milliseconds) in your
-- audio, during which you want to search for a period of silence. See for
-- more detail.
nonTalkTimeFilter_absoluteTimeRange :: Lens.Lens' NonTalkTimeFilter (Prelude.Maybe AbsoluteTimeRange)
nonTalkTimeFilter_absoluteTimeRange = Lens.lens (\NonTalkTimeFilter' {absoluteTimeRange} -> absoluteTimeRange) (\s@NonTalkTimeFilter' {} a -> s {absoluteTimeRange = a} :: NonTalkTimeFilter)

-- | Set to @TRUE@ to flag periods of speech. Set to @FALSE@ to flag periods
-- of silence
nonTalkTimeFilter_negate :: Lens.Lens' NonTalkTimeFilter (Prelude.Maybe Prelude.Bool)
nonTalkTimeFilter_negate = Lens.lens (\NonTalkTimeFilter' {negate} -> negate) (\s@NonTalkTimeFilter' {} a -> s {negate = a} :: NonTalkTimeFilter)

-- | Makes it possible to specify a time range (in percentage) in your media
-- file, during which you want to search for a period of silence. See for
-- more detail.
nonTalkTimeFilter_relativeTimeRange :: Lens.Lens' NonTalkTimeFilter (Prelude.Maybe RelativeTimeRange)
nonTalkTimeFilter_relativeTimeRange = Lens.lens (\NonTalkTimeFilter' {relativeTimeRange} -> relativeTimeRange) (\s@NonTalkTimeFilter' {} a -> s {relativeTimeRange = a} :: NonTalkTimeFilter)

-- | Specify the duration, in milliseconds, of the period of silence that you
-- want to flag. For example, you can flag a silent period that lasts
-- 30,000 milliseconds.
nonTalkTimeFilter_threshold :: Lens.Lens' NonTalkTimeFilter (Prelude.Maybe Prelude.Natural)
nonTalkTimeFilter_threshold = Lens.lens (\NonTalkTimeFilter' {threshold} -> threshold) (\s@NonTalkTimeFilter' {} a -> s {threshold = a} :: NonTalkTimeFilter)

instance Data.FromJSON NonTalkTimeFilter where
  parseJSON =
    Data.withObject
      "NonTalkTimeFilter"
      ( \x ->
          NonTalkTimeFilter'
            Prelude.<$> (x Data..:? "AbsoluteTimeRange")
            Prelude.<*> (x Data..:? "Negate")
            Prelude.<*> (x Data..:? "RelativeTimeRange")
            Prelude.<*> (x Data..:? "Threshold")
      )

instance Prelude.Hashable NonTalkTimeFilter where
  hashWithSalt _salt NonTalkTimeFilter' {..} =
    _salt
      `Prelude.hashWithSalt` absoluteTimeRange
      `Prelude.hashWithSalt` negate
      `Prelude.hashWithSalt` relativeTimeRange
      `Prelude.hashWithSalt` threshold

instance Prelude.NFData NonTalkTimeFilter where
  rnf NonTalkTimeFilter' {..} =
    Prelude.rnf absoluteTimeRange
      `Prelude.seq` Prelude.rnf negate
      `Prelude.seq` Prelude.rnf relativeTimeRange
      `Prelude.seq` Prelude.rnf threshold

instance Data.ToJSON NonTalkTimeFilter where
  toJSON NonTalkTimeFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AbsoluteTimeRange" Data..=)
              Prelude.<$> absoluteTimeRange,
            ("Negate" Data..=) Prelude.<$> negate,
            ("RelativeTimeRange" Data..=)
              Prelude.<$> relativeTimeRange,
            ("Threshold" Data..=) Prelude.<$> threshold
          ]
      )
