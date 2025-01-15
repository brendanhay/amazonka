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
-- Module      : Amazonka.Transcribe.Types.RelativeTimeRange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types.RelativeTimeRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A time range, in percentage, between two points in your media file.
--
-- You can use @StartPercentage@ and @EndPercentage@ to search a custom
-- segment. For example, setting @StartPercentage@ to 10 and
-- @EndPercentage@ to 50 only searches for your specified criteria in the
-- audio contained between the 10 percent mark and the 50 percent mark of
-- your media file.
--
-- You can use also @First@ to search from the start of the media file
-- until the time that you specify. Or use @Last@ to search from the time
-- that you specify until the end of the media file. For example, setting
-- @First@ to 10 only searches for your specified criteria in the audio
-- contained in the first 10 percent of the media file.
--
-- If you prefer to use milliseconds instead of percentage, see .
--
-- /See:/ 'newRelativeTimeRange' smart constructor.
data RelativeTimeRange = RelativeTimeRange'
  { -- | The time, in percentage, when Amazon Transcribe stops searching for the
    -- specified criteria in your media file. If you include @EndPercentage@ in
    -- your request, you must also include @StartPercentage@.
    endPercentage :: Prelude.Maybe Prelude.Natural,
    -- | The time, in percentage, from the start of your media file until the
    -- specified value. Amazon Transcribe searches for your specified criteria
    -- in this time segment.
    first :: Prelude.Maybe Prelude.Natural,
    -- | The time, in percentage, from the specified value until the end of your
    -- media file. Amazon Transcribe searches for your specified criteria in
    -- this time segment.
    last :: Prelude.Maybe Prelude.Natural,
    -- | The time, in percentage, when Amazon Transcribe starts searching for the
    -- specified criteria in your media file. If you include @StartPercentage@
    -- in your request, you must also include @EndPercentage@.
    startPercentage :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RelativeTimeRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endPercentage', 'relativeTimeRange_endPercentage' - The time, in percentage, when Amazon Transcribe stops searching for the
-- specified criteria in your media file. If you include @EndPercentage@ in
-- your request, you must also include @StartPercentage@.
--
-- 'first', 'relativeTimeRange_first' - The time, in percentage, from the start of your media file until the
-- specified value. Amazon Transcribe searches for your specified criteria
-- in this time segment.
--
-- 'last', 'relativeTimeRange_last' - The time, in percentage, from the specified value until the end of your
-- media file. Amazon Transcribe searches for your specified criteria in
-- this time segment.
--
-- 'startPercentage', 'relativeTimeRange_startPercentage' - The time, in percentage, when Amazon Transcribe starts searching for the
-- specified criteria in your media file. If you include @StartPercentage@
-- in your request, you must also include @EndPercentage@.
newRelativeTimeRange ::
  RelativeTimeRange
newRelativeTimeRange =
  RelativeTimeRange'
    { endPercentage = Prelude.Nothing,
      first = Prelude.Nothing,
      last = Prelude.Nothing,
      startPercentage = Prelude.Nothing
    }

-- | The time, in percentage, when Amazon Transcribe stops searching for the
-- specified criteria in your media file. If you include @EndPercentage@ in
-- your request, you must also include @StartPercentage@.
relativeTimeRange_endPercentage :: Lens.Lens' RelativeTimeRange (Prelude.Maybe Prelude.Natural)
relativeTimeRange_endPercentage = Lens.lens (\RelativeTimeRange' {endPercentage} -> endPercentage) (\s@RelativeTimeRange' {} a -> s {endPercentage = a} :: RelativeTimeRange)

-- | The time, in percentage, from the start of your media file until the
-- specified value. Amazon Transcribe searches for your specified criteria
-- in this time segment.
relativeTimeRange_first :: Lens.Lens' RelativeTimeRange (Prelude.Maybe Prelude.Natural)
relativeTimeRange_first = Lens.lens (\RelativeTimeRange' {first} -> first) (\s@RelativeTimeRange' {} a -> s {first = a} :: RelativeTimeRange)

-- | The time, in percentage, from the specified value until the end of your
-- media file. Amazon Transcribe searches for your specified criteria in
-- this time segment.
relativeTimeRange_last :: Lens.Lens' RelativeTimeRange (Prelude.Maybe Prelude.Natural)
relativeTimeRange_last = Lens.lens (\RelativeTimeRange' {last} -> last) (\s@RelativeTimeRange' {} a -> s {last = a} :: RelativeTimeRange)

-- | The time, in percentage, when Amazon Transcribe starts searching for the
-- specified criteria in your media file. If you include @StartPercentage@
-- in your request, you must also include @EndPercentage@.
relativeTimeRange_startPercentage :: Lens.Lens' RelativeTimeRange (Prelude.Maybe Prelude.Natural)
relativeTimeRange_startPercentage = Lens.lens (\RelativeTimeRange' {startPercentage} -> startPercentage) (\s@RelativeTimeRange' {} a -> s {startPercentage = a} :: RelativeTimeRange)

instance Data.FromJSON RelativeTimeRange where
  parseJSON =
    Data.withObject
      "RelativeTimeRange"
      ( \x ->
          RelativeTimeRange'
            Prelude.<$> (x Data..:? "EndPercentage")
            Prelude.<*> (x Data..:? "First")
            Prelude.<*> (x Data..:? "Last")
            Prelude.<*> (x Data..:? "StartPercentage")
      )

instance Prelude.Hashable RelativeTimeRange where
  hashWithSalt _salt RelativeTimeRange' {..} =
    _salt
      `Prelude.hashWithSalt` endPercentage
      `Prelude.hashWithSalt` first
      `Prelude.hashWithSalt` last
      `Prelude.hashWithSalt` startPercentage

instance Prelude.NFData RelativeTimeRange where
  rnf RelativeTimeRange' {..} =
    Prelude.rnf endPercentage `Prelude.seq`
      Prelude.rnf first `Prelude.seq`
        Prelude.rnf last `Prelude.seq`
          Prelude.rnf startPercentage

instance Data.ToJSON RelativeTimeRange where
  toJSON RelativeTimeRange' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EndPercentage" Data..=) Prelude.<$> endPercentage,
            ("First" Data..=) Prelude.<$> first,
            ("Last" Data..=) Prelude.<$> last,
            ("StartPercentage" Data..=)
              Prelude.<$> startPercentage
          ]
      )
