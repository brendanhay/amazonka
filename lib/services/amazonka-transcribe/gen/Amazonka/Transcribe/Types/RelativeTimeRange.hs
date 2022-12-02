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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
-- until the time you specify, or @Last@ to search from the time you
-- specify until the end of the media file. For example, setting @First@ to
-- 10 only searches for your specified criteria in the audio contained in
-- the first 10 percent of the media file.
--
-- If you prefer to use milliseconds instead of percentage, see .
--
-- /See:/ 'newRelativeTimeRange' smart constructor.
data RelativeTimeRange = RelativeTimeRange'
  { -- | The time, in percentage, from the value you specify until the end of
    -- your media file in which Amazon Transcribe searches for your specified
    -- criteria.
    last :: Prelude.Maybe Prelude.Natural,
    -- | The time, in percentage, when Amazon Transcribe stops searching for the
    -- specified criteria in your media file. If you include @EndPercentage@ in
    -- your request, you must also include @StartPercentage@.
    endPercentage :: Prelude.Maybe Prelude.Natural,
    -- | The time, in percentage, when Amazon Transcribe starts searching for the
    -- specified criteria in your media file. If you include @StartPercentage@
    -- in your request, you must also include @EndPercentage@.
    startPercentage :: Prelude.Maybe Prelude.Natural,
    -- | The time, in percentage, from the start of your media file until the
    -- value you specify in which Amazon Transcribe searches for your specified
    -- criteria.
    first :: Prelude.Maybe Prelude.Natural
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
-- 'last', 'relativeTimeRange_last' - The time, in percentage, from the value you specify until the end of
-- your media file in which Amazon Transcribe searches for your specified
-- criteria.
--
-- 'endPercentage', 'relativeTimeRange_endPercentage' - The time, in percentage, when Amazon Transcribe stops searching for the
-- specified criteria in your media file. If you include @EndPercentage@ in
-- your request, you must also include @StartPercentage@.
--
-- 'startPercentage', 'relativeTimeRange_startPercentage' - The time, in percentage, when Amazon Transcribe starts searching for the
-- specified criteria in your media file. If you include @StartPercentage@
-- in your request, you must also include @EndPercentage@.
--
-- 'first', 'relativeTimeRange_first' - The time, in percentage, from the start of your media file until the
-- value you specify in which Amazon Transcribe searches for your specified
-- criteria.
newRelativeTimeRange ::
  RelativeTimeRange
newRelativeTimeRange =
  RelativeTimeRange'
    { last = Prelude.Nothing,
      endPercentage = Prelude.Nothing,
      startPercentage = Prelude.Nothing,
      first = Prelude.Nothing
    }

-- | The time, in percentage, from the value you specify until the end of
-- your media file in which Amazon Transcribe searches for your specified
-- criteria.
relativeTimeRange_last :: Lens.Lens' RelativeTimeRange (Prelude.Maybe Prelude.Natural)
relativeTimeRange_last = Lens.lens (\RelativeTimeRange' {last} -> last) (\s@RelativeTimeRange' {} a -> s {last = a} :: RelativeTimeRange)

-- | The time, in percentage, when Amazon Transcribe stops searching for the
-- specified criteria in your media file. If you include @EndPercentage@ in
-- your request, you must also include @StartPercentage@.
relativeTimeRange_endPercentage :: Lens.Lens' RelativeTimeRange (Prelude.Maybe Prelude.Natural)
relativeTimeRange_endPercentage = Lens.lens (\RelativeTimeRange' {endPercentage} -> endPercentage) (\s@RelativeTimeRange' {} a -> s {endPercentage = a} :: RelativeTimeRange)

-- | The time, in percentage, when Amazon Transcribe starts searching for the
-- specified criteria in your media file. If you include @StartPercentage@
-- in your request, you must also include @EndPercentage@.
relativeTimeRange_startPercentage :: Lens.Lens' RelativeTimeRange (Prelude.Maybe Prelude.Natural)
relativeTimeRange_startPercentage = Lens.lens (\RelativeTimeRange' {startPercentage} -> startPercentage) (\s@RelativeTimeRange' {} a -> s {startPercentage = a} :: RelativeTimeRange)

-- | The time, in percentage, from the start of your media file until the
-- value you specify in which Amazon Transcribe searches for your specified
-- criteria.
relativeTimeRange_first :: Lens.Lens' RelativeTimeRange (Prelude.Maybe Prelude.Natural)
relativeTimeRange_first = Lens.lens (\RelativeTimeRange' {first} -> first) (\s@RelativeTimeRange' {} a -> s {first = a} :: RelativeTimeRange)

instance Data.FromJSON RelativeTimeRange where
  parseJSON =
    Data.withObject
      "RelativeTimeRange"
      ( \x ->
          RelativeTimeRange'
            Prelude.<$> (x Data..:? "Last")
            Prelude.<*> (x Data..:? "EndPercentage")
            Prelude.<*> (x Data..:? "StartPercentage")
            Prelude.<*> (x Data..:? "First")
      )

instance Prelude.Hashable RelativeTimeRange where
  hashWithSalt _salt RelativeTimeRange' {..} =
    _salt `Prelude.hashWithSalt` last
      `Prelude.hashWithSalt` endPercentage
      `Prelude.hashWithSalt` startPercentage
      `Prelude.hashWithSalt` first

instance Prelude.NFData RelativeTimeRange where
  rnf RelativeTimeRange' {..} =
    Prelude.rnf last
      `Prelude.seq` Prelude.rnf endPercentage
      `Prelude.seq` Prelude.rnf startPercentage
      `Prelude.seq` Prelude.rnf first

instance Data.ToJSON RelativeTimeRange where
  toJSON RelativeTimeRange' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Last" Data..=) Prelude.<$> last,
            ("EndPercentage" Data..=) Prelude.<$> endPercentage,
            ("StartPercentage" Data..=)
              Prelude.<$> startPercentage,
            ("First" Data..=) Prelude.<$> first
          ]
      )
