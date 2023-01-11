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
-- Module      : Amazonka.Transcribe.Types.InterruptionFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types.InterruptionFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transcribe.Types.AbsoluteTimeRange
import Amazonka.Transcribe.Types.ParticipantRole
import Amazonka.Transcribe.Types.RelativeTimeRange

-- | Flag the presence or absence of interruptions in your Call Analytics
-- transcription output.
--
-- Rules using @InterruptionFilter@ are designed to match:
--
-- -   Instances where an agent interrupts a customer
--
-- -   Instances where a customer interrupts an agent
--
-- -   Either participant interrupting the other
--
-- -   A lack of interruptions
--
-- See
-- <https://docs.aws.amazon.com/transcribe/latest/dg/tca-categories-batch.html#tca-rules-batch Rule criteria for batch categories>
-- for usage examples.
--
-- /See:/ 'newInterruptionFilter' smart constructor.
data InterruptionFilter = InterruptionFilter'
  { -- | Makes it possible to specify a time range (in milliseconds) in your
    -- audio, during which you want to search for an interruption. See for more
    -- detail.
    absoluteTimeRange :: Prelude.Maybe AbsoluteTimeRange,
    -- | Set to @TRUE@ to flag speech that does not contain interruptions. Set to
    -- @FALSE@ to flag speech that contains interruptions.
    negate :: Prelude.Maybe Prelude.Bool,
    -- | Specify the interrupter that you want to flag. Omitting this parameter
    -- is equivalent to specifying both participants.
    participantRole :: Prelude.Maybe ParticipantRole,
    -- | Makes it possible to specify a time range (in percentage) in your media
    -- file, during which you want to search for an interruption. See for more
    -- detail.
    relativeTimeRange :: Prelude.Maybe RelativeTimeRange,
    -- | Specify the duration of the interruptions in milliseconds. For example,
    -- you can flag speech that contains more than 10,000 milliseconds of
    -- interruptions.
    threshold :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InterruptionFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'absoluteTimeRange', 'interruptionFilter_absoluteTimeRange' - Makes it possible to specify a time range (in milliseconds) in your
-- audio, during which you want to search for an interruption. See for more
-- detail.
--
-- 'negate', 'interruptionFilter_negate' - Set to @TRUE@ to flag speech that does not contain interruptions. Set to
-- @FALSE@ to flag speech that contains interruptions.
--
-- 'participantRole', 'interruptionFilter_participantRole' - Specify the interrupter that you want to flag. Omitting this parameter
-- is equivalent to specifying both participants.
--
-- 'relativeTimeRange', 'interruptionFilter_relativeTimeRange' - Makes it possible to specify a time range (in percentage) in your media
-- file, during which you want to search for an interruption. See for more
-- detail.
--
-- 'threshold', 'interruptionFilter_threshold' - Specify the duration of the interruptions in milliseconds. For example,
-- you can flag speech that contains more than 10,000 milliseconds of
-- interruptions.
newInterruptionFilter ::
  InterruptionFilter
newInterruptionFilter =
  InterruptionFilter'
    { absoluteTimeRange =
        Prelude.Nothing,
      negate = Prelude.Nothing,
      participantRole = Prelude.Nothing,
      relativeTimeRange = Prelude.Nothing,
      threshold = Prelude.Nothing
    }

-- | Makes it possible to specify a time range (in milliseconds) in your
-- audio, during which you want to search for an interruption. See for more
-- detail.
interruptionFilter_absoluteTimeRange :: Lens.Lens' InterruptionFilter (Prelude.Maybe AbsoluteTimeRange)
interruptionFilter_absoluteTimeRange = Lens.lens (\InterruptionFilter' {absoluteTimeRange} -> absoluteTimeRange) (\s@InterruptionFilter' {} a -> s {absoluteTimeRange = a} :: InterruptionFilter)

-- | Set to @TRUE@ to flag speech that does not contain interruptions. Set to
-- @FALSE@ to flag speech that contains interruptions.
interruptionFilter_negate :: Lens.Lens' InterruptionFilter (Prelude.Maybe Prelude.Bool)
interruptionFilter_negate = Lens.lens (\InterruptionFilter' {negate} -> negate) (\s@InterruptionFilter' {} a -> s {negate = a} :: InterruptionFilter)

-- | Specify the interrupter that you want to flag. Omitting this parameter
-- is equivalent to specifying both participants.
interruptionFilter_participantRole :: Lens.Lens' InterruptionFilter (Prelude.Maybe ParticipantRole)
interruptionFilter_participantRole = Lens.lens (\InterruptionFilter' {participantRole} -> participantRole) (\s@InterruptionFilter' {} a -> s {participantRole = a} :: InterruptionFilter)

-- | Makes it possible to specify a time range (in percentage) in your media
-- file, during which you want to search for an interruption. See for more
-- detail.
interruptionFilter_relativeTimeRange :: Lens.Lens' InterruptionFilter (Prelude.Maybe RelativeTimeRange)
interruptionFilter_relativeTimeRange = Lens.lens (\InterruptionFilter' {relativeTimeRange} -> relativeTimeRange) (\s@InterruptionFilter' {} a -> s {relativeTimeRange = a} :: InterruptionFilter)

-- | Specify the duration of the interruptions in milliseconds. For example,
-- you can flag speech that contains more than 10,000 milliseconds of
-- interruptions.
interruptionFilter_threshold :: Lens.Lens' InterruptionFilter (Prelude.Maybe Prelude.Natural)
interruptionFilter_threshold = Lens.lens (\InterruptionFilter' {threshold} -> threshold) (\s@InterruptionFilter' {} a -> s {threshold = a} :: InterruptionFilter)

instance Data.FromJSON InterruptionFilter where
  parseJSON =
    Data.withObject
      "InterruptionFilter"
      ( \x ->
          InterruptionFilter'
            Prelude.<$> (x Data..:? "AbsoluteTimeRange")
            Prelude.<*> (x Data..:? "Negate")
            Prelude.<*> (x Data..:? "ParticipantRole")
            Prelude.<*> (x Data..:? "RelativeTimeRange")
            Prelude.<*> (x Data..:? "Threshold")
      )

instance Prelude.Hashable InterruptionFilter where
  hashWithSalt _salt InterruptionFilter' {..} =
    _salt `Prelude.hashWithSalt` absoluteTimeRange
      `Prelude.hashWithSalt` negate
      `Prelude.hashWithSalt` participantRole
      `Prelude.hashWithSalt` relativeTimeRange
      `Prelude.hashWithSalt` threshold

instance Prelude.NFData InterruptionFilter where
  rnf InterruptionFilter' {..} =
    Prelude.rnf absoluteTimeRange
      `Prelude.seq` Prelude.rnf negate
      `Prelude.seq` Prelude.rnf participantRole
      `Prelude.seq` Prelude.rnf relativeTimeRange
      `Prelude.seq` Prelude.rnf threshold

instance Data.ToJSON InterruptionFilter where
  toJSON InterruptionFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AbsoluteTimeRange" Data..=)
              Prelude.<$> absoluteTimeRange,
            ("Negate" Data..=) Prelude.<$> negate,
            ("ParticipantRole" Data..=)
              Prelude.<$> participantRole,
            ("RelativeTimeRange" Data..=)
              Prelude.<$> relativeTimeRange,
            ("Threshold" Data..=) Prelude.<$> threshold
          ]
      )
