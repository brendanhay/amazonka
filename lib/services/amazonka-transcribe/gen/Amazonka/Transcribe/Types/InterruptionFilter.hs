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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types.InterruptionFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
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
-- <https://docs.aws.amazon.com/transcribe/latest/dg/call-analytics-create-categories.html#call-analytics-create-categories-rules Rule criteria>
-- for usage examples.
--
-- /See:/ 'newInterruptionFilter' smart constructor.
data InterruptionFilter = InterruptionFilter'
  { -- | Set to @TRUE@ to flag speech that does not contain interruptions. Set to
    -- @FALSE@ to flag speech that contains interruptions.
    negate :: Prelude.Maybe Prelude.Bool,
    -- | Allows you to specify a time range (in milliseconds) in your audio,
    -- during which you want to search for an interruption. See for more
    -- detail.
    absoluteTimeRange :: Prelude.Maybe AbsoluteTimeRange,
    -- | Specify the interrupter you want to flag. Omitting this parameter is
    -- equivalent to specifying both participants.
    participantRole :: Prelude.Maybe ParticipantRole,
    -- | Specify the duration of the interruptions in milliseconds. For example,
    -- you can flag speech that contains more than 10000 milliseconds of
    -- interruptions.
    threshold :: Prelude.Maybe Prelude.Natural,
    -- | Allows you to specify a time range (in percentage) in your media file,
    -- during which you want to search for an interruption. See for more
    -- detail.
    relativeTimeRange :: Prelude.Maybe RelativeTimeRange
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
-- 'negate', 'interruptionFilter_negate' - Set to @TRUE@ to flag speech that does not contain interruptions. Set to
-- @FALSE@ to flag speech that contains interruptions.
--
-- 'absoluteTimeRange', 'interruptionFilter_absoluteTimeRange' - Allows you to specify a time range (in milliseconds) in your audio,
-- during which you want to search for an interruption. See for more
-- detail.
--
-- 'participantRole', 'interruptionFilter_participantRole' - Specify the interrupter you want to flag. Omitting this parameter is
-- equivalent to specifying both participants.
--
-- 'threshold', 'interruptionFilter_threshold' - Specify the duration of the interruptions in milliseconds. For example,
-- you can flag speech that contains more than 10000 milliseconds of
-- interruptions.
--
-- 'relativeTimeRange', 'interruptionFilter_relativeTimeRange' - Allows you to specify a time range (in percentage) in your media file,
-- during which you want to search for an interruption. See for more
-- detail.
newInterruptionFilter ::
  InterruptionFilter
newInterruptionFilter =
  InterruptionFilter'
    { negate = Prelude.Nothing,
      absoluteTimeRange = Prelude.Nothing,
      participantRole = Prelude.Nothing,
      threshold = Prelude.Nothing,
      relativeTimeRange = Prelude.Nothing
    }

-- | Set to @TRUE@ to flag speech that does not contain interruptions. Set to
-- @FALSE@ to flag speech that contains interruptions.
interruptionFilter_negate :: Lens.Lens' InterruptionFilter (Prelude.Maybe Prelude.Bool)
interruptionFilter_negate = Lens.lens (\InterruptionFilter' {negate} -> negate) (\s@InterruptionFilter' {} a -> s {negate = a} :: InterruptionFilter)

-- | Allows you to specify a time range (in milliseconds) in your audio,
-- during which you want to search for an interruption. See for more
-- detail.
interruptionFilter_absoluteTimeRange :: Lens.Lens' InterruptionFilter (Prelude.Maybe AbsoluteTimeRange)
interruptionFilter_absoluteTimeRange = Lens.lens (\InterruptionFilter' {absoluteTimeRange} -> absoluteTimeRange) (\s@InterruptionFilter' {} a -> s {absoluteTimeRange = a} :: InterruptionFilter)

-- | Specify the interrupter you want to flag. Omitting this parameter is
-- equivalent to specifying both participants.
interruptionFilter_participantRole :: Lens.Lens' InterruptionFilter (Prelude.Maybe ParticipantRole)
interruptionFilter_participantRole = Lens.lens (\InterruptionFilter' {participantRole} -> participantRole) (\s@InterruptionFilter' {} a -> s {participantRole = a} :: InterruptionFilter)

-- | Specify the duration of the interruptions in milliseconds. For example,
-- you can flag speech that contains more than 10000 milliseconds of
-- interruptions.
interruptionFilter_threshold :: Lens.Lens' InterruptionFilter (Prelude.Maybe Prelude.Natural)
interruptionFilter_threshold = Lens.lens (\InterruptionFilter' {threshold} -> threshold) (\s@InterruptionFilter' {} a -> s {threshold = a} :: InterruptionFilter)

-- | Allows you to specify a time range (in percentage) in your media file,
-- during which you want to search for an interruption. See for more
-- detail.
interruptionFilter_relativeTimeRange :: Lens.Lens' InterruptionFilter (Prelude.Maybe RelativeTimeRange)
interruptionFilter_relativeTimeRange = Lens.lens (\InterruptionFilter' {relativeTimeRange} -> relativeTimeRange) (\s@InterruptionFilter' {} a -> s {relativeTimeRange = a} :: InterruptionFilter)

instance Core.FromJSON InterruptionFilter where
  parseJSON =
    Core.withObject
      "InterruptionFilter"
      ( \x ->
          InterruptionFilter'
            Prelude.<$> (x Core..:? "Negate")
            Prelude.<*> (x Core..:? "AbsoluteTimeRange")
            Prelude.<*> (x Core..:? "ParticipantRole")
            Prelude.<*> (x Core..:? "Threshold")
            Prelude.<*> (x Core..:? "RelativeTimeRange")
      )

instance Prelude.Hashable InterruptionFilter where
  hashWithSalt _salt InterruptionFilter' {..} =
    _salt `Prelude.hashWithSalt` negate
      `Prelude.hashWithSalt` absoluteTimeRange
      `Prelude.hashWithSalt` participantRole
      `Prelude.hashWithSalt` threshold
      `Prelude.hashWithSalt` relativeTimeRange

instance Prelude.NFData InterruptionFilter where
  rnf InterruptionFilter' {..} =
    Prelude.rnf negate
      `Prelude.seq` Prelude.rnf absoluteTimeRange
      `Prelude.seq` Prelude.rnf participantRole
      `Prelude.seq` Prelude.rnf threshold
      `Prelude.seq` Prelude.rnf relativeTimeRange

instance Core.ToJSON InterruptionFilter where
  toJSON InterruptionFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Negate" Core..=) Prelude.<$> negate,
            ("AbsoluteTimeRange" Core..=)
              Prelude.<$> absoluteTimeRange,
            ("ParticipantRole" Core..=)
              Prelude.<$> participantRole,
            ("Threshold" Core..=) Prelude.<$> threshold,
            ("RelativeTimeRange" Core..=)
              Prelude.<$> relativeTimeRange
          ]
      )
