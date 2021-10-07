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
-- Module      : Network.AWS.Transcribe.Types.InterruptionFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.InterruptionFilter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Transcribe.Types.AbsoluteTimeRange
import Network.AWS.Transcribe.Types.ParticipantRole
import Network.AWS.Transcribe.Types.RelativeTimeRange

-- | An object that enables you to configure your category to be applied to
-- call analytics jobs where either the customer or agent was interrupted.
--
-- /See:/ 'newInterruptionFilter' smart constructor.
data InterruptionFilter = InterruptionFilter'
  { -- | The duration of the interruption.
    threshold :: Prelude.Maybe Prelude.Natural,
    -- | An object that allows percentages to specify the proportion of the call
    -- where there was a interruption. For example, you can specify the first
    -- half of the call. You can also specify the period of time between
    -- halfway through to three-quarters of the way through the call. Because
    -- the length of conversation can vary between calls, you can apply
    -- relative time ranges across all calls.
    relativeTimeRange :: Prelude.Maybe RelativeTimeRange,
    -- | Set to @TRUE@ to look for a time period where there was no interruption.
    negate :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether the caller or customer was interrupting.
    participantRole :: Prelude.Maybe ParticipantRole,
    -- | An object you can use to specify a time range (in milliseconds) for when
    -- you\'d want to find the interruption. For example, you could search for
    -- an interruption between the 30,000 millisecond mark and the 45,000
    -- millisecond mark. You could also specify the time period as the first
    -- 15,000 milliseconds or the last 15,000 milliseconds.
    absoluteTimeRange :: Prelude.Maybe AbsoluteTimeRange
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
-- 'threshold', 'interruptionFilter_threshold' - The duration of the interruption.
--
-- 'relativeTimeRange', 'interruptionFilter_relativeTimeRange' - An object that allows percentages to specify the proportion of the call
-- where there was a interruption. For example, you can specify the first
-- half of the call. You can also specify the period of time between
-- halfway through to three-quarters of the way through the call. Because
-- the length of conversation can vary between calls, you can apply
-- relative time ranges across all calls.
--
-- 'negate', 'interruptionFilter_negate' - Set to @TRUE@ to look for a time period where there was no interruption.
--
-- 'participantRole', 'interruptionFilter_participantRole' - Indicates whether the caller or customer was interrupting.
--
-- 'absoluteTimeRange', 'interruptionFilter_absoluteTimeRange' - An object you can use to specify a time range (in milliseconds) for when
-- you\'d want to find the interruption. For example, you could search for
-- an interruption between the 30,000 millisecond mark and the 45,000
-- millisecond mark. You could also specify the time period as the first
-- 15,000 milliseconds or the last 15,000 milliseconds.
newInterruptionFilter ::
  InterruptionFilter
newInterruptionFilter =
  InterruptionFilter'
    { threshold = Prelude.Nothing,
      relativeTimeRange = Prelude.Nothing,
      negate = Prelude.Nothing,
      participantRole = Prelude.Nothing,
      absoluteTimeRange = Prelude.Nothing
    }

-- | The duration of the interruption.
interruptionFilter_threshold :: Lens.Lens' InterruptionFilter (Prelude.Maybe Prelude.Natural)
interruptionFilter_threshold = Lens.lens (\InterruptionFilter' {threshold} -> threshold) (\s@InterruptionFilter' {} a -> s {threshold = a} :: InterruptionFilter)

-- | An object that allows percentages to specify the proportion of the call
-- where there was a interruption. For example, you can specify the first
-- half of the call. You can also specify the period of time between
-- halfway through to three-quarters of the way through the call. Because
-- the length of conversation can vary between calls, you can apply
-- relative time ranges across all calls.
interruptionFilter_relativeTimeRange :: Lens.Lens' InterruptionFilter (Prelude.Maybe RelativeTimeRange)
interruptionFilter_relativeTimeRange = Lens.lens (\InterruptionFilter' {relativeTimeRange} -> relativeTimeRange) (\s@InterruptionFilter' {} a -> s {relativeTimeRange = a} :: InterruptionFilter)

-- | Set to @TRUE@ to look for a time period where there was no interruption.
interruptionFilter_negate :: Lens.Lens' InterruptionFilter (Prelude.Maybe Prelude.Bool)
interruptionFilter_negate = Lens.lens (\InterruptionFilter' {negate} -> negate) (\s@InterruptionFilter' {} a -> s {negate = a} :: InterruptionFilter)

-- | Indicates whether the caller or customer was interrupting.
interruptionFilter_participantRole :: Lens.Lens' InterruptionFilter (Prelude.Maybe ParticipantRole)
interruptionFilter_participantRole = Lens.lens (\InterruptionFilter' {participantRole} -> participantRole) (\s@InterruptionFilter' {} a -> s {participantRole = a} :: InterruptionFilter)

-- | An object you can use to specify a time range (in milliseconds) for when
-- you\'d want to find the interruption. For example, you could search for
-- an interruption between the 30,000 millisecond mark and the 45,000
-- millisecond mark. You could also specify the time period as the first
-- 15,000 milliseconds or the last 15,000 milliseconds.
interruptionFilter_absoluteTimeRange :: Lens.Lens' InterruptionFilter (Prelude.Maybe AbsoluteTimeRange)
interruptionFilter_absoluteTimeRange = Lens.lens (\InterruptionFilter' {absoluteTimeRange} -> absoluteTimeRange) (\s@InterruptionFilter' {} a -> s {absoluteTimeRange = a} :: InterruptionFilter)

instance Core.FromJSON InterruptionFilter where
  parseJSON =
    Core.withObject
      "InterruptionFilter"
      ( \x ->
          InterruptionFilter'
            Prelude.<$> (x Core..:? "Threshold")
            Prelude.<*> (x Core..:? "RelativeTimeRange")
            Prelude.<*> (x Core..:? "Negate")
            Prelude.<*> (x Core..:? "ParticipantRole")
            Prelude.<*> (x Core..:? "AbsoluteTimeRange")
      )

instance Prelude.Hashable InterruptionFilter

instance Prelude.NFData InterruptionFilter

instance Core.ToJSON InterruptionFilter where
  toJSON InterruptionFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Threshold" Core..=) Prelude.<$> threshold,
            ("RelativeTimeRange" Core..=)
              Prelude.<$> relativeTimeRange,
            ("Negate" Core..=) Prelude.<$> negate,
            ("ParticipantRole" Core..=)
              Prelude.<$> participantRole,
            ("AbsoluteTimeRange" Core..=)
              Prelude.<$> absoluteTimeRange
          ]
      )
