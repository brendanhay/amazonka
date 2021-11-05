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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types.NonTalkTimeFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transcribe.Types.AbsoluteTimeRange
import Amazonka.Transcribe.Types.RelativeTimeRange

-- | An object that enables you to configure your category to be applied to
-- call analytics jobs where either the customer or agent was interrupted.
--
-- /See:/ 'newNonTalkTimeFilter' smart constructor.
data NonTalkTimeFilter = NonTalkTimeFilter'
  { -- | An object that allows percentages to specify the proportion of the call
    -- where there was silence. For example, you can specify the first half of
    -- the call. You can also specify the period of time between halfway
    -- through to three-quarters of the way through the call. Because the
    -- length of conversation can vary between calls, you can apply relative
    -- time ranges across all calls.
    relativeTimeRange :: Prelude.Maybe RelativeTimeRange,
    -- | Set to @TRUE@ to look for a time period when people were talking.
    negate :: Prelude.Maybe Prelude.Bool,
    -- | The duration of the period when neither the customer nor agent was
    -- talking.
    threshold :: Prelude.Maybe Prelude.Natural,
    -- | An object you can use to specify a time range (in milliseconds) for when
    -- no one is talking. For example, you could specify a time period between
    -- the 30,000 millisecond mark and the 45,000 millisecond mark. You could
    -- also specify the time period as the first 15,000 milliseconds or the
    -- last 15,000 milliseconds.
    absoluteTimeRange :: Prelude.Maybe AbsoluteTimeRange
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
-- 'relativeTimeRange', 'nonTalkTimeFilter_relativeTimeRange' - An object that allows percentages to specify the proportion of the call
-- where there was silence. For example, you can specify the first half of
-- the call. You can also specify the period of time between halfway
-- through to three-quarters of the way through the call. Because the
-- length of conversation can vary between calls, you can apply relative
-- time ranges across all calls.
--
-- 'negate', 'nonTalkTimeFilter_negate' - Set to @TRUE@ to look for a time period when people were talking.
--
-- 'threshold', 'nonTalkTimeFilter_threshold' - The duration of the period when neither the customer nor agent was
-- talking.
--
-- 'absoluteTimeRange', 'nonTalkTimeFilter_absoluteTimeRange' - An object you can use to specify a time range (in milliseconds) for when
-- no one is talking. For example, you could specify a time period between
-- the 30,000 millisecond mark and the 45,000 millisecond mark. You could
-- also specify the time period as the first 15,000 milliseconds or the
-- last 15,000 milliseconds.
newNonTalkTimeFilter ::
  NonTalkTimeFilter
newNonTalkTimeFilter =
  NonTalkTimeFilter'
    { relativeTimeRange =
        Prelude.Nothing,
      negate = Prelude.Nothing,
      threshold = Prelude.Nothing,
      absoluteTimeRange = Prelude.Nothing
    }

-- | An object that allows percentages to specify the proportion of the call
-- where there was silence. For example, you can specify the first half of
-- the call. You can also specify the period of time between halfway
-- through to three-quarters of the way through the call. Because the
-- length of conversation can vary between calls, you can apply relative
-- time ranges across all calls.
nonTalkTimeFilter_relativeTimeRange :: Lens.Lens' NonTalkTimeFilter (Prelude.Maybe RelativeTimeRange)
nonTalkTimeFilter_relativeTimeRange = Lens.lens (\NonTalkTimeFilter' {relativeTimeRange} -> relativeTimeRange) (\s@NonTalkTimeFilter' {} a -> s {relativeTimeRange = a} :: NonTalkTimeFilter)

-- | Set to @TRUE@ to look for a time period when people were talking.
nonTalkTimeFilter_negate :: Lens.Lens' NonTalkTimeFilter (Prelude.Maybe Prelude.Bool)
nonTalkTimeFilter_negate = Lens.lens (\NonTalkTimeFilter' {negate} -> negate) (\s@NonTalkTimeFilter' {} a -> s {negate = a} :: NonTalkTimeFilter)

-- | The duration of the period when neither the customer nor agent was
-- talking.
nonTalkTimeFilter_threshold :: Lens.Lens' NonTalkTimeFilter (Prelude.Maybe Prelude.Natural)
nonTalkTimeFilter_threshold = Lens.lens (\NonTalkTimeFilter' {threshold} -> threshold) (\s@NonTalkTimeFilter' {} a -> s {threshold = a} :: NonTalkTimeFilter)

-- | An object you can use to specify a time range (in milliseconds) for when
-- no one is talking. For example, you could specify a time period between
-- the 30,000 millisecond mark and the 45,000 millisecond mark. You could
-- also specify the time period as the first 15,000 milliseconds or the
-- last 15,000 milliseconds.
nonTalkTimeFilter_absoluteTimeRange :: Lens.Lens' NonTalkTimeFilter (Prelude.Maybe AbsoluteTimeRange)
nonTalkTimeFilter_absoluteTimeRange = Lens.lens (\NonTalkTimeFilter' {absoluteTimeRange} -> absoluteTimeRange) (\s@NonTalkTimeFilter' {} a -> s {absoluteTimeRange = a} :: NonTalkTimeFilter)

instance Core.FromJSON NonTalkTimeFilter where
  parseJSON =
    Core.withObject
      "NonTalkTimeFilter"
      ( \x ->
          NonTalkTimeFilter'
            Prelude.<$> (x Core..:? "RelativeTimeRange")
            Prelude.<*> (x Core..:? "Negate")
            Prelude.<*> (x Core..:? "Threshold")
            Prelude.<*> (x Core..:? "AbsoluteTimeRange")
      )

instance Prelude.Hashable NonTalkTimeFilter

instance Prelude.NFData NonTalkTimeFilter

instance Core.ToJSON NonTalkTimeFilter where
  toJSON NonTalkTimeFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RelativeTimeRange" Core..=)
              Prelude.<$> relativeTimeRange,
            ("Negate" Core..=) Prelude.<$> negate,
            ("Threshold" Core..=) Prelude.<$> threshold,
            ("AbsoluteTimeRange" Core..=)
              Prelude.<$> absoluteTimeRange
          ]
      )
