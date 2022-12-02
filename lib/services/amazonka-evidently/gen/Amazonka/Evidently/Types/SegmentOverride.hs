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
-- Module      : Amazonka.Evidently.Types.SegmentOverride
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Evidently.Types.SegmentOverride where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | This structure specifies a segment that you have already created, and
-- defines the traffic split for that segment to be used in a launch.
--
-- /See:/ 'newSegmentOverride' smart constructor.
data SegmentOverride = SegmentOverride'
  { -- | A number indicating the order to use to evaluate segment overrides, if
    -- there are more than one. Segment overrides with lower numbers are
    -- evaluated first.
    evaluationOrder :: Prelude.Integer,
    -- | The ARN of the segment to use.
    segment :: Prelude.Text,
    -- | The traffic allocation percentages among the feature variations to
    -- assign to this segment. This is a set of key-value pairs. The keys are
    -- variation names. The values represent the amount of traffic to allocate
    -- to that variation for this segment. This is expressed in thousandths of
    -- a percent, so a weight of 50000 represents 50% of traffic.
    weights :: Prelude.HashMap Prelude.Text Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SegmentOverride' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'evaluationOrder', 'segmentOverride_evaluationOrder' - A number indicating the order to use to evaluate segment overrides, if
-- there are more than one. Segment overrides with lower numbers are
-- evaluated first.
--
-- 'segment', 'segmentOverride_segment' - The ARN of the segment to use.
--
-- 'weights', 'segmentOverride_weights' - The traffic allocation percentages among the feature variations to
-- assign to this segment. This is a set of key-value pairs. The keys are
-- variation names. The values represent the amount of traffic to allocate
-- to that variation for this segment. This is expressed in thousandths of
-- a percent, so a weight of 50000 represents 50% of traffic.
newSegmentOverride ::
  -- | 'evaluationOrder'
  Prelude.Integer ->
  -- | 'segment'
  Prelude.Text ->
  SegmentOverride
newSegmentOverride pEvaluationOrder_ pSegment_ =
  SegmentOverride'
    { evaluationOrder =
        pEvaluationOrder_,
      segment = pSegment_,
      weights = Prelude.mempty
    }

-- | A number indicating the order to use to evaluate segment overrides, if
-- there are more than one. Segment overrides with lower numbers are
-- evaluated first.
segmentOverride_evaluationOrder :: Lens.Lens' SegmentOverride Prelude.Integer
segmentOverride_evaluationOrder = Lens.lens (\SegmentOverride' {evaluationOrder} -> evaluationOrder) (\s@SegmentOverride' {} a -> s {evaluationOrder = a} :: SegmentOverride)

-- | The ARN of the segment to use.
segmentOverride_segment :: Lens.Lens' SegmentOverride Prelude.Text
segmentOverride_segment = Lens.lens (\SegmentOverride' {segment} -> segment) (\s@SegmentOverride' {} a -> s {segment = a} :: SegmentOverride)

-- | The traffic allocation percentages among the feature variations to
-- assign to this segment. This is a set of key-value pairs. The keys are
-- variation names. The values represent the amount of traffic to allocate
-- to that variation for this segment. This is expressed in thousandths of
-- a percent, so a weight of 50000 represents 50% of traffic.
segmentOverride_weights :: Lens.Lens' SegmentOverride (Prelude.HashMap Prelude.Text Prelude.Natural)
segmentOverride_weights = Lens.lens (\SegmentOverride' {weights} -> weights) (\s@SegmentOverride' {} a -> s {weights = a} :: SegmentOverride) Prelude.. Lens.coerced

instance Data.FromJSON SegmentOverride where
  parseJSON =
    Data.withObject
      "SegmentOverride"
      ( \x ->
          SegmentOverride'
            Prelude.<$> (x Data..: "evaluationOrder")
            Prelude.<*> (x Data..: "segment")
            Prelude.<*> (x Data..:? "weights" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable SegmentOverride where
  hashWithSalt _salt SegmentOverride' {..} =
    _salt `Prelude.hashWithSalt` evaluationOrder
      `Prelude.hashWithSalt` segment
      `Prelude.hashWithSalt` weights

instance Prelude.NFData SegmentOverride where
  rnf SegmentOverride' {..} =
    Prelude.rnf evaluationOrder
      `Prelude.seq` Prelude.rnf segment
      `Prelude.seq` Prelude.rnf weights

instance Data.ToJSON SegmentOverride where
  toJSON SegmentOverride' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("evaluationOrder" Data..= evaluationOrder),
            Prelude.Just ("segment" Data..= segment),
            Prelude.Just ("weights" Data..= weights)
          ]
      )
