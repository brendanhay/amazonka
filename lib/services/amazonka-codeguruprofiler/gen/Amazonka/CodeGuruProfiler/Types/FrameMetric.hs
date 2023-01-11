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
-- Module      : Amazonka.CodeGuruProfiler.Types.FrameMetric
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeGuruProfiler.Types.FrameMetric where

import Amazonka.CodeGuruProfiler.Types.MetricType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The frame name, metric type, and thread states. These are used to derive
-- the value of the metric for the frame.
--
-- /See:/ 'newFrameMetric' smart constructor.
data FrameMetric = FrameMetric'
  { -- | Name of the method common across the multiple occurrences of a frame in
    -- an application profile.
    frameName :: Prelude.Text,
    -- | List of application runtime thread states used to get the counts for a
    -- frame a derive a metric value.
    threadStates :: [Prelude.Text],
    -- | A type of aggregation that specifies how a metric for a frame is
    -- analyzed. The supported value @AggregatedRelativeTotalTime@ is an
    -- aggregation of the metric value for one frame that is calculated across
    -- the occurrences of all frames in a profile.
    type' :: MetricType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FrameMetric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'frameName', 'frameMetric_frameName' - Name of the method common across the multiple occurrences of a frame in
-- an application profile.
--
-- 'threadStates', 'frameMetric_threadStates' - List of application runtime thread states used to get the counts for a
-- frame a derive a metric value.
--
-- 'type'', 'frameMetric_type' - A type of aggregation that specifies how a metric for a frame is
-- analyzed. The supported value @AggregatedRelativeTotalTime@ is an
-- aggregation of the metric value for one frame that is calculated across
-- the occurrences of all frames in a profile.
newFrameMetric ::
  -- | 'frameName'
  Prelude.Text ->
  -- | 'type''
  MetricType ->
  FrameMetric
newFrameMetric pFrameName_ pType_ =
  FrameMetric'
    { frameName = pFrameName_,
      threadStates = Prelude.mempty,
      type' = pType_
    }

-- | Name of the method common across the multiple occurrences of a frame in
-- an application profile.
frameMetric_frameName :: Lens.Lens' FrameMetric Prelude.Text
frameMetric_frameName = Lens.lens (\FrameMetric' {frameName} -> frameName) (\s@FrameMetric' {} a -> s {frameName = a} :: FrameMetric)

-- | List of application runtime thread states used to get the counts for a
-- frame a derive a metric value.
frameMetric_threadStates :: Lens.Lens' FrameMetric [Prelude.Text]
frameMetric_threadStates = Lens.lens (\FrameMetric' {threadStates} -> threadStates) (\s@FrameMetric' {} a -> s {threadStates = a} :: FrameMetric) Prelude.. Lens.coerced

-- | A type of aggregation that specifies how a metric for a frame is
-- analyzed. The supported value @AggregatedRelativeTotalTime@ is an
-- aggregation of the metric value for one frame that is calculated across
-- the occurrences of all frames in a profile.
frameMetric_type :: Lens.Lens' FrameMetric MetricType
frameMetric_type = Lens.lens (\FrameMetric' {type'} -> type') (\s@FrameMetric' {} a -> s {type' = a} :: FrameMetric)

instance Data.FromJSON FrameMetric where
  parseJSON =
    Data.withObject
      "FrameMetric"
      ( \x ->
          FrameMetric'
            Prelude.<$> (x Data..: "frameName")
            Prelude.<*> (x Data..:? "threadStates" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "type")
      )

instance Prelude.Hashable FrameMetric where
  hashWithSalt _salt FrameMetric' {..} =
    _salt `Prelude.hashWithSalt` frameName
      `Prelude.hashWithSalt` threadStates
      `Prelude.hashWithSalt` type'

instance Prelude.NFData FrameMetric where
  rnf FrameMetric' {..} =
    Prelude.rnf frameName
      `Prelude.seq` Prelude.rnf threadStates
      `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON FrameMetric where
  toJSON FrameMetric' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("frameName" Data..= frameName),
            Prelude.Just ("threadStates" Data..= threadStates),
            Prelude.Just ("type" Data..= type')
          ]
      )
