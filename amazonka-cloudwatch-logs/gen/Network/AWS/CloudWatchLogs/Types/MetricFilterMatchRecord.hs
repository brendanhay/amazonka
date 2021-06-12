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
-- Module      : Network.AWS.CloudWatchLogs.Types.MetricFilterMatchRecord
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.MetricFilterMatchRecord where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents a matched event.
--
-- /See:/ 'newMetricFilterMatchRecord' smart constructor.
data MetricFilterMatchRecord = MetricFilterMatchRecord'
  { -- | The event number.
    eventNumber :: Core.Maybe Core.Integer,
    -- | The raw event data.
    eventMessage :: Core.Maybe Core.Text,
    -- | The values extracted from the event data by the filter.
    extractedValues :: Core.Maybe (Core.HashMap Core.Text Core.Text)
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MetricFilterMatchRecord' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventNumber', 'metricFilterMatchRecord_eventNumber' - The event number.
--
-- 'eventMessage', 'metricFilterMatchRecord_eventMessage' - The raw event data.
--
-- 'extractedValues', 'metricFilterMatchRecord_extractedValues' - The values extracted from the event data by the filter.
newMetricFilterMatchRecord ::
  MetricFilterMatchRecord
newMetricFilterMatchRecord =
  MetricFilterMatchRecord'
    { eventNumber =
        Core.Nothing,
      eventMessage = Core.Nothing,
      extractedValues = Core.Nothing
    }

-- | The event number.
metricFilterMatchRecord_eventNumber :: Lens.Lens' MetricFilterMatchRecord (Core.Maybe Core.Integer)
metricFilterMatchRecord_eventNumber = Lens.lens (\MetricFilterMatchRecord' {eventNumber} -> eventNumber) (\s@MetricFilterMatchRecord' {} a -> s {eventNumber = a} :: MetricFilterMatchRecord)

-- | The raw event data.
metricFilterMatchRecord_eventMessage :: Lens.Lens' MetricFilterMatchRecord (Core.Maybe Core.Text)
metricFilterMatchRecord_eventMessage = Lens.lens (\MetricFilterMatchRecord' {eventMessage} -> eventMessage) (\s@MetricFilterMatchRecord' {} a -> s {eventMessage = a} :: MetricFilterMatchRecord)

-- | The values extracted from the event data by the filter.
metricFilterMatchRecord_extractedValues :: Lens.Lens' MetricFilterMatchRecord (Core.Maybe (Core.HashMap Core.Text Core.Text))
metricFilterMatchRecord_extractedValues = Lens.lens (\MetricFilterMatchRecord' {extractedValues} -> extractedValues) (\s@MetricFilterMatchRecord' {} a -> s {extractedValues = a} :: MetricFilterMatchRecord) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON MetricFilterMatchRecord where
  parseJSON =
    Core.withObject
      "MetricFilterMatchRecord"
      ( \x ->
          MetricFilterMatchRecord'
            Core.<$> (x Core..:? "eventNumber")
            Core.<*> (x Core..:? "eventMessage")
            Core.<*> (x Core..:? "extractedValues" Core..!= Core.mempty)
      )

instance Core.Hashable MetricFilterMatchRecord

instance Core.NFData MetricFilterMatchRecord
