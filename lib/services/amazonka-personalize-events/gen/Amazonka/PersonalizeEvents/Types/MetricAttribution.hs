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
-- Module      : Amazonka.PersonalizeEvents.Types.MetricAttribution
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PersonalizeEvents.Types.MetricAttribution where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a metric attribution associated with an
-- event. For more information about metric attributions, see
-- <https://docs.aws.amazon.com/personalize/latest/dg/measuring-recommendation-impact.html Measuring impact of recommendations>.
--
-- /See:/ 'newMetricAttribution' smart constructor.
data MetricAttribution = MetricAttribution'
  { -- | The source of the event, such as a third party.
    eventAttributionSource :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricAttribution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventAttributionSource', 'metricAttribution_eventAttributionSource' - The source of the event, such as a third party.
newMetricAttribution ::
  -- | 'eventAttributionSource'
  Prelude.Text ->
  MetricAttribution
newMetricAttribution pEventAttributionSource_ =
  MetricAttribution'
    { eventAttributionSource =
        pEventAttributionSource_
    }

-- | The source of the event, such as a third party.
metricAttribution_eventAttributionSource :: Lens.Lens' MetricAttribution Prelude.Text
metricAttribution_eventAttributionSource = Lens.lens (\MetricAttribution' {eventAttributionSource} -> eventAttributionSource) (\s@MetricAttribution' {} a -> s {eventAttributionSource = a} :: MetricAttribution)

instance Prelude.Hashable MetricAttribution where
  hashWithSalt _salt MetricAttribution' {..} =
    _salt `Prelude.hashWithSalt` eventAttributionSource

instance Prelude.NFData MetricAttribution where
  rnf MetricAttribution' {..} =
    Prelude.rnf eventAttributionSource

instance Data.ToJSON MetricAttribution where
  toJSON MetricAttribution' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "eventAttributionSource"
                  Data..= eventAttributionSource
              )
          ]
      )
