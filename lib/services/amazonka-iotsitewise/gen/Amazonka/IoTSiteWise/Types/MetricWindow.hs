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
-- Module      : Amazonka.IoTSiteWise.Types.MetricWindow
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.MetricWindow where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types.TumblingWindow
import qualified Amazonka.Prelude as Prelude

-- | Contains a time interval window used for data aggregate computations
-- (for example, average, sum, count, and so on).
--
-- /See:/ 'newMetricWindow' smart constructor.
data MetricWindow = MetricWindow'
  { -- | The tumbling time interval window.
    tumbling :: Prelude.Maybe TumblingWindow
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricWindow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tumbling', 'metricWindow_tumbling' - The tumbling time interval window.
newMetricWindow ::
  MetricWindow
newMetricWindow =
  MetricWindow' {tumbling = Prelude.Nothing}

-- | The tumbling time interval window.
metricWindow_tumbling :: Lens.Lens' MetricWindow (Prelude.Maybe TumblingWindow)
metricWindow_tumbling = Lens.lens (\MetricWindow' {tumbling} -> tumbling) (\s@MetricWindow' {} a -> s {tumbling = a} :: MetricWindow)

instance Data.FromJSON MetricWindow where
  parseJSON =
    Data.withObject
      "MetricWindow"
      ( \x ->
          MetricWindow' Prelude.<$> (x Data..:? "tumbling")
      )

instance Prelude.Hashable MetricWindow where
  hashWithSalt _salt MetricWindow' {..} =
    _salt `Prelude.hashWithSalt` tumbling

instance Prelude.NFData MetricWindow where
  rnf MetricWindow' {..} = Prelude.rnf tumbling

instance Data.ToJSON MetricWindow where
  toJSON MetricWindow' {..} =
    Data.object
      ( Prelude.catMaybes
          [("tumbling" Data..=) Prelude.<$> tumbling]
      )
