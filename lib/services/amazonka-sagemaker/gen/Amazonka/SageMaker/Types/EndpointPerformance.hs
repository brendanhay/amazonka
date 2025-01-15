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
-- Module      : Amazonka.SageMaker.Types.EndpointPerformance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.EndpointPerformance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.EndpointInfo
import Amazonka.SageMaker.Types.InferenceMetrics

-- | The performance results from running an Inference Recommender job on an
-- existing endpoint.
--
-- /See:/ 'newEndpointPerformance' smart constructor.
data EndpointPerformance = EndpointPerformance'
  { -- | The metrics for an existing endpoint.
    metrics :: InferenceMetrics,
    endpointInfo :: EndpointInfo
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EndpointPerformance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metrics', 'endpointPerformance_metrics' - The metrics for an existing endpoint.
--
-- 'endpointInfo', 'endpointPerformance_endpointInfo' - Undocumented member.
newEndpointPerformance ::
  -- | 'metrics'
  InferenceMetrics ->
  -- | 'endpointInfo'
  EndpointInfo ->
  EndpointPerformance
newEndpointPerformance pMetrics_ pEndpointInfo_ =
  EndpointPerformance'
    { metrics = pMetrics_,
      endpointInfo = pEndpointInfo_
    }

-- | The metrics for an existing endpoint.
endpointPerformance_metrics :: Lens.Lens' EndpointPerformance InferenceMetrics
endpointPerformance_metrics = Lens.lens (\EndpointPerformance' {metrics} -> metrics) (\s@EndpointPerformance' {} a -> s {metrics = a} :: EndpointPerformance)

-- | Undocumented member.
endpointPerformance_endpointInfo :: Lens.Lens' EndpointPerformance EndpointInfo
endpointPerformance_endpointInfo = Lens.lens (\EndpointPerformance' {endpointInfo} -> endpointInfo) (\s@EndpointPerformance' {} a -> s {endpointInfo = a} :: EndpointPerformance)

instance Data.FromJSON EndpointPerformance where
  parseJSON =
    Data.withObject
      "EndpointPerformance"
      ( \x ->
          EndpointPerformance'
            Prelude.<$> (x Data..: "Metrics")
            Prelude.<*> (x Data..: "EndpointInfo")
      )

instance Prelude.Hashable EndpointPerformance where
  hashWithSalt _salt EndpointPerformance' {..} =
    _salt
      `Prelude.hashWithSalt` metrics
      `Prelude.hashWithSalt` endpointInfo

instance Prelude.NFData EndpointPerformance where
  rnf EndpointPerformance' {..} =
    Prelude.rnf metrics `Prelude.seq`
      Prelude.rnf endpointInfo
