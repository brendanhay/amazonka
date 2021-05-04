{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.XRay.Types.EdgeStatistics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.EdgeStatistics where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.XRay.Types.ErrorStatistics
import Network.AWS.XRay.Types.FaultStatistics

-- | Response statistics for an edge.
--
-- /See:/ 'newEdgeStatistics' smart constructor.
data EdgeStatistics = EdgeStatistics'
  { -- | The aggregate response time of completed requests.
    totalResponseTime :: Prelude.Maybe Prelude.Double,
    -- | The number of requests that completed with a 2xx Success status code.
    okCount :: Prelude.Maybe Prelude.Integer,
    -- | Information about requests that failed with a 5xx Server Error status
    -- code.
    faultStatistics :: Prelude.Maybe FaultStatistics,
    -- | The total number of completed requests.
    totalCount :: Prelude.Maybe Prelude.Integer,
    -- | Information about requests that failed with a 4xx Client Error status
    -- code.
    errorStatistics :: Prelude.Maybe ErrorStatistics
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EdgeStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'totalResponseTime', 'edgeStatistics_totalResponseTime' - The aggregate response time of completed requests.
--
-- 'okCount', 'edgeStatistics_okCount' - The number of requests that completed with a 2xx Success status code.
--
-- 'faultStatistics', 'edgeStatistics_faultStatistics' - Information about requests that failed with a 5xx Server Error status
-- code.
--
-- 'totalCount', 'edgeStatistics_totalCount' - The total number of completed requests.
--
-- 'errorStatistics', 'edgeStatistics_errorStatistics' - Information about requests that failed with a 4xx Client Error status
-- code.
newEdgeStatistics ::
  EdgeStatistics
newEdgeStatistics =
  EdgeStatistics'
    { totalResponseTime =
        Prelude.Nothing,
      okCount = Prelude.Nothing,
      faultStatistics = Prelude.Nothing,
      totalCount = Prelude.Nothing,
      errorStatistics = Prelude.Nothing
    }

-- | The aggregate response time of completed requests.
edgeStatistics_totalResponseTime :: Lens.Lens' EdgeStatistics (Prelude.Maybe Prelude.Double)
edgeStatistics_totalResponseTime = Lens.lens (\EdgeStatistics' {totalResponseTime} -> totalResponseTime) (\s@EdgeStatistics' {} a -> s {totalResponseTime = a} :: EdgeStatistics)

-- | The number of requests that completed with a 2xx Success status code.
edgeStatistics_okCount :: Lens.Lens' EdgeStatistics (Prelude.Maybe Prelude.Integer)
edgeStatistics_okCount = Lens.lens (\EdgeStatistics' {okCount} -> okCount) (\s@EdgeStatistics' {} a -> s {okCount = a} :: EdgeStatistics)

-- | Information about requests that failed with a 5xx Server Error status
-- code.
edgeStatistics_faultStatistics :: Lens.Lens' EdgeStatistics (Prelude.Maybe FaultStatistics)
edgeStatistics_faultStatistics = Lens.lens (\EdgeStatistics' {faultStatistics} -> faultStatistics) (\s@EdgeStatistics' {} a -> s {faultStatistics = a} :: EdgeStatistics)

-- | The total number of completed requests.
edgeStatistics_totalCount :: Lens.Lens' EdgeStatistics (Prelude.Maybe Prelude.Integer)
edgeStatistics_totalCount = Lens.lens (\EdgeStatistics' {totalCount} -> totalCount) (\s@EdgeStatistics' {} a -> s {totalCount = a} :: EdgeStatistics)

-- | Information about requests that failed with a 4xx Client Error status
-- code.
edgeStatistics_errorStatistics :: Lens.Lens' EdgeStatistics (Prelude.Maybe ErrorStatistics)
edgeStatistics_errorStatistics = Lens.lens (\EdgeStatistics' {errorStatistics} -> errorStatistics) (\s@EdgeStatistics' {} a -> s {errorStatistics = a} :: EdgeStatistics)

instance Prelude.FromJSON EdgeStatistics where
  parseJSON =
    Prelude.withObject
      "EdgeStatistics"
      ( \x ->
          EdgeStatistics'
            Prelude.<$> (x Prelude..:? "TotalResponseTime")
            Prelude.<*> (x Prelude..:? "OkCount")
            Prelude.<*> (x Prelude..:? "FaultStatistics")
            Prelude.<*> (x Prelude..:? "TotalCount")
            Prelude.<*> (x Prelude..:? "ErrorStatistics")
      )

instance Prelude.Hashable EdgeStatistics

instance Prelude.NFData EdgeStatistics
