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
-- Module      : Network.AWS.XRay.Types.SamplingStatisticSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.SamplingStatisticSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Aggregated request sampling data for a sampling rule across all services
-- for a 10-second window.
--
-- /See:/ 'newSamplingStatisticSummary' smart constructor.
data SamplingStatisticSummary = SamplingStatisticSummary'
  { -- | The name of the sampling rule.
    ruleName :: Core.Maybe Core.Text,
    -- | The number of requests recorded with borrowed reservoir quota.
    borrowCount :: Core.Maybe Core.Int,
    -- | The number of requests that matched the rule.
    requestCount :: Core.Maybe Core.Int,
    -- | The number of requests recorded.
    sampledCount :: Core.Maybe Core.Int,
    -- | The start time of the reporting window.
    timestamp :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SamplingStatisticSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleName', 'samplingStatisticSummary_ruleName' - The name of the sampling rule.
--
-- 'borrowCount', 'samplingStatisticSummary_borrowCount' - The number of requests recorded with borrowed reservoir quota.
--
-- 'requestCount', 'samplingStatisticSummary_requestCount' - The number of requests that matched the rule.
--
-- 'sampledCount', 'samplingStatisticSummary_sampledCount' - The number of requests recorded.
--
-- 'timestamp', 'samplingStatisticSummary_timestamp' - The start time of the reporting window.
newSamplingStatisticSummary ::
  SamplingStatisticSummary
newSamplingStatisticSummary =
  SamplingStatisticSummary'
    { ruleName = Core.Nothing,
      borrowCount = Core.Nothing,
      requestCount = Core.Nothing,
      sampledCount = Core.Nothing,
      timestamp = Core.Nothing
    }

-- | The name of the sampling rule.
samplingStatisticSummary_ruleName :: Lens.Lens' SamplingStatisticSummary (Core.Maybe Core.Text)
samplingStatisticSummary_ruleName = Lens.lens (\SamplingStatisticSummary' {ruleName} -> ruleName) (\s@SamplingStatisticSummary' {} a -> s {ruleName = a} :: SamplingStatisticSummary)

-- | The number of requests recorded with borrowed reservoir quota.
samplingStatisticSummary_borrowCount :: Lens.Lens' SamplingStatisticSummary (Core.Maybe Core.Int)
samplingStatisticSummary_borrowCount = Lens.lens (\SamplingStatisticSummary' {borrowCount} -> borrowCount) (\s@SamplingStatisticSummary' {} a -> s {borrowCount = a} :: SamplingStatisticSummary)

-- | The number of requests that matched the rule.
samplingStatisticSummary_requestCount :: Lens.Lens' SamplingStatisticSummary (Core.Maybe Core.Int)
samplingStatisticSummary_requestCount = Lens.lens (\SamplingStatisticSummary' {requestCount} -> requestCount) (\s@SamplingStatisticSummary' {} a -> s {requestCount = a} :: SamplingStatisticSummary)

-- | The number of requests recorded.
samplingStatisticSummary_sampledCount :: Lens.Lens' SamplingStatisticSummary (Core.Maybe Core.Int)
samplingStatisticSummary_sampledCount = Lens.lens (\SamplingStatisticSummary' {sampledCount} -> sampledCount) (\s@SamplingStatisticSummary' {} a -> s {sampledCount = a} :: SamplingStatisticSummary)

-- | The start time of the reporting window.
samplingStatisticSummary_timestamp :: Lens.Lens' SamplingStatisticSummary (Core.Maybe Core.UTCTime)
samplingStatisticSummary_timestamp = Lens.lens (\SamplingStatisticSummary' {timestamp} -> timestamp) (\s@SamplingStatisticSummary' {} a -> s {timestamp = a} :: SamplingStatisticSummary) Core.. Lens.mapping Core._Time

instance Core.FromJSON SamplingStatisticSummary where
  parseJSON =
    Core.withObject
      "SamplingStatisticSummary"
      ( \x ->
          SamplingStatisticSummary'
            Core.<$> (x Core..:? "RuleName")
            Core.<*> (x Core..:? "BorrowCount")
            Core.<*> (x Core..:? "RequestCount")
            Core.<*> (x Core..:? "SampledCount")
            Core.<*> (x Core..:? "Timestamp")
      )

instance Core.Hashable SamplingStatisticSummary

instance Core.NFData SamplingStatisticSummary
