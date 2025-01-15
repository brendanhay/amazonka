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
-- Module      : Amazonka.XRay.Types.SamplingStatisticSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types.SamplingStatisticSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Aggregated request sampling data for a sampling rule across all services
-- for a 10-second window.
--
-- /See:/ 'newSamplingStatisticSummary' smart constructor.
data SamplingStatisticSummary = SamplingStatisticSummary'
  { -- | The number of requests recorded with borrowed reservoir quota.
    borrowCount :: Prelude.Maybe Prelude.Int,
    -- | The number of requests that matched the rule.
    requestCount :: Prelude.Maybe Prelude.Int,
    -- | The name of the sampling rule.
    ruleName :: Prelude.Maybe Prelude.Text,
    -- | The number of requests recorded.
    sampledCount :: Prelude.Maybe Prelude.Int,
    -- | The start time of the reporting window.
    timestamp :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SamplingStatisticSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'borrowCount', 'samplingStatisticSummary_borrowCount' - The number of requests recorded with borrowed reservoir quota.
--
-- 'requestCount', 'samplingStatisticSummary_requestCount' - The number of requests that matched the rule.
--
-- 'ruleName', 'samplingStatisticSummary_ruleName' - The name of the sampling rule.
--
-- 'sampledCount', 'samplingStatisticSummary_sampledCount' - The number of requests recorded.
--
-- 'timestamp', 'samplingStatisticSummary_timestamp' - The start time of the reporting window.
newSamplingStatisticSummary ::
  SamplingStatisticSummary
newSamplingStatisticSummary =
  SamplingStatisticSummary'
    { borrowCount =
        Prelude.Nothing,
      requestCount = Prelude.Nothing,
      ruleName = Prelude.Nothing,
      sampledCount = Prelude.Nothing,
      timestamp = Prelude.Nothing
    }

-- | The number of requests recorded with borrowed reservoir quota.
samplingStatisticSummary_borrowCount :: Lens.Lens' SamplingStatisticSummary (Prelude.Maybe Prelude.Int)
samplingStatisticSummary_borrowCount = Lens.lens (\SamplingStatisticSummary' {borrowCount} -> borrowCount) (\s@SamplingStatisticSummary' {} a -> s {borrowCount = a} :: SamplingStatisticSummary)

-- | The number of requests that matched the rule.
samplingStatisticSummary_requestCount :: Lens.Lens' SamplingStatisticSummary (Prelude.Maybe Prelude.Int)
samplingStatisticSummary_requestCount = Lens.lens (\SamplingStatisticSummary' {requestCount} -> requestCount) (\s@SamplingStatisticSummary' {} a -> s {requestCount = a} :: SamplingStatisticSummary)

-- | The name of the sampling rule.
samplingStatisticSummary_ruleName :: Lens.Lens' SamplingStatisticSummary (Prelude.Maybe Prelude.Text)
samplingStatisticSummary_ruleName = Lens.lens (\SamplingStatisticSummary' {ruleName} -> ruleName) (\s@SamplingStatisticSummary' {} a -> s {ruleName = a} :: SamplingStatisticSummary)

-- | The number of requests recorded.
samplingStatisticSummary_sampledCount :: Lens.Lens' SamplingStatisticSummary (Prelude.Maybe Prelude.Int)
samplingStatisticSummary_sampledCount = Lens.lens (\SamplingStatisticSummary' {sampledCount} -> sampledCount) (\s@SamplingStatisticSummary' {} a -> s {sampledCount = a} :: SamplingStatisticSummary)

-- | The start time of the reporting window.
samplingStatisticSummary_timestamp :: Lens.Lens' SamplingStatisticSummary (Prelude.Maybe Prelude.UTCTime)
samplingStatisticSummary_timestamp = Lens.lens (\SamplingStatisticSummary' {timestamp} -> timestamp) (\s@SamplingStatisticSummary' {} a -> s {timestamp = a} :: SamplingStatisticSummary) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON SamplingStatisticSummary where
  parseJSON =
    Data.withObject
      "SamplingStatisticSummary"
      ( \x ->
          SamplingStatisticSummary'
            Prelude.<$> (x Data..:? "BorrowCount")
            Prelude.<*> (x Data..:? "RequestCount")
            Prelude.<*> (x Data..:? "RuleName")
            Prelude.<*> (x Data..:? "SampledCount")
            Prelude.<*> (x Data..:? "Timestamp")
      )

instance Prelude.Hashable SamplingStatisticSummary where
  hashWithSalt _salt SamplingStatisticSummary' {..} =
    _salt
      `Prelude.hashWithSalt` borrowCount
      `Prelude.hashWithSalt` requestCount
      `Prelude.hashWithSalt` ruleName
      `Prelude.hashWithSalt` sampledCount
      `Prelude.hashWithSalt` timestamp

instance Prelude.NFData SamplingStatisticSummary where
  rnf SamplingStatisticSummary' {..} =
    Prelude.rnf borrowCount `Prelude.seq`
      Prelude.rnf requestCount `Prelude.seq`
        Prelude.rnf ruleName `Prelude.seq`
          Prelude.rnf sampledCount `Prelude.seq`
            Prelude.rnf timestamp
