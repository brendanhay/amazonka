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
-- Module      : Network.AWS.XRay.Types.SamplingStatisticSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.SamplingStatisticSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Aggregated request sampling data for a sampling rule across all services
-- for a 10-second window.
--
-- /See:/ 'newSamplingStatisticSummary' smart constructor.
data SamplingStatisticSummary = SamplingStatisticSummary'
  { -- | The name of the sampling rule.
    ruleName :: Prelude.Maybe Prelude.Text,
    -- | The number of requests recorded with borrowed reservoir quota.
    borrowCount :: Prelude.Maybe Prelude.Int,
    -- | The number of requests that matched the rule.
    requestCount :: Prelude.Maybe Prelude.Int,
    -- | The number of requests recorded.
    sampledCount :: Prelude.Maybe Prelude.Int,
    -- | The start time of the reporting window.
    timestamp :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { ruleName =
        Prelude.Nothing,
      borrowCount = Prelude.Nothing,
      requestCount = Prelude.Nothing,
      sampledCount = Prelude.Nothing,
      timestamp = Prelude.Nothing
    }

-- | The name of the sampling rule.
samplingStatisticSummary_ruleName :: Lens.Lens' SamplingStatisticSummary (Prelude.Maybe Prelude.Text)
samplingStatisticSummary_ruleName = Lens.lens (\SamplingStatisticSummary' {ruleName} -> ruleName) (\s@SamplingStatisticSummary' {} a -> s {ruleName = a} :: SamplingStatisticSummary)

-- | The number of requests recorded with borrowed reservoir quota.
samplingStatisticSummary_borrowCount :: Lens.Lens' SamplingStatisticSummary (Prelude.Maybe Prelude.Int)
samplingStatisticSummary_borrowCount = Lens.lens (\SamplingStatisticSummary' {borrowCount} -> borrowCount) (\s@SamplingStatisticSummary' {} a -> s {borrowCount = a} :: SamplingStatisticSummary)

-- | The number of requests that matched the rule.
samplingStatisticSummary_requestCount :: Lens.Lens' SamplingStatisticSummary (Prelude.Maybe Prelude.Int)
samplingStatisticSummary_requestCount = Lens.lens (\SamplingStatisticSummary' {requestCount} -> requestCount) (\s@SamplingStatisticSummary' {} a -> s {requestCount = a} :: SamplingStatisticSummary)

-- | The number of requests recorded.
samplingStatisticSummary_sampledCount :: Lens.Lens' SamplingStatisticSummary (Prelude.Maybe Prelude.Int)
samplingStatisticSummary_sampledCount = Lens.lens (\SamplingStatisticSummary' {sampledCount} -> sampledCount) (\s@SamplingStatisticSummary' {} a -> s {sampledCount = a} :: SamplingStatisticSummary)

-- | The start time of the reporting window.
samplingStatisticSummary_timestamp :: Lens.Lens' SamplingStatisticSummary (Prelude.Maybe Prelude.UTCTime)
samplingStatisticSummary_timestamp = Lens.lens (\SamplingStatisticSummary' {timestamp} -> timestamp) (\s@SamplingStatisticSummary' {} a -> s {timestamp = a} :: SamplingStatisticSummary) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON SamplingStatisticSummary where
  parseJSON =
    Prelude.withObject
      "SamplingStatisticSummary"
      ( \x ->
          SamplingStatisticSummary'
            Prelude.<$> (x Prelude..:? "RuleName")
            Prelude.<*> (x Prelude..:? "BorrowCount")
            Prelude.<*> (x Prelude..:? "RequestCount")
            Prelude.<*> (x Prelude..:? "SampledCount")
            Prelude.<*> (x Prelude..:? "Timestamp")
      )

instance Prelude.Hashable SamplingStatisticSummary

instance Prelude.NFData SamplingStatisticSummary
