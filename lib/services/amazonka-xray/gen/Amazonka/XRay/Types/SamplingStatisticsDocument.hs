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
-- Module      : Amazonka.XRay.Types.SamplingStatisticsDocument
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types.SamplingStatisticsDocument where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Request sampling results for a single rule from a service. Results are
-- for the last 10 seconds unless the service has been assigned a longer
-- reporting interval after a previous call to
-- <https://docs.aws.amazon.com/xray/latest/api/API_GetSamplingTargets.html GetSamplingTargets>.
--
-- /See:/ 'newSamplingStatisticsDocument' smart constructor.
data SamplingStatisticsDocument = SamplingStatisticsDocument'
  { -- | The number of requests recorded with borrowed reservoir quota.
    borrowCount :: Prelude.Maybe Prelude.Natural,
    -- | The name of the sampling rule.
    ruleName :: Prelude.Text,
    -- | A unique identifier for the service in hexadecimal.
    clientID :: Prelude.Text,
    -- | The current time.
    timestamp :: Core.POSIX,
    -- | The number of requests that matched the rule.
    requestCount :: Prelude.Natural,
    -- | The number of requests recorded.
    sampledCount :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SamplingStatisticsDocument' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'borrowCount', 'samplingStatisticsDocument_borrowCount' - The number of requests recorded with borrowed reservoir quota.
--
-- 'ruleName', 'samplingStatisticsDocument_ruleName' - The name of the sampling rule.
--
-- 'clientID', 'samplingStatisticsDocument_clientID' - A unique identifier for the service in hexadecimal.
--
-- 'timestamp', 'samplingStatisticsDocument_timestamp' - The current time.
--
-- 'requestCount', 'samplingStatisticsDocument_requestCount' - The number of requests that matched the rule.
--
-- 'sampledCount', 'samplingStatisticsDocument_sampledCount' - The number of requests recorded.
newSamplingStatisticsDocument ::
  -- | 'ruleName'
  Prelude.Text ->
  -- | 'clientID'
  Prelude.Text ->
  -- | 'timestamp'
  Prelude.UTCTime ->
  -- | 'requestCount'
  Prelude.Natural ->
  -- | 'sampledCount'
  Prelude.Natural ->
  SamplingStatisticsDocument
newSamplingStatisticsDocument
  pRuleName_
  pClientID_
  pTimestamp_
  pRequestCount_
  pSampledCount_ =
    SamplingStatisticsDocument'
      { borrowCount =
          Prelude.Nothing,
        ruleName = pRuleName_,
        clientID = pClientID_,
        timestamp = Core._Time Lens.# pTimestamp_,
        requestCount = pRequestCount_,
        sampledCount = pSampledCount_
      }

-- | The number of requests recorded with borrowed reservoir quota.
samplingStatisticsDocument_borrowCount :: Lens.Lens' SamplingStatisticsDocument (Prelude.Maybe Prelude.Natural)
samplingStatisticsDocument_borrowCount = Lens.lens (\SamplingStatisticsDocument' {borrowCount} -> borrowCount) (\s@SamplingStatisticsDocument' {} a -> s {borrowCount = a} :: SamplingStatisticsDocument)

-- | The name of the sampling rule.
samplingStatisticsDocument_ruleName :: Lens.Lens' SamplingStatisticsDocument Prelude.Text
samplingStatisticsDocument_ruleName = Lens.lens (\SamplingStatisticsDocument' {ruleName} -> ruleName) (\s@SamplingStatisticsDocument' {} a -> s {ruleName = a} :: SamplingStatisticsDocument)

-- | A unique identifier for the service in hexadecimal.
samplingStatisticsDocument_clientID :: Lens.Lens' SamplingStatisticsDocument Prelude.Text
samplingStatisticsDocument_clientID = Lens.lens (\SamplingStatisticsDocument' {clientID} -> clientID) (\s@SamplingStatisticsDocument' {} a -> s {clientID = a} :: SamplingStatisticsDocument)

-- | The current time.
samplingStatisticsDocument_timestamp :: Lens.Lens' SamplingStatisticsDocument Prelude.UTCTime
samplingStatisticsDocument_timestamp = Lens.lens (\SamplingStatisticsDocument' {timestamp} -> timestamp) (\s@SamplingStatisticsDocument' {} a -> s {timestamp = a} :: SamplingStatisticsDocument) Prelude.. Core._Time

-- | The number of requests that matched the rule.
samplingStatisticsDocument_requestCount :: Lens.Lens' SamplingStatisticsDocument Prelude.Natural
samplingStatisticsDocument_requestCount = Lens.lens (\SamplingStatisticsDocument' {requestCount} -> requestCount) (\s@SamplingStatisticsDocument' {} a -> s {requestCount = a} :: SamplingStatisticsDocument)

-- | The number of requests recorded.
samplingStatisticsDocument_sampledCount :: Lens.Lens' SamplingStatisticsDocument Prelude.Natural
samplingStatisticsDocument_sampledCount = Lens.lens (\SamplingStatisticsDocument' {sampledCount} -> sampledCount) (\s@SamplingStatisticsDocument' {} a -> s {sampledCount = a} :: SamplingStatisticsDocument)

instance Prelude.Hashable SamplingStatisticsDocument where
  hashWithSalt _salt SamplingStatisticsDocument' {..} =
    _salt `Prelude.hashWithSalt` borrowCount
      `Prelude.hashWithSalt` ruleName
      `Prelude.hashWithSalt` clientID
      `Prelude.hashWithSalt` timestamp
      `Prelude.hashWithSalt` requestCount
      `Prelude.hashWithSalt` sampledCount

instance Prelude.NFData SamplingStatisticsDocument where
  rnf SamplingStatisticsDocument' {..} =
    Prelude.rnf borrowCount
      `Prelude.seq` Prelude.rnf ruleName
      `Prelude.seq` Prelude.rnf clientID
      `Prelude.seq` Prelude.rnf timestamp
      `Prelude.seq` Prelude.rnf requestCount
      `Prelude.seq` Prelude.rnf sampledCount

instance Core.ToJSON SamplingStatisticsDocument where
  toJSON SamplingStatisticsDocument' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("BorrowCount" Core..=) Prelude.<$> borrowCount,
            Prelude.Just ("RuleName" Core..= ruleName),
            Prelude.Just ("ClientID" Core..= clientID),
            Prelude.Just ("Timestamp" Core..= timestamp),
            Prelude.Just ("RequestCount" Core..= requestCount),
            Prelude.Just ("SampledCount" Core..= sampledCount)
          ]
      )
