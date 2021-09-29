{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.GetSamplingTargets
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests a sampling quota for rules that the service is using to sample
-- requests.
module Network.AWS.XRay.GetSamplingTargets
  ( -- * Creating a Request
    GetSamplingTargets (..),
    newGetSamplingTargets,

    -- * Request Lenses
    getSamplingTargets_samplingStatisticsDocuments,

    -- * Destructuring the Response
    GetSamplingTargetsResponse (..),
    newGetSamplingTargetsResponse,

    -- * Response Lenses
    getSamplingTargetsResponse_samplingTargetDocuments,
    getSamplingTargetsResponse_lastRuleModification,
    getSamplingTargetsResponse_unprocessedStatistics,
    getSamplingTargetsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.XRay.Types

-- | /See:/ 'newGetSamplingTargets' smart constructor.
data GetSamplingTargets = GetSamplingTargets'
  { -- | Information about rules that the service is using to sample requests.
    samplingStatisticsDocuments :: [SamplingStatisticsDocument]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSamplingTargets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'samplingStatisticsDocuments', 'getSamplingTargets_samplingStatisticsDocuments' - Information about rules that the service is using to sample requests.
newGetSamplingTargets ::
  GetSamplingTargets
newGetSamplingTargets =
  GetSamplingTargets'
    { samplingStatisticsDocuments =
        Prelude.mempty
    }

-- | Information about rules that the service is using to sample requests.
getSamplingTargets_samplingStatisticsDocuments :: Lens.Lens' GetSamplingTargets [SamplingStatisticsDocument]
getSamplingTargets_samplingStatisticsDocuments = Lens.lens (\GetSamplingTargets' {samplingStatisticsDocuments} -> samplingStatisticsDocuments) (\s@GetSamplingTargets' {} a -> s {samplingStatisticsDocuments = a} :: GetSamplingTargets) Prelude.. Lens._Coerce

instance Core.AWSRequest GetSamplingTargets where
  type
    AWSResponse GetSamplingTargets =
      GetSamplingTargetsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSamplingTargetsResponse'
            Prelude.<$> ( x Core..?> "SamplingTargetDocuments"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "LastRuleModification")
            Prelude.<*> ( x Core..?> "UnprocessedStatistics"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSamplingTargets

instance Prelude.NFData GetSamplingTargets

instance Core.ToHeaders GetSamplingTargets where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON GetSamplingTargets where
  toJSON GetSamplingTargets' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "SamplingStatisticsDocuments"
                  Core..= samplingStatisticsDocuments
              )
          ]
      )

instance Core.ToPath GetSamplingTargets where
  toPath = Prelude.const "/SamplingTargets"

instance Core.ToQuery GetSamplingTargets where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSamplingTargetsResponse' smart constructor.
data GetSamplingTargetsResponse = GetSamplingTargetsResponse'
  { -- | Updated rules that the service should use to sample requests.
    samplingTargetDocuments :: Prelude.Maybe [SamplingTargetDocument],
    -- | The last time a user changed the sampling rule configuration. If the
    -- sampling rule configuration changed since the service last retrieved it,
    -- the service should call
    -- <https://docs.aws.amazon.com/xray/latest/api/API_GetSamplingRules.html GetSamplingRules>
    -- to get the latest version.
    lastRuleModification :: Prelude.Maybe Core.POSIX,
    -- | Information about
    -- <https://docs.aws.amazon.com/xray/latest/api/API_SamplingStatisticsDocument.html SamplingStatisticsDocument>
    -- that X-Ray could not process.
    unprocessedStatistics :: Prelude.Maybe [UnprocessedStatistics],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSamplingTargetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'samplingTargetDocuments', 'getSamplingTargetsResponse_samplingTargetDocuments' - Updated rules that the service should use to sample requests.
--
-- 'lastRuleModification', 'getSamplingTargetsResponse_lastRuleModification' - The last time a user changed the sampling rule configuration. If the
-- sampling rule configuration changed since the service last retrieved it,
-- the service should call
-- <https://docs.aws.amazon.com/xray/latest/api/API_GetSamplingRules.html GetSamplingRules>
-- to get the latest version.
--
-- 'unprocessedStatistics', 'getSamplingTargetsResponse_unprocessedStatistics' - Information about
-- <https://docs.aws.amazon.com/xray/latest/api/API_SamplingStatisticsDocument.html SamplingStatisticsDocument>
-- that X-Ray could not process.
--
-- 'httpStatus', 'getSamplingTargetsResponse_httpStatus' - The response's http status code.
newGetSamplingTargetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSamplingTargetsResponse
newGetSamplingTargetsResponse pHttpStatus_ =
  GetSamplingTargetsResponse'
    { samplingTargetDocuments =
        Prelude.Nothing,
      lastRuleModification = Prelude.Nothing,
      unprocessedStatistics = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Updated rules that the service should use to sample requests.
getSamplingTargetsResponse_samplingTargetDocuments :: Lens.Lens' GetSamplingTargetsResponse (Prelude.Maybe [SamplingTargetDocument])
getSamplingTargetsResponse_samplingTargetDocuments = Lens.lens (\GetSamplingTargetsResponse' {samplingTargetDocuments} -> samplingTargetDocuments) (\s@GetSamplingTargetsResponse' {} a -> s {samplingTargetDocuments = a} :: GetSamplingTargetsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The last time a user changed the sampling rule configuration. If the
-- sampling rule configuration changed since the service last retrieved it,
-- the service should call
-- <https://docs.aws.amazon.com/xray/latest/api/API_GetSamplingRules.html GetSamplingRules>
-- to get the latest version.
getSamplingTargetsResponse_lastRuleModification :: Lens.Lens' GetSamplingTargetsResponse (Prelude.Maybe Prelude.UTCTime)
getSamplingTargetsResponse_lastRuleModification = Lens.lens (\GetSamplingTargetsResponse' {lastRuleModification} -> lastRuleModification) (\s@GetSamplingTargetsResponse' {} a -> s {lastRuleModification = a} :: GetSamplingTargetsResponse) Prelude.. Lens.mapping Core._Time

-- | Information about
-- <https://docs.aws.amazon.com/xray/latest/api/API_SamplingStatisticsDocument.html SamplingStatisticsDocument>
-- that X-Ray could not process.
getSamplingTargetsResponse_unprocessedStatistics :: Lens.Lens' GetSamplingTargetsResponse (Prelude.Maybe [UnprocessedStatistics])
getSamplingTargetsResponse_unprocessedStatistics = Lens.lens (\GetSamplingTargetsResponse' {unprocessedStatistics} -> unprocessedStatistics) (\s@GetSamplingTargetsResponse' {} a -> s {unprocessedStatistics = a} :: GetSamplingTargetsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getSamplingTargetsResponse_httpStatus :: Lens.Lens' GetSamplingTargetsResponse Prelude.Int
getSamplingTargetsResponse_httpStatus = Lens.lens (\GetSamplingTargetsResponse' {httpStatus} -> httpStatus) (\s@GetSamplingTargetsResponse' {} a -> s {httpStatus = a} :: GetSamplingTargetsResponse)

instance Prelude.NFData GetSamplingTargetsResponse
