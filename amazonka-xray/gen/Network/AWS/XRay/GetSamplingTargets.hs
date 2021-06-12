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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.XRay.Types

-- | /See:/ 'newGetSamplingTargets' smart constructor.
data GetSamplingTargets = GetSamplingTargets'
  { -- | Information about rules that the service is using to sample requests.
    samplingStatisticsDocuments :: [SamplingStatisticsDocument]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.mempty
    }

-- | Information about rules that the service is using to sample requests.
getSamplingTargets_samplingStatisticsDocuments :: Lens.Lens' GetSamplingTargets [SamplingStatisticsDocument]
getSamplingTargets_samplingStatisticsDocuments = Lens.lens (\GetSamplingTargets' {samplingStatisticsDocuments} -> samplingStatisticsDocuments) (\s@GetSamplingTargets' {} a -> s {samplingStatisticsDocuments = a} :: GetSamplingTargets) Core.. Lens._Coerce

instance Core.AWSRequest GetSamplingTargets where
  type
    AWSResponse GetSamplingTargets =
      GetSamplingTargetsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSamplingTargetsResponse'
            Core.<$> ( x Core..?> "SamplingTargetDocuments"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "LastRuleModification")
            Core.<*> ( x Core..?> "UnprocessedStatistics"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetSamplingTargets

instance Core.NFData GetSamplingTargets

instance Core.ToHeaders GetSamplingTargets where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON GetSamplingTargets where
  toJSON GetSamplingTargets' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "SamplingStatisticsDocuments"
                  Core..= samplingStatisticsDocuments
              )
          ]
      )

instance Core.ToPath GetSamplingTargets where
  toPath = Core.const "/SamplingTargets"

instance Core.ToQuery GetSamplingTargets where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetSamplingTargetsResponse' smart constructor.
data GetSamplingTargetsResponse = GetSamplingTargetsResponse'
  { -- | Updated rules that the service should use to sample requests.
    samplingTargetDocuments :: Core.Maybe [SamplingTargetDocument],
    -- | The last time a user changed the sampling rule configuration. If the
    -- sampling rule configuration changed since the service last retrieved it,
    -- the service should call GetSamplingRules to get the latest version.
    lastRuleModification :: Core.Maybe Core.POSIX,
    -- | Information about SamplingStatisticsDocument that X-Ray could not
    -- process.
    unprocessedStatistics :: Core.Maybe [UnprocessedStatistics],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- the service should call GetSamplingRules to get the latest version.
--
-- 'unprocessedStatistics', 'getSamplingTargetsResponse_unprocessedStatistics' - Information about SamplingStatisticsDocument that X-Ray could not
-- process.
--
-- 'httpStatus', 'getSamplingTargetsResponse_httpStatus' - The response's http status code.
newGetSamplingTargetsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetSamplingTargetsResponse
newGetSamplingTargetsResponse pHttpStatus_ =
  GetSamplingTargetsResponse'
    { samplingTargetDocuments =
        Core.Nothing,
      lastRuleModification = Core.Nothing,
      unprocessedStatistics = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Updated rules that the service should use to sample requests.
getSamplingTargetsResponse_samplingTargetDocuments :: Lens.Lens' GetSamplingTargetsResponse (Core.Maybe [SamplingTargetDocument])
getSamplingTargetsResponse_samplingTargetDocuments = Lens.lens (\GetSamplingTargetsResponse' {samplingTargetDocuments} -> samplingTargetDocuments) (\s@GetSamplingTargetsResponse' {} a -> s {samplingTargetDocuments = a} :: GetSamplingTargetsResponse) Core.. Lens.mapping Lens._Coerce

-- | The last time a user changed the sampling rule configuration. If the
-- sampling rule configuration changed since the service last retrieved it,
-- the service should call GetSamplingRules to get the latest version.
getSamplingTargetsResponse_lastRuleModification :: Lens.Lens' GetSamplingTargetsResponse (Core.Maybe Core.UTCTime)
getSamplingTargetsResponse_lastRuleModification = Lens.lens (\GetSamplingTargetsResponse' {lastRuleModification} -> lastRuleModification) (\s@GetSamplingTargetsResponse' {} a -> s {lastRuleModification = a} :: GetSamplingTargetsResponse) Core.. Lens.mapping Core._Time

-- | Information about SamplingStatisticsDocument that X-Ray could not
-- process.
getSamplingTargetsResponse_unprocessedStatistics :: Lens.Lens' GetSamplingTargetsResponse (Core.Maybe [UnprocessedStatistics])
getSamplingTargetsResponse_unprocessedStatistics = Lens.lens (\GetSamplingTargetsResponse' {unprocessedStatistics} -> unprocessedStatistics) (\s@GetSamplingTargetsResponse' {} a -> s {unprocessedStatistics = a} :: GetSamplingTargetsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getSamplingTargetsResponse_httpStatus :: Lens.Lens' GetSamplingTargetsResponse Core.Int
getSamplingTargetsResponse_httpStatus = Lens.lens (\GetSamplingTargetsResponse' {httpStatus} -> httpStatus) (\s@GetSamplingTargetsResponse' {} a -> s {httpStatus = a} :: GetSamplingTargetsResponse)

instance Core.NFData GetSamplingTargetsResponse
