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
-- Module      : Amazonka.Route53RecoveryReadiness.GetArchitectureRecommendations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets recommendations about architecture designs for improving resiliency
-- for an application, based on a recovery group.
module Amazonka.Route53RecoveryReadiness.GetArchitectureRecommendations
  ( -- * Creating a Request
    GetArchitectureRecommendations (..),
    newGetArchitectureRecommendations,

    -- * Request Lenses
    getArchitectureRecommendations_nextToken,
    getArchitectureRecommendations_maxResults,
    getArchitectureRecommendations_recoveryGroupName,

    -- * Destructuring the Response
    GetArchitectureRecommendationsResponse (..),
    newGetArchitectureRecommendationsResponse,

    -- * Response Lenses
    getArchitectureRecommendationsResponse_nextToken,
    getArchitectureRecommendationsResponse_recommendations,
    getArchitectureRecommendationsResponse_lastAuditTimestamp,
    getArchitectureRecommendationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryReadiness.Types

-- | /See:/ 'newGetArchitectureRecommendations' smart constructor.
data GetArchitectureRecommendations = GetArchitectureRecommendations'
  { -- | The token that identifies which batch of results you want to see.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The number of objects that you want to return with this call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name of a recovery group.
    recoveryGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetArchitectureRecommendations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getArchitectureRecommendations_nextToken' - The token that identifies which batch of results you want to see.
--
-- 'maxResults', 'getArchitectureRecommendations_maxResults' - The number of objects that you want to return with this call.
--
-- 'recoveryGroupName', 'getArchitectureRecommendations_recoveryGroupName' - The name of a recovery group.
newGetArchitectureRecommendations ::
  -- | 'recoveryGroupName'
  Prelude.Text ->
  GetArchitectureRecommendations
newGetArchitectureRecommendations pRecoveryGroupName_ =
  GetArchitectureRecommendations'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      recoveryGroupName = pRecoveryGroupName_
    }

-- | The token that identifies which batch of results you want to see.
getArchitectureRecommendations_nextToken :: Lens.Lens' GetArchitectureRecommendations (Prelude.Maybe Prelude.Text)
getArchitectureRecommendations_nextToken = Lens.lens (\GetArchitectureRecommendations' {nextToken} -> nextToken) (\s@GetArchitectureRecommendations' {} a -> s {nextToken = a} :: GetArchitectureRecommendations)

-- | The number of objects that you want to return with this call.
getArchitectureRecommendations_maxResults :: Lens.Lens' GetArchitectureRecommendations (Prelude.Maybe Prelude.Natural)
getArchitectureRecommendations_maxResults = Lens.lens (\GetArchitectureRecommendations' {maxResults} -> maxResults) (\s@GetArchitectureRecommendations' {} a -> s {maxResults = a} :: GetArchitectureRecommendations)

-- | The name of a recovery group.
getArchitectureRecommendations_recoveryGroupName :: Lens.Lens' GetArchitectureRecommendations Prelude.Text
getArchitectureRecommendations_recoveryGroupName = Lens.lens (\GetArchitectureRecommendations' {recoveryGroupName} -> recoveryGroupName) (\s@GetArchitectureRecommendations' {} a -> s {recoveryGroupName = a} :: GetArchitectureRecommendations)

instance
  Core.AWSRequest
    GetArchitectureRecommendations
  where
  type
    AWSResponse GetArchitectureRecommendations =
      GetArchitectureRecommendationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetArchitectureRecommendationsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> ( x Core..?> "recommendations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "lastAuditTimestamp")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetArchitectureRecommendations
  where
  hashWithSalt
    _salt
    GetArchitectureRecommendations' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` recoveryGroupName

instance
  Prelude.NFData
    GetArchitectureRecommendations
  where
  rnf GetArchitectureRecommendations' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf recoveryGroupName

instance
  Core.ToHeaders
    GetArchitectureRecommendations
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetArchitectureRecommendations where
  toPath GetArchitectureRecommendations' {..} =
    Prelude.mconcat
      [ "/recoverygroups/",
        Core.toBS recoveryGroupName,
        "/architectureRecommendations"
      ]

instance Core.ToQuery GetArchitectureRecommendations where
  toQuery GetArchitectureRecommendations' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newGetArchitectureRecommendationsResponse' smart constructor.
data GetArchitectureRecommendationsResponse = GetArchitectureRecommendationsResponse'
  { -- | The token that identifies which batch of results you want to see.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of the recommendations for the customer\'s application.
    recommendations :: Prelude.Maybe [Recommendation],
    -- | The time that a recovery group was last assessed for recommendations, in
    -- UTC ISO-8601 format.
    lastAuditTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetArchitectureRecommendationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getArchitectureRecommendationsResponse_nextToken' - The token that identifies which batch of results you want to see.
--
-- 'recommendations', 'getArchitectureRecommendationsResponse_recommendations' - A list of the recommendations for the customer\'s application.
--
-- 'lastAuditTimestamp', 'getArchitectureRecommendationsResponse_lastAuditTimestamp' - The time that a recovery group was last assessed for recommendations, in
-- UTC ISO-8601 format.
--
-- 'httpStatus', 'getArchitectureRecommendationsResponse_httpStatus' - The response's http status code.
newGetArchitectureRecommendationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetArchitectureRecommendationsResponse
newGetArchitectureRecommendationsResponse
  pHttpStatus_ =
    GetArchitectureRecommendationsResponse'
      { nextToken =
          Prelude.Nothing,
        recommendations = Prelude.Nothing,
        lastAuditTimestamp =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token that identifies which batch of results you want to see.
getArchitectureRecommendationsResponse_nextToken :: Lens.Lens' GetArchitectureRecommendationsResponse (Prelude.Maybe Prelude.Text)
getArchitectureRecommendationsResponse_nextToken = Lens.lens (\GetArchitectureRecommendationsResponse' {nextToken} -> nextToken) (\s@GetArchitectureRecommendationsResponse' {} a -> s {nextToken = a} :: GetArchitectureRecommendationsResponse)

-- | A list of the recommendations for the customer\'s application.
getArchitectureRecommendationsResponse_recommendations :: Lens.Lens' GetArchitectureRecommendationsResponse (Prelude.Maybe [Recommendation])
getArchitectureRecommendationsResponse_recommendations = Lens.lens (\GetArchitectureRecommendationsResponse' {recommendations} -> recommendations) (\s@GetArchitectureRecommendationsResponse' {} a -> s {recommendations = a} :: GetArchitectureRecommendationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The time that a recovery group was last assessed for recommendations, in
-- UTC ISO-8601 format.
getArchitectureRecommendationsResponse_lastAuditTimestamp :: Lens.Lens' GetArchitectureRecommendationsResponse (Prelude.Maybe Prelude.UTCTime)
getArchitectureRecommendationsResponse_lastAuditTimestamp = Lens.lens (\GetArchitectureRecommendationsResponse' {lastAuditTimestamp} -> lastAuditTimestamp) (\s@GetArchitectureRecommendationsResponse' {} a -> s {lastAuditTimestamp = a} :: GetArchitectureRecommendationsResponse) Prelude.. Lens.mapping Core._Time

-- | The response's http status code.
getArchitectureRecommendationsResponse_httpStatus :: Lens.Lens' GetArchitectureRecommendationsResponse Prelude.Int
getArchitectureRecommendationsResponse_httpStatus = Lens.lens (\GetArchitectureRecommendationsResponse' {httpStatus} -> httpStatus) (\s@GetArchitectureRecommendationsResponse' {} a -> s {httpStatus = a} :: GetArchitectureRecommendationsResponse)

instance
  Prelude.NFData
    GetArchitectureRecommendationsResponse
  where
  rnf GetArchitectureRecommendationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf recommendations
      `Prelude.seq` Prelude.rnf lastAuditTimestamp
      `Prelude.seq` Prelude.rnf httpStatus
