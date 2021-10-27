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
-- Module      : Network.AWS.Route53RecoveryReadiness.GetArchitectureRecommendations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a collection of recommendations to improve resilliance and
-- readiness check quality for a Recovery Group.
module Network.AWS.Route53RecoveryReadiness.GetArchitectureRecommendations
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
    getArchitectureRecommendationsResponse_lastAuditTimestamp,
    getArchitectureRecommendationsResponse_nextToken,
    getArchitectureRecommendationsResponse_recommendations,
    getArchitectureRecommendationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53RecoveryReadiness.Types

-- | /See:/ 'newGetArchitectureRecommendations' smart constructor.
data GetArchitectureRecommendations = GetArchitectureRecommendations'
  { -- | A token that can be used to resume pagination from the end of the
    -- collection.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Upper bound on number of records to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Name of RecoveryGroup (top level resource) to be analyzed.
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
-- 'nextToken', 'getArchitectureRecommendations_nextToken' - A token that can be used to resume pagination from the end of the
-- collection.
--
-- 'maxResults', 'getArchitectureRecommendations_maxResults' - Upper bound on number of records to return.
--
-- 'recoveryGroupName', 'getArchitectureRecommendations_recoveryGroupName' - Name of RecoveryGroup (top level resource) to be analyzed.
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

-- | A token that can be used to resume pagination from the end of the
-- collection.
getArchitectureRecommendations_nextToken :: Lens.Lens' GetArchitectureRecommendations (Prelude.Maybe Prelude.Text)
getArchitectureRecommendations_nextToken = Lens.lens (\GetArchitectureRecommendations' {nextToken} -> nextToken) (\s@GetArchitectureRecommendations' {} a -> s {nextToken = a} :: GetArchitectureRecommendations)

-- | Upper bound on number of records to return.
getArchitectureRecommendations_maxResults :: Lens.Lens' GetArchitectureRecommendations (Prelude.Maybe Prelude.Natural)
getArchitectureRecommendations_maxResults = Lens.lens (\GetArchitectureRecommendations' {maxResults} -> maxResults) (\s@GetArchitectureRecommendations' {} a -> s {maxResults = a} :: GetArchitectureRecommendations)

-- | Name of RecoveryGroup (top level resource) to be analyzed.
getArchitectureRecommendations_recoveryGroupName :: Lens.Lens' GetArchitectureRecommendations Prelude.Text
getArchitectureRecommendations_recoveryGroupName = Lens.lens (\GetArchitectureRecommendations' {recoveryGroupName} -> recoveryGroupName) (\s@GetArchitectureRecommendations' {} a -> s {recoveryGroupName = a} :: GetArchitectureRecommendations)

instance
  Core.AWSRequest
    GetArchitectureRecommendations
  where
  type
    AWSResponse GetArchitectureRecommendations =
      GetArchitectureRecommendationsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetArchitectureRecommendationsResponse'
            Prelude.<$> (x Core..?> "lastAuditTimestamp")
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> ( x Core..?> "recommendations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetArchitectureRecommendations

instance
  Prelude.NFData
    GetArchitectureRecommendations

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
  { -- | The time a Recovery Group was last assessed for recommendations in UTC
    -- ISO-8601 format.
    lastAuditTimestamp :: Prelude.Maybe Core.POSIX,
    -- | A token that can be used to resume pagination from the end of the
    -- collection
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of recommendations for the customer\'s application
    recommendations :: Prelude.Maybe [Recommendation],
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
-- 'lastAuditTimestamp', 'getArchitectureRecommendationsResponse_lastAuditTimestamp' - The time a Recovery Group was last assessed for recommendations in UTC
-- ISO-8601 format.
--
-- 'nextToken', 'getArchitectureRecommendationsResponse_nextToken' - A token that can be used to resume pagination from the end of the
-- collection
--
-- 'recommendations', 'getArchitectureRecommendationsResponse_recommendations' - A list of recommendations for the customer\'s application
--
-- 'httpStatus', 'getArchitectureRecommendationsResponse_httpStatus' - The response's http status code.
newGetArchitectureRecommendationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetArchitectureRecommendationsResponse
newGetArchitectureRecommendationsResponse
  pHttpStatus_ =
    GetArchitectureRecommendationsResponse'
      { lastAuditTimestamp =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        recommendations = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The time a Recovery Group was last assessed for recommendations in UTC
-- ISO-8601 format.
getArchitectureRecommendationsResponse_lastAuditTimestamp :: Lens.Lens' GetArchitectureRecommendationsResponse (Prelude.Maybe Prelude.UTCTime)
getArchitectureRecommendationsResponse_lastAuditTimestamp = Lens.lens (\GetArchitectureRecommendationsResponse' {lastAuditTimestamp} -> lastAuditTimestamp) (\s@GetArchitectureRecommendationsResponse' {} a -> s {lastAuditTimestamp = a} :: GetArchitectureRecommendationsResponse) Prelude.. Lens.mapping Core._Time

-- | A token that can be used to resume pagination from the end of the
-- collection
getArchitectureRecommendationsResponse_nextToken :: Lens.Lens' GetArchitectureRecommendationsResponse (Prelude.Maybe Prelude.Text)
getArchitectureRecommendationsResponse_nextToken = Lens.lens (\GetArchitectureRecommendationsResponse' {nextToken} -> nextToken) (\s@GetArchitectureRecommendationsResponse' {} a -> s {nextToken = a} :: GetArchitectureRecommendationsResponse)

-- | A list of recommendations for the customer\'s application
getArchitectureRecommendationsResponse_recommendations :: Lens.Lens' GetArchitectureRecommendationsResponse (Prelude.Maybe [Recommendation])
getArchitectureRecommendationsResponse_recommendations = Lens.lens (\GetArchitectureRecommendationsResponse' {recommendations} -> recommendations) (\s@GetArchitectureRecommendationsResponse' {} a -> s {recommendations = a} :: GetArchitectureRecommendationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getArchitectureRecommendationsResponse_httpStatus :: Lens.Lens' GetArchitectureRecommendationsResponse Prelude.Int
getArchitectureRecommendationsResponse_httpStatus = Lens.lens (\GetArchitectureRecommendationsResponse' {httpStatus} -> httpStatus) (\s@GetArchitectureRecommendationsResponse' {} a -> s {httpStatus = a} :: GetArchitectureRecommendationsResponse)

instance
  Prelude.NFData
    GetArchitectureRecommendationsResponse
