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
-- Module      : Amazonka.Wisdom.GetRecommendations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves recommendations for the specified session. To avoid retrieving
-- the same recommendations in subsequent calls, use
-- <https://docs.aws.amazon.com/wisdom/latest/APIReference/API_NotifyRecommendationsReceived.html NotifyRecommendationsReceived>.
-- This API supports long-polling behavior with the @waitTimeSeconds@
-- parameter. Short poll is the default behavior and only returns
-- recommendations already available. To perform a manual query against an
-- assistant, use
-- <https://docs.aws.amazon.com/wisdom/latest/APIReference/API_QueryAssistant.html QueryAssistant>.
module Amazonka.Wisdom.GetRecommendations
  ( -- * Creating a Request
    GetRecommendations (..),
    newGetRecommendations,

    -- * Request Lenses
    getRecommendations_maxResults,
    getRecommendations_waitTimeSeconds,
    getRecommendations_assistantId,
    getRecommendations_sessionId,

    -- * Destructuring the Response
    GetRecommendationsResponse (..),
    newGetRecommendationsResponse,

    -- * Response Lenses
    getRecommendationsResponse_triggers,
    getRecommendationsResponse_httpStatus,
    getRecommendationsResponse_recommendations,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Wisdom.Types

-- | /See:/ 'newGetRecommendations' smart constructor.
data GetRecommendations = GetRecommendations'
  { -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The duration (in seconds) for which the call waits for a recommendation
    -- to be made available before returning. If a recommendation is available,
    -- the call returns sooner than @WaitTimeSeconds@. If no messages are
    -- available and the wait time expires, the call returns successfully with
    -- an empty list.
    waitTimeSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The identifier of the Wisdom assistant. Can be either the ID or the ARN.
    -- URLs cannot contain the ARN.
    assistantId :: Prelude.Text,
    -- | The identifier of the session. Can be either the ID or the ARN. URLs
    -- cannot contain the ARN.
    sessionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRecommendations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getRecommendations_maxResults' - The maximum number of results to return per page.
--
-- 'waitTimeSeconds', 'getRecommendations_waitTimeSeconds' - The duration (in seconds) for which the call waits for a recommendation
-- to be made available before returning. If a recommendation is available,
-- the call returns sooner than @WaitTimeSeconds@. If no messages are
-- available and the wait time expires, the call returns successfully with
-- an empty list.
--
-- 'assistantId', 'getRecommendations_assistantId' - The identifier of the Wisdom assistant. Can be either the ID or the ARN.
-- URLs cannot contain the ARN.
--
-- 'sessionId', 'getRecommendations_sessionId' - The identifier of the session. Can be either the ID or the ARN. URLs
-- cannot contain the ARN.
newGetRecommendations ::
  -- | 'assistantId'
  Prelude.Text ->
  -- | 'sessionId'
  Prelude.Text ->
  GetRecommendations
newGetRecommendations pAssistantId_ pSessionId_ =
  GetRecommendations'
    { maxResults = Prelude.Nothing,
      waitTimeSeconds = Prelude.Nothing,
      assistantId = pAssistantId_,
      sessionId = pSessionId_
    }

-- | The maximum number of results to return per page.
getRecommendations_maxResults :: Lens.Lens' GetRecommendations (Prelude.Maybe Prelude.Natural)
getRecommendations_maxResults = Lens.lens (\GetRecommendations' {maxResults} -> maxResults) (\s@GetRecommendations' {} a -> s {maxResults = a} :: GetRecommendations)

-- | The duration (in seconds) for which the call waits for a recommendation
-- to be made available before returning. If a recommendation is available,
-- the call returns sooner than @WaitTimeSeconds@. If no messages are
-- available and the wait time expires, the call returns successfully with
-- an empty list.
getRecommendations_waitTimeSeconds :: Lens.Lens' GetRecommendations (Prelude.Maybe Prelude.Natural)
getRecommendations_waitTimeSeconds = Lens.lens (\GetRecommendations' {waitTimeSeconds} -> waitTimeSeconds) (\s@GetRecommendations' {} a -> s {waitTimeSeconds = a} :: GetRecommendations)

-- | The identifier of the Wisdom assistant. Can be either the ID or the ARN.
-- URLs cannot contain the ARN.
getRecommendations_assistantId :: Lens.Lens' GetRecommendations Prelude.Text
getRecommendations_assistantId = Lens.lens (\GetRecommendations' {assistantId} -> assistantId) (\s@GetRecommendations' {} a -> s {assistantId = a} :: GetRecommendations)

-- | The identifier of the session. Can be either the ID or the ARN. URLs
-- cannot contain the ARN.
getRecommendations_sessionId :: Lens.Lens' GetRecommendations Prelude.Text
getRecommendations_sessionId = Lens.lens (\GetRecommendations' {sessionId} -> sessionId) (\s@GetRecommendations' {} a -> s {sessionId = a} :: GetRecommendations)

instance Core.AWSRequest GetRecommendations where
  type
    AWSResponse GetRecommendations =
      GetRecommendationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRecommendationsResponse'
            Prelude.<$> (x Data..?> "triggers" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "recommendations"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable GetRecommendations where
  hashWithSalt _salt GetRecommendations' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` waitTimeSeconds
      `Prelude.hashWithSalt` assistantId
      `Prelude.hashWithSalt` sessionId

instance Prelude.NFData GetRecommendations where
  rnf GetRecommendations' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf waitTimeSeconds
      `Prelude.seq` Prelude.rnf assistantId
      `Prelude.seq` Prelude.rnf sessionId

instance Data.ToHeaders GetRecommendations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetRecommendations where
  toPath GetRecommendations' {..} =
    Prelude.mconcat
      [ "/assistants/",
        Data.toBS assistantId,
        "/sessions/",
        Data.toBS sessionId,
        "/recommendations"
      ]

instance Data.ToQuery GetRecommendations where
  toQuery GetRecommendations' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "waitTimeSeconds" Data.=: waitTimeSeconds
      ]

-- | /See:/ 'newGetRecommendationsResponse' smart constructor.
data GetRecommendationsResponse = GetRecommendationsResponse'
  { -- | The triggers corresponding to recommendations.
    triggers :: Prelude.Maybe [RecommendationTrigger],
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The recommendations.
    recommendations :: [RecommendationData]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRecommendationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'triggers', 'getRecommendationsResponse_triggers' - The triggers corresponding to recommendations.
--
-- 'httpStatus', 'getRecommendationsResponse_httpStatus' - The response's http status code.
--
-- 'recommendations', 'getRecommendationsResponse_recommendations' - The recommendations.
newGetRecommendationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRecommendationsResponse
newGetRecommendationsResponse pHttpStatus_ =
  GetRecommendationsResponse'
    { triggers =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      recommendations = Prelude.mempty
    }

-- | The triggers corresponding to recommendations.
getRecommendationsResponse_triggers :: Lens.Lens' GetRecommendationsResponse (Prelude.Maybe [RecommendationTrigger])
getRecommendationsResponse_triggers = Lens.lens (\GetRecommendationsResponse' {triggers} -> triggers) (\s@GetRecommendationsResponse' {} a -> s {triggers = a} :: GetRecommendationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getRecommendationsResponse_httpStatus :: Lens.Lens' GetRecommendationsResponse Prelude.Int
getRecommendationsResponse_httpStatus = Lens.lens (\GetRecommendationsResponse' {httpStatus} -> httpStatus) (\s@GetRecommendationsResponse' {} a -> s {httpStatus = a} :: GetRecommendationsResponse)

-- | The recommendations.
getRecommendationsResponse_recommendations :: Lens.Lens' GetRecommendationsResponse [RecommendationData]
getRecommendationsResponse_recommendations = Lens.lens (\GetRecommendationsResponse' {recommendations} -> recommendations) (\s@GetRecommendationsResponse' {} a -> s {recommendations = a} :: GetRecommendationsResponse) Prelude.. Lens.coerced

instance Prelude.NFData GetRecommendationsResponse where
  rnf GetRecommendationsResponse' {..} =
    Prelude.rnf triggers
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf recommendations
