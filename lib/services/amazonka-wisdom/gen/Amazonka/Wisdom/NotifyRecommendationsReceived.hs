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
-- Module      : Amazonka.Wisdom.NotifyRecommendationsReceived
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified recommendations from the specified assistant\'s
-- queue of newly available recommendations. You can use this API in
-- conjunction with
-- <https://docs.aws.amazon.com/wisdom/latest/APIReference/API_GetRecommendations.html GetRecommendations>
-- and a @waitTimeSeconds@ input for long-polling behavior and avoiding
-- duplicate recommendations.
module Amazonka.Wisdom.NotifyRecommendationsReceived
  ( -- * Creating a Request
    NotifyRecommendationsReceived (..),
    newNotifyRecommendationsReceived,

    -- * Request Lenses
    notifyRecommendationsReceived_assistantId,
    notifyRecommendationsReceived_recommendationIds,
    notifyRecommendationsReceived_sessionId,

    -- * Destructuring the Response
    NotifyRecommendationsReceivedResponse (..),
    newNotifyRecommendationsReceivedResponse,

    -- * Response Lenses
    notifyRecommendationsReceivedResponse_recommendationIds,
    notifyRecommendationsReceivedResponse_errors,
    notifyRecommendationsReceivedResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Wisdom.Types

-- | /See:/ 'newNotifyRecommendationsReceived' smart constructor.
data NotifyRecommendationsReceived = NotifyRecommendationsReceived'
  { -- | The identifier of the Wisdom assistant. Can be either the ID or the ARN.
    -- URLs cannot contain the ARN.
    assistantId :: Prelude.Text,
    -- | The identifiers of the recommendations.
    recommendationIds :: [Prelude.Text],
    -- | The identifier of the session. Can be either the ID or the ARN. URLs
    -- cannot contain the ARN.
    sessionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NotifyRecommendationsReceived' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assistantId', 'notifyRecommendationsReceived_assistantId' - The identifier of the Wisdom assistant. Can be either the ID or the ARN.
-- URLs cannot contain the ARN.
--
-- 'recommendationIds', 'notifyRecommendationsReceived_recommendationIds' - The identifiers of the recommendations.
--
-- 'sessionId', 'notifyRecommendationsReceived_sessionId' - The identifier of the session. Can be either the ID or the ARN. URLs
-- cannot contain the ARN.
newNotifyRecommendationsReceived ::
  -- | 'assistantId'
  Prelude.Text ->
  -- | 'sessionId'
  Prelude.Text ->
  NotifyRecommendationsReceived
newNotifyRecommendationsReceived
  pAssistantId_
  pSessionId_ =
    NotifyRecommendationsReceived'
      { assistantId =
          pAssistantId_,
        recommendationIds = Prelude.mempty,
        sessionId = pSessionId_
      }

-- | The identifier of the Wisdom assistant. Can be either the ID or the ARN.
-- URLs cannot contain the ARN.
notifyRecommendationsReceived_assistantId :: Lens.Lens' NotifyRecommendationsReceived Prelude.Text
notifyRecommendationsReceived_assistantId = Lens.lens (\NotifyRecommendationsReceived' {assistantId} -> assistantId) (\s@NotifyRecommendationsReceived' {} a -> s {assistantId = a} :: NotifyRecommendationsReceived)

-- | The identifiers of the recommendations.
notifyRecommendationsReceived_recommendationIds :: Lens.Lens' NotifyRecommendationsReceived [Prelude.Text]
notifyRecommendationsReceived_recommendationIds = Lens.lens (\NotifyRecommendationsReceived' {recommendationIds} -> recommendationIds) (\s@NotifyRecommendationsReceived' {} a -> s {recommendationIds = a} :: NotifyRecommendationsReceived) Prelude.. Lens.coerced

-- | The identifier of the session. Can be either the ID or the ARN. URLs
-- cannot contain the ARN.
notifyRecommendationsReceived_sessionId :: Lens.Lens' NotifyRecommendationsReceived Prelude.Text
notifyRecommendationsReceived_sessionId = Lens.lens (\NotifyRecommendationsReceived' {sessionId} -> sessionId) (\s@NotifyRecommendationsReceived' {} a -> s {sessionId = a} :: NotifyRecommendationsReceived)

instance
  Core.AWSRequest
    NotifyRecommendationsReceived
  where
  type
    AWSResponse NotifyRecommendationsReceived =
      NotifyRecommendationsReceivedResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          NotifyRecommendationsReceivedResponse'
            Prelude.<$> ( x Core..?> "recommendationIds"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "errors" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    NotifyRecommendationsReceived
  where
  hashWithSalt _salt NotifyRecommendationsReceived' {..} =
    _salt `Prelude.hashWithSalt` assistantId
      `Prelude.hashWithSalt` recommendationIds
      `Prelude.hashWithSalt` sessionId

instance Prelude.NFData NotifyRecommendationsReceived where
  rnf NotifyRecommendationsReceived' {..} =
    Prelude.rnf assistantId
      `Prelude.seq` Prelude.rnf recommendationIds
      `Prelude.seq` Prelude.rnf sessionId

instance Core.ToHeaders NotifyRecommendationsReceived where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON NotifyRecommendationsReceived where
  toJSON NotifyRecommendationsReceived' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("recommendationIds" Core..= recommendationIds)
          ]
      )

instance Core.ToPath NotifyRecommendationsReceived where
  toPath NotifyRecommendationsReceived' {..} =
    Prelude.mconcat
      [ "/assistants/",
        Core.toBS assistantId,
        "/sessions/",
        Core.toBS sessionId,
        "/recommendations/notify"
      ]

instance Core.ToQuery NotifyRecommendationsReceived where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newNotifyRecommendationsReceivedResponse' smart constructor.
data NotifyRecommendationsReceivedResponse = NotifyRecommendationsReceivedResponse'
  { -- | The identifiers of the recommendations.
    recommendationIds :: Prelude.Maybe [Prelude.Text],
    -- | The identifiers of recommendations that are causing errors.
    errors :: Prelude.Maybe [NotifyRecommendationsReceivedError],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NotifyRecommendationsReceivedResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recommendationIds', 'notifyRecommendationsReceivedResponse_recommendationIds' - The identifiers of the recommendations.
--
-- 'errors', 'notifyRecommendationsReceivedResponse_errors' - The identifiers of recommendations that are causing errors.
--
-- 'httpStatus', 'notifyRecommendationsReceivedResponse_httpStatus' - The response's http status code.
newNotifyRecommendationsReceivedResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  NotifyRecommendationsReceivedResponse
newNotifyRecommendationsReceivedResponse pHttpStatus_ =
  NotifyRecommendationsReceivedResponse'
    { recommendationIds =
        Prelude.Nothing,
      errors = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifiers of the recommendations.
notifyRecommendationsReceivedResponse_recommendationIds :: Lens.Lens' NotifyRecommendationsReceivedResponse (Prelude.Maybe [Prelude.Text])
notifyRecommendationsReceivedResponse_recommendationIds = Lens.lens (\NotifyRecommendationsReceivedResponse' {recommendationIds} -> recommendationIds) (\s@NotifyRecommendationsReceivedResponse' {} a -> s {recommendationIds = a} :: NotifyRecommendationsReceivedResponse) Prelude.. Lens.mapping Lens.coerced

-- | The identifiers of recommendations that are causing errors.
notifyRecommendationsReceivedResponse_errors :: Lens.Lens' NotifyRecommendationsReceivedResponse (Prelude.Maybe [NotifyRecommendationsReceivedError])
notifyRecommendationsReceivedResponse_errors = Lens.lens (\NotifyRecommendationsReceivedResponse' {errors} -> errors) (\s@NotifyRecommendationsReceivedResponse' {} a -> s {errors = a} :: NotifyRecommendationsReceivedResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
notifyRecommendationsReceivedResponse_httpStatus :: Lens.Lens' NotifyRecommendationsReceivedResponse Prelude.Int
notifyRecommendationsReceivedResponse_httpStatus = Lens.lens (\NotifyRecommendationsReceivedResponse' {httpStatus} -> httpStatus) (\s@NotifyRecommendationsReceivedResponse' {} a -> s {httpStatus = a} :: NotifyRecommendationsReceivedResponse)

instance
  Prelude.NFData
    NotifyRecommendationsReceivedResponse
  where
  rnf NotifyRecommendationsReceivedResponse' {..} =
    Prelude.rnf recommendationIds
      `Prelude.seq` Prelude.rnf errors
      `Prelude.seq` Prelude.rnf httpStatus
