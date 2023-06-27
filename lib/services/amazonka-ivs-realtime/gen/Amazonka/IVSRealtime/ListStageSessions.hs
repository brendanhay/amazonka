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
-- Module      : Amazonka.IVSRealtime.ListStageSessions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets all sessions for a specified stage.
module Amazonka.IVSRealtime.ListStageSessions
  ( -- * Creating a Request
    ListStageSessions (..),
    newListStageSessions,

    -- * Request Lenses
    listStageSessions_maxResults,
    listStageSessions_nextToken,
    listStageSessions_stageArn,

    -- * Destructuring the Response
    ListStageSessionsResponse (..),
    newListStageSessionsResponse,

    -- * Response Lenses
    listStageSessionsResponse_nextToken,
    listStageSessionsResponse_httpStatus,
    listStageSessionsResponse_stageSessions,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVSRealtime.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListStageSessions' smart constructor.
data ListStageSessions = ListStageSessions'
  { -- | Maximum number of results to return. Default: 50.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The first stage to retrieve. This is used for pagination; see the
    -- @nextToken@ response field.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Stage ARN.
    stageArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStageSessions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listStageSessions_maxResults' - Maximum number of results to return. Default: 50.
--
-- 'nextToken', 'listStageSessions_nextToken' - The first stage to retrieve. This is used for pagination; see the
-- @nextToken@ response field.
--
-- 'stageArn', 'listStageSessions_stageArn' - Stage ARN.
newListStageSessions ::
  -- | 'stageArn'
  Prelude.Text ->
  ListStageSessions
newListStageSessions pStageArn_ =
  ListStageSessions'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      stageArn = pStageArn_
    }

-- | Maximum number of results to return. Default: 50.
listStageSessions_maxResults :: Lens.Lens' ListStageSessions (Prelude.Maybe Prelude.Natural)
listStageSessions_maxResults = Lens.lens (\ListStageSessions' {maxResults} -> maxResults) (\s@ListStageSessions' {} a -> s {maxResults = a} :: ListStageSessions)

-- | The first stage to retrieve. This is used for pagination; see the
-- @nextToken@ response field.
listStageSessions_nextToken :: Lens.Lens' ListStageSessions (Prelude.Maybe Prelude.Text)
listStageSessions_nextToken = Lens.lens (\ListStageSessions' {nextToken} -> nextToken) (\s@ListStageSessions' {} a -> s {nextToken = a} :: ListStageSessions)

-- | Stage ARN.
listStageSessions_stageArn :: Lens.Lens' ListStageSessions Prelude.Text
listStageSessions_stageArn = Lens.lens (\ListStageSessions' {stageArn} -> stageArn) (\s@ListStageSessions' {} a -> s {stageArn = a} :: ListStageSessions)

instance Core.AWSRequest ListStageSessions where
  type
    AWSResponse ListStageSessions =
      ListStageSessionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListStageSessionsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "stageSessions" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListStageSessions where
  hashWithSalt _salt ListStageSessions' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` stageArn

instance Prelude.NFData ListStageSessions where
  rnf ListStageSessions' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf stageArn

instance Data.ToHeaders ListStageSessions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListStageSessions where
  toJSON ListStageSessions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("stageArn" Data..= stageArn)
          ]
      )

instance Data.ToPath ListStageSessions where
  toPath = Prelude.const "/ListStageSessions"

instance Data.ToQuery ListStageSessions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListStageSessionsResponse' smart constructor.
data ListStageSessionsResponse = ListStageSessionsResponse'
  { -- | If there are more rooms than @maxResults@, use @nextToken@ in the
    -- request to get the next set.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | List of matching stage sessions.
    stageSessions :: [StageSessionSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStageSessionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listStageSessionsResponse_nextToken' - If there are more rooms than @maxResults@, use @nextToken@ in the
-- request to get the next set.
--
-- 'httpStatus', 'listStageSessionsResponse_httpStatus' - The response's http status code.
--
-- 'stageSessions', 'listStageSessionsResponse_stageSessions' - List of matching stage sessions.
newListStageSessionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListStageSessionsResponse
newListStageSessionsResponse pHttpStatus_ =
  ListStageSessionsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      stageSessions = Prelude.mempty
    }

-- | If there are more rooms than @maxResults@, use @nextToken@ in the
-- request to get the next set.
listStageSessionsResponse_nextToken :: Lens.Lens' ListStageSessionsResponse (Prelude.Maybe Prelude.Text)
listStageSessionsResponse_nextToken = Lens.lens (\ListStageSessionsResponse' {nextToken} -> nextToken) (\s@ListStageSessionsResponse' {} a -> s {nextToken = a} :: ListStageSessionsResponse)

-- | The response's http status code.
listStageSessionsResponse_httpStatus :: Lens.Lens' ListStageSessionsResponse Prelude.Int
listStageSessionsResponse_httpStatus = Lens.lens (\ListStageSessionsResponse' {httpStatus} -> httpStatus) (\s@ListStageSessionsResponse' {} a -> s {httpStatus = a} :: ListStageSessionsResponse)

-- | List of matching stage sessions.
listStageSessionsResponse_stageSessions :: Lens.Lens' ListStageSessionsResponse [StageSessionSummary]
listStageSessionsResponse_stageSessions = Lens.lens (\ListStageSessionsResponse' {stageSessions} -> stageSessions) (\s@ListStageSessionsResponse' {} a -> s {stageSessions = a} :: ListStageSessionsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListStageSessionsResponse where
  rnf ListStageSessionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf stageSessions
