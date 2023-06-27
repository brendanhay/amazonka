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
-- Module      : Amazonka.IVSRealtime.ListStages
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets summary information about all stages in your account, in the AWS
-- region where the API request is processed.
module Amazonka.IVSRealtime.ListStages
  ( -- * Creating a Request
    ListStages (..),
    newListStages,

    -- * Request Lenses
    listStages_maxResults,
    listStages_nextToken,

    -- * Destructuring the Response
    ListStagesResponse (..),
    newListStagesResponse,

    -- * Response Lenses
    listStagesResponse_nextToken,
    listStagesResponse_httpStatus,
    listStagesResponse_stages,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVSRealtime.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListStages' smart constructor.
data ListStages = ListStages'
  { -- | Maximum number of results to return. Default: 50.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The first stage to retrieve. This is used for pagination; see the
    -- @nextToken@ response field.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStages' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listStages_maxResults' - Maximum number of results to return. Default: 50.
--
-- 'nextToken', 'listStages_nextToken' - The first stage to retrieve. This is used for pagination; see the
-- @nextToken@ response field.
newListStages ::
  ListStages
newListStages =
  ListStages'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Maximum number of results to return. Default: 50.
listStages_maxResults :: Lens.Lens' ListStages (Prelude.Maybe Prelude.Natural)
listStages_maxResults = Lens.lens (\ListStages' {maxResults} -> maxResults) (\s@ListStages' {} a -> s {maxResults = a} :: ListStages)

-- | The first stage to retrieve. This is used for pagination; see the
-- @nextToken@ response field.
listStages_nextToken :: Lens.Lens' ListStages (Prelude.Maybe Prelude.Text)
listStages_nextToken = Lens.lens (\ListStages' {nextToken} -> nextToken) (\s@ListStages' {} a -> s {nextToken = a} :: ListStages)

instance Core.AWSRequest ListStages where
  type AWSResponse ListStages = ListStagesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListStagesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "stages" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListStages where
  hashWithSalt _salt ListStages' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListStages where
  rnf ListStages' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListStages where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListStages where
  toJSON ListStages' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListStages where
  toPath = Prelude.const "/ListStages"

instance Data.ToQuery ListStages where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListStagesResponse' smart constructor.
data ListStagesResponse = ListStagesResponse'
  { -- | If there are more rooms than @maxResults@, use @nextToken@ in the
    -- request to get the next set.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | List of the matching stages (summary information only).
    stages :: [StageSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStagesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listStagesResponse_nextToken' - If there are more rooms than @maxResults@, use @nextToken@ in the
-- request to get the next set.
--
-- 'httpStatus', 'listStagesResponse_httpStatus' - The response's http status code.
--
-- 'stages', 'listStagesResponse_stages' - List of the matching stages (summary information only).
newListStagesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListStagesResponse
newListStagesResponse pHttpStatus_ =
  ListStagesResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      stages = Prelude.mempty
    }

-- | If there are more rooms than @maxResults@, use @nextToken@ in the
-- request to get the next set.
listStagesResponse_nextToken :: Lens.Lens' ListStagesResponse (Prelude.Maybe Prelude.Text)
listStagesResponse_nextToken = Lens.lens (\ListStagesResponse' {nextToken} -> nextToken) (\s@ListStagesResponse' {} a -> s {nextToken = a} :: ListStagesResponse)

-- | The response's http status code.
listStagesResponse_httpStatus :: Lens.Lens' ListStagesResponse Prelude.Int
listStagesResponse_httpStatus = Lens.lens (\ListStagesResponse' {httpStatus} -> httpStatus) (\s@ListStagesResponse' {} a -> s {httpStatus = a} :: ListStagesResponse)

-- | List of the matching stages (summary information only).
listStagesResponse_stages :: Lens.Lens' ListStagesResponse [StageSummary]
listStagesResponse_stages = Lens.lens (\ListStagesResponse' {stages} -> stages) (\s@ListStagesResponse' {} a -> s {stages = a} :: ListStagesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListStagesResponse where
  rnf ListStagesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf stages
