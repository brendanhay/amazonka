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
-- Module      : Amazonka.KafkaConnect.ListWorkerConfigurations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all of the worker configurations in this account and
-- Region.
--
-- This operation returns paginated results.
module Amazonka.KafkaConnect.ListWorkerConfigurations
  ( -- * Creating a Request
    ListWorkerConfigurations (..),
    newListWorkerConfigurations,

    -- * Request Lenses
    listWorkerConfigurations_nextToken,
    listWorkerConfigurations_maxResults,

    -- * Destructuring the Response
    ListWorkerConfigurationsResponse (..),
    newListWorkerConfigurationsResponse,

    -- * Response Lenses
    listWorkerConfigurationsResponse_nextToken,
    listWorkerConfigurationsResponse_workerConfigurations,
    listWorkerConfigurationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KafkaConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListWorkerConfigurations' smart constructor.
data ListWorkerConfigurations = ListWorkerConfigurations'
  { -- | If the response of a ListWorkerConfigurations operation is truncated, it
    -- will include a NextToken. Send this NextToken in a subsequent request to
    -- continue listing from where the previous operation left off.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of worker configurations to list in one response.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWorkerConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listWorkerConfigurations_nextToken' - If the response of a ListWorkerConfigurations operation is truncated, it
-- will include a NextToken. Send this NextToken in a subsequent request to
-- continue listing from where the previous operation left off.
--
-- 'maxResults', 'listWorkerConfigurations_maxResults' - The maximum number of worker configurations to list in one response.
newListWorkerConfigurations ::
  ListWorkerConfigurations
newListWorkerConfigurations =
  ListWorkerConfigurations'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | If the response of a ListWorkerConfigurations operation is truncated, it
-- will include a NextToken. Send this NextToken in a subsequent request to
-- continue listing from where the previous operation left off.
listWorkerConfigurations_nextToken :: Lens.Lens' ListWorkerConfigurations (Prelude.Maybe Prelude.Text)
listWorkerConfigurations_nextToken = Lens.lens (\ListWorkerConfigurations' {nextToken} -> nextToken) (\s@ListWorkerConfigurations' {} a -> s {nextToken = a} :: ListWorkerConfigurations)

-- | The maximum number of worker configurations to list in one response.
listWorkerConfigurations_maxResults :: Lens.Lens' ListWorkerConfigurations (Prelude.Maybe Prelude.Natural)
listWorkerConfigurations_maxResults = Lens.lens (\ListWorkerConfigurations' {maxResults} -> maxResults) (\s@ListWorkerConfigurations' {} a -> s {maxResults = a} :: ListWorkerConfigurations)

instance Core.AWSPager ListWorkerConfigurations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listWorkerConfigurationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listWorkerConfigurationsResponse_workerConfigurations
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listWorkerConfigurations_nextToken
          Lens..~ rs
          Lens.^? listWorkerConfigurationsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListWorkerConfigurations where
  type
    AWSResponse ListWorkerConfigurations =
      ListWorkerConfigurationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListWorkerConfigurationsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x Data..?> "workerConfigurations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListWorkerConfigurations where
  hashWithSalt _salt ListWorkerConfigurations' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListWorkerConfigurations where
  rnf ListWorkerConfigurations' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListWorkerConfigurations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListWorkerConfigurations where
  toPath = Prelude.const "/v1/worker-configurations"

instance Data.ToQuery ListWorkerConfigurations where
  toQuery ListWorkerConfigurations' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "maxResults" Data.=: maxResults
      ]

-- | /See:/ 'newListWorkerConfigurationsResponse' smart constructor.
data ListWorkerConfigurationsResponse = ListWorkerConfigurationsResponse'
  { -- | If the response of a ListWorkerConfigurations operation is truncated, it
    -- will include a NextToken. Send this NextToken in a subsequent request to
    -- continue listing from where the previous operation left off.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of worker configuration descriptions.
    workerConfigurations :: Prelude.Maybe [WorkerConfigurationSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWorkerConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listWorkerConfigurationsResponse_nextToken' - If the response of a ListWorkerConfigurations operation is truncated, it
-- will include a NextToken. Send this NextToken in a subsequent request to
-- continue listing from where the previous operation left off.
--
-- 'workerConfigurations', 'listWorkerConfigurationsResponse_workerConfigurations' - An array of worker configuration descriptions.
--
-- 'httpStatus', 'listWorkerConfigurationsResponse_httpStatus' - The response's http status code.
newListWorkerConfigurationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListWorkerConfigurationsResponse
newListWorkerConfigurationsResponse pHttpStatus_ =
  ListWorkerConfigurationsResponse'
    { nextToken =
        Prelude.Nothing,
      workerConfigurations = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the response of a ListWorkerConfigurations operation is truncated, it
-- will include a NextToken. Send this NextToken in a subsequent request to
-- continue listing from where the previous operation left off.
listWorkerConfigurationsResponse_nextToken :: Lens.Lens' ListWorkerConfigurationsResponse (Prelude.Maybe Prelude.Text)
listWorkerConfigurationsResponse_nextToken = Lens.lens (\ListWorkerConfigurationsResponse' {nextToken} -> nextToken) (\s@ListWorkerConfigurationsResponse' {} a -> s {nextToken = a} :: ListWorkerConfigurationsResponse)

-- | An array of worker configuration descriptions.
listWorkerConfigurationsResponse_workerConfigurations :: Lens.Lens' ListWorkerConfigurationsResponse (Prelude.Maybe [WorkerConfigurationSummary])
listWorkerConfigurationsResponse_workerConfigurations = Lens.lens (\ListWorkerConfigurationsResponse' {workerConfigurations} -> workerConfigurations) (\s@ListWorkerConfigurationsResponse' {} a -> s {workerConfigurations = a} :: ListWorkerConfigurationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listWorkerConfigurationsResponse_httpStatus :: Lens.Lens' ListWorkerConfigurationsResponse Prelude.Int
listWorkerConfigurationsResponse_httpStatus = Lens.lens (\ListWorkerConfigurationsResponse' {httpStatus} -> httpStatus) (\s@ListWorkerConfigurationsResponse' {} a -> s {httpStatus = a} :: ListWorkerConfigurationsResponse)

instance
  Prelude.NFData
    ListWorkerConfigurationsResponse
  where
  rnf ListWorkerConfigurationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf workerConfigurations
      `Prelude.seq` Prelude.rnf httpStatus
