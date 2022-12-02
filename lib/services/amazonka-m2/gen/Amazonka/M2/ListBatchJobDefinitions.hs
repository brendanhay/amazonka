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
-- Module      : Amazonka.M2.ListBatchJobDefinitions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the available batch job definitions based on the batch job
-- resources uploaded during the application creation. The listed batch job
-- definitions can then be used to start a batch job.
--
-- This operation returns paginated results.
module Amazonka.M2.ListBatchJobDefinitions
  ( -- * Creating a Request
    ListBatchJobDefinitions (..),
    newListBatchJobDefinitions,

    -- * Request Lenses
    listBatchJobDefinitions_nextToken,
    listBatchJobDefinitions_maxResults,
    listBatchJobDefinitions_prefix,
    listBatchJobDefinitions_applicationId,

    -- * Destructuring the Response
    ListBatchJobDefinitionsResponse (..),
    newListBatchJobDefinitionsResponse,

    -- * Response Lenses
    listBatchJobDefinitionsResponse_nextToken,
    listBatchJobDefinitionsResponse_httpStatus,
    listBatchJobDefinitionsResponse_batchJobDefinitions,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.M2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListBatchJobDefinitions' smart constructor.
data ListBatchJobDefinitions = ListBatchJobDefinitions'
  { -- | A pagination token returned from a previous call to this operation. This
    -- specifies the next item to return. To return to the beginning of the
    -- list, exclude this parameter.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of batch job definitions to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the batch job definition is a FileBatchJobDefinition, the prefix
    -- allows you to search on the file names of FileBatchJobDefinitions.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the application.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBatchJobDefinitions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listBatchJobDefinitions_nextToken' - A pagination token returned from a previous call to this operation. This
-- specifies the next item to return. To return to the beginning of the
-- list, exclude this parameter.
--
-- 'maxResults', 'listBatchJobDefinitions_maxResults' - The maximum number of batch job definitions to return.
--
-- 'prefix', 'listBatchJobDefinitions_prefix' - If the batch job definition is a FileBatchJobDefinition, the prefix
-- allows you to search on the file names of FileBatchJobDefinitions.
--
-- 'applicationId', 'listBatchJobDefinitions_applicationId' - The identifier of the application.
newListBatchJobDefinitions ::
  -- | 'applicationId'
  Prelude.Text ->
  ListBatchJobDefinitions
newListBatchJobDefinitions pApplicationId_ =
  ListBatchJobDefinitions'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      prefix = Prelude.Nothing,
      applicationId = pApplicationId_
    }

-- | A pagination token returned from a previous call to this operation. This
-- specifies the next item to return. To return to the beginning of the
-- list, exclude this parameter.
listBatchJobDefinitions_nextToken :: Lens.Lens' ListBatchJobDefinitions (Prelude.Maybe Prelude.Text)
listBatchJobDefinitions_nextToken = Lens.lens (\ListBatchJobDefinitions' {nextToken} -> nextToken) (\s@ListBatchJobDefinitions' {} a -> s {nextToken = a} :: ListBatchJobDefinitions)

-- | The maximum number of batch job definitions to return.
listBatchJobDefinitions_maxResults :: Lens.Lens' ListBatchJobDefinitions (Prelude.Maybe Prelude.Natural)
listBatchJobDefinitions_maxResults = Lens.lens (\ListBatchJobDefinitions' {maxResults} -> maxResults) (\s@ListBatchJobDefinitions' {} a -> s {maxResults = a} :: ListBatchJobDefinitions)

-- | If the batch job definition is a FileBatchJobDefinition, the prefix
-- allows you to search on the file names of FileBatchJobDefinitions.
listBatchJobDefinitions_prefix :: Lens.Lens' ListBatchJobDefinitions (Prelude.Maybe Prelude.Text)
listBatchJobDefinitions_prefix = Lens.lens (\ListBatchJobDefinitions' {prefix} -> prefix) (\s@ListBatchJobDefinitions' {} a -> s {prefix = a} :: ListBatchJobDefinitions)

-- | The identifier of the application.
listBatchJobDefinitions_applicationId :: Lens.Lens' ListBatchJobDefinitions Prelude.Text
listBatchJobDefinitions_applicationId = Lens.lens (\ListBatchJobDefinitions' {applicationId} -> applicationId) (\s@ListBatchJobDefinitions' {} a -> s {applicationId = a} :: ListBatchJobDefinitions)

instance Core.AWSPager ListBatchJobDefinitions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listBatchJobDefinitionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listBatchJobDefinitionsResponse_batchJobDefinitions
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listBatchJobDefinitions_nextToken
          Lens..~ rs
          Lens.^? listBatchJobDefinitionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListBatchJobDefinitions where
  type
    AWSResponse ListBatchJobDefinitions =
      ListBatchJobDefinitionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBatchJobDefinitionsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..?> "batchJobDefinitions"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListBatchJobDefinitions where
  hashWithSalt _salt ListBatchJobDefinitions' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` prefix
      `Prelude.hashWithSalt` applicationId

instance Prelude.NFData ListBatchJobDefinitions where
  rnf ListBatchJobDefinitions' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf prefix
      `Prelude.seq` Prelude.rnf applicationId

instance Data.ToHeaders ListBatchJobDefinitions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListBatchJobDefinitions where
  toPath ListBatchJobDefinitions' {..} =
    Prelude.mconcat
      [ "/applications/",
        Data.toBS applicationId,
        "/batch-job-definitions"
      ]

instance Data.ToQuery ListBatchJobDefinitions where
  toQuery ListBatchJobDefinitions' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "maxResults" Data.=: maxResults,
        "prefix" Data.=: prefix
      ]

-- | /See:/ 'newListBatchJobDefinitionsResponse' smart constructor.
data ListBatchJobDefinitionsResponse = ListBatchJobDefinitionsResponse'
  { -- | If there are more items to return, this contains a token that is passed
    -- to a subsequent call to this operation to retrieve the next set of
    -- items.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The list of batch job definitions.
    batchJobDefinitions :: [BatchJobDefinition]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBatchJobDefinitionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listBatchJobDefinitionsResponse_nextToken' - If there are more items to return, this contains a token that is passed
-- to a subsequent call to this operation to retrieve the next set of
-- items.
--
-- 'httpStatus', 'listBatchJobDefinitionsResponse_httpStatus' - The response's http status code.
--
-- 'batchJobDefinitions', 'listBatchJobDefinitionsResponse_batchJobDefinitions' - The list of batch job definitions.
newListBatchJobDefinitionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListBatchJobDefinitionsResponse
newListBatchJobDefinitionsResponse pHttpStatus_ =
  ListBatchJobDefinitionsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      batchJobDefinitions = Prelude.mempty
    }

-- | If there are more items to return, this contains a token that is passed
-- to a subsequent call to this operation to retrieve the next set of
-- items.
listBatchJobDefinitionsResponse_nextToken :: Lens.Lens' ListBatchJobDefinitionsResponse (Prelude.Maybe Prelude.Text)
listBatchJobDefinitionsResponse_nextToken = Lens.lens (\ListBatchJobDefinitionsResponse' {nextToken} -> nextToken) (\s@ListBatchJobDefinitionsResponse' {} a -> s {nextToken = a} :: ListBatchJobDefinitionsResponse)

-- | The response's http status code.
listBatchJobDefinitionsResponse_httpStatus :: Lens.Lens' ListBatchJobDefinitionsResponse Prelude.Int
listBatchJobDefinitionsResponse_httpStatus = Lens.lens (\ListBatchJobDefinitionsResponse' {httpStatus} -> httpStatus) (\s@ListBatchJobDefinitionsResponse' {} a -> s {httpStatus = a} :: ListBatchJobDefinitionsResponse)

-- | The list of batch job definitions.
listBatchJobDefinitionsResponse_batchJobDefinitions :: Lens.Lens' ListBatchJobDefinitionsResponse [BatchJobDefinition]
listBatchJobDefinitionsResponse_batchJobDefinitions = Lens.lens (\ListBatchJobDefinitionsResponse' {batchJobDefinitions} -> batchJobDefinitions) (\s@ListBatchJobDefinitionsResponse' {} a -> s {batchJobDefinitions = a} :: ListBatchJobDefinitionsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListBatchJobDefinitionsResponse
  where
  rnf ListBatchJobDefinitionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf batchJobDefinitions
