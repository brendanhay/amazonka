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
-- Module      : Amazonka.Kendra.ListIndices
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the Amazon Kendra indexes that you created.
module Amazonka.Kendra.ListIndices
  ( -- * Creating a Request
    ListIndices (..),
    newListIndices,

    -- * Request Lenses
    listIndices_maxResults,
    listIndices_nextToken,

    -- * Destructuring the Response
    ListIndicesResponse (..),
    newListIndicesResponse,

    -- * Response Lenses
    listIndicesResponse_indexConfigurationSummaryItems,
    listIndicesResponse_nextToken,
    listIndicesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListIndices' smart constructor.
data ListIndices = ListIndices'
  { -- | The maximum number of indices to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the previous response was incomplete (because there is more data to
    -- retrieve), Amazon Kendra returns a pagination token in the response. You
    -- can use this pagination token to retrieve the next set of indexes.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListIndices' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listIndices_maxResults' - The maximum number of indices to return.
--
-- 'nextToken', 'listIndices_nextToken' - If the previous response was incomplete (because there is more data to
-- retrieve), Amazon Kendra returns a pagination token in the response. You
-- can use this pagination token to retrieve the next set of indexes.
newListIndices ::
  ListIndices
newListIndices =
  ListIndices'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of indices to return.
listIndices_maxResults :: Lens.Lens' ListIndices (Prelude.Maybe Prelude.Natural)
listIndices_maxResults = Lens.lens (\ListIndices' {maxResults} -> maxResults) (\s@ListIndices' {} a -> s {maxResults = a} :: ListIndices)

-- | If the previous response was incomplete (because there is more data to
-- retrieve), Amazon Kendra returns a pagination token in the response. You
-- can use this pagination token to retrieve the next set of indexes.
listIndices_nextToken :: Lens.Lens' ListIndices (Prelude.Maybe Prelude.Text)
listIndices_nextToken = Lens.lens (\ListIndices' {nextToken} -> nextToken) (\s@ListIndices' {} a -> s {nextToken = a} :: ListIndices)

instance Core.AWSRequest ListIndices where
  type AWSResponse ListIndices = ListIndicesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListIndicesResponse'
            Prelude.<$> ( x Data..?> "IndexConfigurationSummaryItems"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListIndices where
  hashWithSalt _salt ListIndices' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListIndices where
  rnf ListIndices' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListIndices where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSKendraFrontendService.ListIndices" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListIndices where
  toJSON ListIndices' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListIndices where
  toPath = Prelude.const "/"

instance Data.ToQuery ListIndices where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListIndicesResponse' smart constructor.
data ListIndicesResponse = ListIndicesResponse'
  { -- | An array of summary information on the configuration of one or more
    -- indexes.
    indexConfigurationSummaryItems :: Prelude.Maybe [IndexConfigurationSummary],
    -- | If the response is truncated, Amazon Kendra returns this token that you
    -- can use in the subsequent request to retrieve the next set of indexes.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListIndicesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'indexConfigurationSummaryItems', 'listIndicesResponse_indexConfigurationSummaryItems' - An array of summary information on the configuration of one or more
-- indexes.
--
-- 'nextToken', 'listIndicesResponse_nextToken' - If the response is truncated, Amazon Kendra returns this token that you
-- can use in the subsequent request to retrieve the next set of indexes.
--
-- 'httpStatus', 'listIndicesResponse_httpStatus' - The response's http status code.
newListIndicesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListIndicesResponse
newListIndicesResponse pHttpStatus_ =
  ListIndicesResponse'
    { indexConfigurationSummaryItems =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of summary information on the configuration of one or more
-- indexes.
listIndicesResponse_indexConfigurationSummaryItems :: Lens.Lens' ListIndicesResponse (Prelude.Maybe [IndexConfigurationSummary])
listIndicesResponse_indexConfigurationSummaryItems = Lens.lens (\ListIndicesResponse' {indexConfigurationSummaryItems} -> indexConfigurationSummaryItems) (\s@ListIndicesResponse' {} a -> s {indexConfigurationSummaryItems = a} :: ListIndicesResponse) Prelude.. Lens.mapping Lens.coerced

-- | If the response is truncated, Amazon Kendra returns this token that you
-- can use in the subsequent request to retrieve the next set of indexes.
listIndicesResponse_nextToken :: Lens.Lens' ListIndicesResponse (Prelude.Maybe Prelude.Text)
listIndicesResponse_nextToken = Lens.lens (\ListIndicesResponse' {nextToken} -> nextToken) (\s@ListIndicesResponse' {} a -> s {nextToken = a} :: ListIndicesResponse)

-- | The response's http status code.
listIndicesResponse_httpStatus :: Lens.Lens' ListIndicesResponse Prelude.Int
listIndicesResponse_httpStatus = Lens.lens (\ListIndicesResponse' {httpStatus} -> httpStatus) (\s@ListIndicesResponse' {} a -> s {httpStatus = a} :: ListIndicesResponse)

instance Prelude.NFData ListIndicesResponse where
  rnf ListIndicesResponse' {..} =
    Prelude.rnf indexConfigurationSummaryItems
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
