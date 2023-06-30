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
-- Module      : Amazonka.Kendra.ListEntityPersonas
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists specific permissions of users and groups with access to your
-- Amazon Kendra experience.
module Amazonka.Kendra.ListEntityPersonas
  ( -- * Creating a Request
    ListEntityPersonas (..),
    newListEntityPersonas,

    -- * Request Lenses
    listEntityPersonas_maxResults,
    listEntityPersonas_nextToken,
    listEntityPersonas_id,
    listEntityPersonas_indexId,

    -- * Destructuring the Response
    ListEntityPersonasResponse (..),
    newListEntityPersonasResponse,

    -- * Response Lenses
    listEntityPersonasResponse_nextToken,
    listEntityPersonasResponse_summaryItems,
    listEntityPersonasResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListEntityPersonas' smart constructor.
data ListEntityPersonas = ListEntityPersonas'
  { -- | The maximum number of returned users or groups.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the previous response was incomplete (because there is more data to
    -- retrieve), Amazon Kendra returns a pagination token in the response. You
    -- can use this pagination token to retrieve the next set of users or
    -- groups.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of your Amazon Kendra experience.
    id :: Prelude.Text,
    -- | The identifier of the index for your Amazon Kendra experience.
    indexId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEntityPersonas' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listEntityPersonas_maxResults' - The maximum number of returned users or groups.
--
-- 'nextToken', 'listEntityPersonas_nextToken' - If the previous response was incomplete (because there is more data to
-- retrieve), Amazon Kendra returns a pagination token in the response. You
-- can use this pagination token to retrieve the next set of users or
-- groups.
--
-- 'id', 'listEntityPersonas_id' - The identifier of your Amazon Kendra experience.
--
-- 'indexId', 'listEntityPersonas_indexId' - The identifier of the index for your Amazon Kendra experience.
newListEntityPersonas ::
  -- | 'id'
  Prelude.Text ->
  -- | 'indexId'
  Prelude.Text ->
  ListEntityPersonas
newListEntityPersonas pId_ pIndexId_ =
  ListEntityPersonas'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      id = pId_,
      indexId = pIndexId_
    }

-- | The maximum number of returned users or groups.
listEntityPersonas_maxResults :: Lens.Lens' ListEntityPersonas (Prelude.Maybe Prelude.Natural)
listEntityPersonas_maxResults = Lens.lens (\ListEntityPersonas' {maxResults} -> maxResults) (\s@ListEntityPersonas' {} a -> s {maxResults = a} :: ListEntityPersonas)

-- | If the previous response was incomplete (because there is more data to
-- retrieve), Amazon Kendra returns a pagination token in the response. You
-- can use this pagination token to retrieve the next set of users or
-- groups.
listEntityPersonas_nextToken :: Lens.Lens' ListEntityPersonas (Prelude.Maybe Prelude.Text)
listEntityPersonas_nextToken = Lens.lens (\ListEntityPersonas' {nextToken} -> nextToken) (\s@ListEntityPersonas' {} a -> s {nextToken = a} :: ListEntityPersonas)

-- | The identifier of your Amazon Kendra experience.
listEntityPersonas_id :: Lens.Lens' ListEntityPersonas Prelude.Text
listEntityPersonas_id = Lens.lens (\ListEntityPersonas' {id} -> id) (\s@ListEntityPersonas' {} a -> s {id = a} :: ListEntityPersonas)

-- | The identifier of the index for your Amazon Kendra experience.
listEntityPersonas_indexId :: Lens.Lens' ListEntityPersonas Prelude.Text
listEntityPersonas_indexId = Lens.lens (\ListEntityPersonas' {indexId} -> indexId) (\s@ListEntityPersonas' {} a -> s {indexId = a} :: ListEntityPersonas)

instance Core.AWSRequest ListEntityPersonas where
  type
    AWSResponse ListEntityPersonas =
      ListEntityPersonasResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEntityPersonasResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "SummaryItems" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListEntityPersonas where
  hashWithSalt _salt ListEntityPersonas' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` indexId

instance Prelude.NFData ListEntityPersonas where
  rnf ListEntityPersonas' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf indexId

instance Data.ToHeaders ListEntityPersonas where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSKendraFrontendService.ListEntityPersonas" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListEntityPersonas where
  toJSON ListEntityPersonas' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("Id" Data..= id),
            Prelude.Just ("IndexId" Data..= indexId)
          ]
      )

instance Data.ToPath ListEntityPersonas where
  toPath = Prelude.const "/"

instance Data.ToQuery ListEntityPersonas where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListEntityPersonasResponse' smart constructor.
data ListEntityPersonasResponse = ListEntityPersonasResponse'
  { -- | If the response is truncated, Amazon Kendra returns this token, which
    -- you can use in a later request to retrieve the next set of users or
    -- groups.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of summary information for one or more users or groups.
    summaryItems :: Prelude.Maybe [PersonasSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEntityPersonasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEntityPersonasResponse_nextToken' - If the response is truncated, Amazon Kendra returns this token, which
-- you can use in a later request to retrieve the next set of users or
-- groups.
--
-- 'summaryItems', 'listEntityPersonasResponse_summaryItems' - An array of summary information for one or more users or groups.
--
-- 'httpStatus', 'listEntityPersonasResponse_httpStatus' - The response's http status code.
newListEntityPersonasResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEntityPersonasResponse
newListEntityPersonasResponse pHttpStatus_ =
  ListEntityPersonasResponse'
    { nextToken =
        Prelude.Nothing,
      summaryItems = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the response is truncated, Amazon Kendra returns this token, which
-- you can use in a later request to retrieve the next set of users or
-- groups.
listEntityPersonasResponse_nextToken :: Lens.Lens' ListEntityPersonasResponse (Prelude.Maybe Prelude.Text)
listEntityPersonasResponse_nextToken = Lens.lens (\ListEntityPersonasResponse' {nextToken} -> nextToken) (\s@ListEntityPersonasResponse' {} a -> s {nextToken = a} :: ListEntityPersonasResponse)

-- | An array of summary information for one or more users or groups.
listEntityPersonasResponse_summaryItems :: Lens.Lens' ListEntityPersonasResponse (Prelude.Maybe [PersonasSummary])
listEntityPersonasResponse_summaryItems = Lens.lens (\ListEntityPersonasResponse' {summaryItems} -> summaryItems) (\s@ListEntityPersonasResponse' {} a -> s {summaryItems = a} :: ListEntityPersonasResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listEntityPersonasResponse_httpStatus :: Lens.Lens' ListEntityPersonasResponse Prelude.Int
listEntityPersonasResponse_httpStatus = Lens.lens (\ListEntityPersonasResponse' {httpStatus} -> httpStatus) (\s@ListEntityPersonasResponse' {} a -> s {httpStatus = a} :: ListEntityPersonasResponse)

instance Prelude.NFData ListEntityPersonasResponse where
  rnf ListEntityPersonasResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf summaryItems
      `Prelude.seq` Prelude.rnf httpStatus
