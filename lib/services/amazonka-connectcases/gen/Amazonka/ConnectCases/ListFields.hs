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
-- Module      : Amazonka.ConnectCases.ListFields
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all fields in a Cases domain.
module Amazonka.ConnectCases.ListFields
  ( -- * Creating a Request
    ListFields (..),
    newListFields,

    -- * Request Lenses
    listFields_maxResults,
    listFields_nextToken,
    listFields_domainId,

    -- * Destructuring the Response
    ListFieldsResponse (..),
    newListFieldsResponse,

    -- * Response Lenses
    listFieldsResponse_nextToken,
    listFieldsResponse_httpStatus,
    listFieldsResponse_fields,
  )
where

import Amazonka.ConnectCases.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListFields' smart constructor.
data ListFields = ListFields'
  { -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the Cases domain.
    domainId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFields' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listFields_maxResults' - The maximum number of results to return per page.
--
-- 'nextToken', 'listFields_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'domainId', 'listFields_domainId' - The unique identifier of the Cases domain.
newListFields ::
  -- | 'domainId'
  Prelude.Text ->
  ListFields
newListFields pDomainId_ =
  ListFields'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      domainId = pDomainId_
    }

-- | The maximum number of results to return per page.
listFields_maxResults :: Lens.Lens' ListFields (Prelude.Maybe Prelude.Natural)
listFields_maxResults = Lens.lens (\ListFields' {maxResults} -> maxResults) (\s@ListFields' {} a -> s {maxResults = a} :: ListFields)

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listFields_nextToken :: Lens.Lens' ListFields (Prelude.Maybe Prelude.Text)
listFields_nextToken = Lens.lens (\ListFields' {nextToken} -> nextToken) (\s@ListFields' {} a -> s {nextToken = a} :: ListFields)

-- | The unique identifier of the Cases domain.
listFields_domainId :: Lens.Lens' ListFields Prelude.Text
listFields_domainId = Lens.lens (\ListFields' {domainId} -> domainId) (\s@ListFields' {} a -> s {domainId = a} :: ListFields)

instance Core.AWSRequest ListFields where
  type AWSResponse ListFields = ListFieldsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFieldsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "fields" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListFields where
  hashWithSalt _salt ListFields' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` domainId

instance Prelude.NFData ListFields where
  rnf ListFields' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf domainId

instance Data.ToHeaders ListFields where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListFields where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath ListFields where
  toPath ListFields' {..} =
    Prelude.mconcat
      ["/domains/", Data.toBS domainId, "/fields-list"]

instance Data.ToQuery ListFields where
  toQuery ListFields' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListFieldsResponse' smart constructor.
data ListFieldsResponse = ListFieldsResponse'
  { -- | The token for the next set of results. This is null if there are no more
    -- results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | List of detailed field information.
    fields :: [FieldSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFieldsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFieldsResponse_nextToken' - The token for the next set of results. This is null if there are no more
-- results to return.
--
-- 'httpStatus', 'listFieldsResponse_httpStatus' - The response's http status code.
--
-- 'fields', 'listFieldsResponse_fields' - List of detailed field information.
newListFieldsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFieldsResponse
newListFieldsResponse pHttpStatus_ =
  ListFieldsResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      fields = Prelude.mempty
    }

-- | The token for the next set of results. This is null if there are no more
-- results to return.
listFieldsResponse_nextToken :: Lens.Lens' ListFieldsResponse (Prelude.Maybe Prelude.Text)
listFieldsResponse_nextToken = Lens.lens (\ListFieldsResponse' {nextToken} -> nextToken) (\s@ListFieldsResponse' {} a -> s {nextToken = a} :: ListFieldsResponse)

-- | The response's http status code.
listFieldsResponse_httpStatus :: Lens.Lens' ListFieldsResponse Prelude.Int
listFieldsResponse_httpStatus = Lens.lens (\ListFieldsResponse' {httpStatus} -> httpStatus) (\s@ListFieldsResponse' {} a -> s {httpStatus = a} :: ListFieldsResponse)

-- | List of detailed field information.
listFieldsResponse_fields :: Lens.Lens' ListFieldsResponse [FieldSummary]
listFieldsResponse_fields = Lens.lens (\ListFieldsResponse' {fields} -> fields) (\s@ListFieldsResponse' {} a -> s {fields = a} :: ListFieldsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListFieldsResponse where
  rnf ListFieldsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf fields
