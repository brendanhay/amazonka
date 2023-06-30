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
-- Module      : Amazonka.ConnectCases.ListFieldOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the field options for a field identifier in the domain.
module Amazonka.ConnectCases.ListFieldOptions
  ( -- * Creating a Request
    ListFieldOptions (..),
    newListFieldOptions,

    -- * Request Lenses
    listFieldOptions_maxResults,
    listFieldOptions_nextToken,
    listFieldOptions_values,
    listFieldOptions_domainId,
    listFieldOptions_fieldId,

    -- * Destructuring the Response
    ListFieldOptionsResponse (..),
    newListFieldOptionsResponse,

    -- * Response Lenses
    listFieldOptionsResponse_nextToken,
    listFieldOptionsResponse_httpStatus,
    listFieldOptionsResponse_options,
  )
where

import Amazonka.ConnectCases.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListFieldOptions' smart constructor.
data ListFieldOptions = ListFieldOptions'
  { -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of @FieldOption@ values to filter on for @ListFieldOptions@.
    values :: Prelude.Maybe [Prelude.Text],
    -- | The unique identifier of the Cases domain.
    domainId :: Prelude.Text,
    -- | The unique identifier of a field.
    fieldId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFieldOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listFieldOptions_maxResults' - The maximum number of results to return per page.
--
-- 'nextToken', 'listFieldOptions_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'values', 'listFieldOptions_values' - A list of @FieldOption@ values to filter on for @ListFieldOptions@.
--
-- 'domainId', 'listFieldOptions_domainId' - The unique identifier of the Cases domain.
--
-- 'fieldId', 'listFieldOptions_fieldId' - The unique identifier of a field.
newListFieldOptions ::
  -- | 'domainId'
  Prelude.Text ->
  -- | 'fieldId'
  Prelude.Text ->
  ListFieldOptions
newListFieldOptions pDomainId_ pFieldId_ =
  ListFieldOptions'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      values = Prelude.Nothing,
      domainId = pDomainId_,
      fieldId = pFieldId_
    }

-- | The maximum number of results to return per page.
listFieldOptions_maxResults :: Lens.Lens' ListFieldOptions (Prelude.Maybe Prelude.Natural)
listFieldOptions_maxResults = Lens.lens (\ListFieldOptions' {maxResults} -> maxResults) (\s@ListFieldOptions' {} a -> s {maxResults = a} :: ListFieldOptions)

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listFieldOptions_nextToken :: Lens.Lens' ListFieldOptions (Prelude.Maybe Prelude.Text)
listFieldOptions_nextToken = Lens.lens (\ListFieldOptions' {nextToken} -> nextToken) (\s@ListFieldOptions' {} a -> s {nextToken = a} :: ListFieldOptions)

-- | A list of @FieldOption@ values to filter on for @ListFieldOptions@.
listFieldOptions_values :: Lens.Lens' ListFieldOptions (Prelude.Maybe [Prelude.Text])
listFieldOptions_values = Lens.lens (\ListFieldOptions' {values} -> values) (\s@ListFieldOptions' {} a -> s {values = a} :: ListFieldOptions) Prelude.. Lens.mapping Lens.coerced

-- | The unique identifier of the Cases domain.
listFieldOptions_domainId :: Lens.Lens' ListFieldOptions Prelude.Text
listFieldOptions_domainId = Lens.lens (\ListFieldOptions' {domainId} -> domainId) (\s@ListFieldOptions' {} a -> s {domainId = a} :: ListFieldOptions)

-- | The unique identifier of a field.
listFieldOptions_fieldId :: Lens.Lens' ListFieldOptions Prelude.Text
listFieldOptions_fieldId = Lens.lens (\ListFieldOptions' {fieldId} -> fieldId) (\s@ListFieldOptions' {} a -> s {fieldId = a} :: ListFieldOptions)

instance Core.AWSRequest ListFieldOptions where
  type
    AWSResponse ListFieldOptions =
      ListFieldOptionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFieldOptionsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "options" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListFieldOptions where
  hashWithSalt _salt ListFieldOptions' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` values
      `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` fieldId

instance Prelude.NFData ListFieldOptions where
  rnf ListFieldOptions' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf values
      `Prelude.seq` Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf fieldId

instance Data.ToHeaders ListFieldOptions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListFieldOptions where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath ListFieldOptions where
  toPath ListFieldOptions' {..} =
    Prelude.mconcat
      [ "/domains/",
        Data.toBS domainId,
        "/fields/",
        Data.toBS fieldId,
        "/options-list"
      ]

instance Data.ToQuery ListFieldOptions where
  toQuery ListFieldOptions' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "values"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> values)
      ]

-- | /See:/ 'newListFieldOptionsResponse' smart constructor.
data ListFieldOptionsResponse = ListFieldOptionsResponse'
  { -- | The token for the next set of results. This is null if there are no more
    -- results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of @FieldOption@ objects.
    options :: [FieldOption]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFieldOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFieldOptionsResponse_nextToken' - The token for the next set of results. This is null if there are no more
-- results to return.
--
-- 'httpStatus', 'listFieldOptionsResponse_httpStatus' - The response's http status code.
--
-- 'options', 'listFieldOptionsResponse_options' - A list of @FieldOption@ objects.
newListFieldOptionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFieldOptionsResponse
newListFieldOptionsResponse pHttpStatus_ =
  ListFieldOptionsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      options = Prelude.mempty
    }

-- | The token for the next set of results. This is null if there are no more
-- results to return.
listFieldOptionsResponse_nextToken :: Lens.Lens' ListFieldOptionsResponse (Prelude.Maybe Prelude.Text)
listFieldOptionsResponse_nextToken = Lens.lens (\ListFieldOptionsResponse' {nextToken} -> nextToken) (\s@ListFieldOptionsResponse' {} a -> s {nextToken = a} :: ListFieldOptionsResponse)

-- | The response's http status code.
listFieldOptionsResponse_httpStatus :: Lens.Lens' ListFieldOptionsResponse Prelude.Int
listFieldOptionsResponse_httpStatus = Lens.lens (\ListFieldOptionsResponse' {httpStatus} -> httpStatus) (\s@ListFieldOptionsResponse' {} a -> s {httpStatus = a} :: ListFieldOptionsResponse)

-- | A list of @FieldOption@ objects.
listFieldOptionsResponse_options :: Lens.Lens' ListFieldOptionsResponse [FieldOption]
listFieldOptionsResponse_options = Lens.lens (\ListFieldOptionsResponse' {options} -> options) (\s@ListFieldOptionsResponse' {} a -> s {options = a} :: ListFieldOptionsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListFieldOptionsResponse where
  rnf ListFieldOptionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf options
