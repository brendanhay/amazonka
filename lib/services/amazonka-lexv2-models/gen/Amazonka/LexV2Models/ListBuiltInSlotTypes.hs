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
-- Module      : Amazonka.LexV2Models.ListBuiltInSlotTypes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of built-in slot types that meet the specified criteria.
module Amazonka.LexV2Models.ListBuiltInSlotTypes
  ( -- * Creating a Request
    ListBuiltInSlotTypes (..),
    newListBuiltInSlotTypes,

    -- * Request Lenses
    listBuiltInSlotTypes_maxResults,
    listBuiltInSlotTypes_nextToken,
    listBuiltInSlotTypes_sortBy,
    listBuiltInSlotTypes_localeId,

    -- * Destructuring the Response
    ListBuiltInSlotTypesResponse (..),
    newListBuiltInSlotTypesResponse,

    -- * Response Lenses
    listBuiltInSlotTypesResponse_builtInSlotTypeSummaries,
    listBuiltInSlotTypesResponse_localeId,
    listBuiltInSlotTypesResponse_nextToken,
    listBuiltInSlotTypesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListBuiltInSlotTypes' smart constructor.
data ListBuiltInSlotTypes = ListBuiltInSlotTypes'
  { -- | The maximum number of built-in slot types to return in each page of
    -- results. If there are fewer results than the max page size, only the
    -- actual number of results are returned.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the response from the @ListBuiltInSlotTypes@ operation contains more
    -- results than specified in the @maxResults@ parameter, a token is
    -- returned in the response. Use that token in the @nextToken@ parameter to
    -- return the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Determines the sort order for the response from the
    -- @ListBuiltInSlotTypes@ operation. You can choose to sort by the slot
    -- type signature in either ascending or descending order.
    sortBy :: Prelude.Maybe BuiltInSlotTypeSortBy,
    -- | The identifier of the language and locale of the slot types to list. The
    -- string must match one of the supported locales. For more information,
    -- see
    -- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
    localeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBuiltInSlotTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listBuiltInSlotTypes_maxResults' - The maximum number of built-in slot types to return in each page of
-- results. If there are fewer results than the max page size, only the
-- actual number of results are returned.
--
-- 'nextToken', 'listBuiltInSlotTypes_nextToken' - If the response from the @ListBuiltInSlotTypes@ operation contains more
-- results than specified in the @maxResults@ parameter, a token is
-- returned in the response. Use that token in the @nextToken@ parameter to
-- return the next page of results.
--
-- 'sortBy', 'listBuiltInSlotTypes_sortBy' - Determines the sort order for the response from the
-- @ListBuiltInSlotTypes@ operation. You can choose to sort by the slot
-- type signature in either ascending or descending order.
--
-- 'localeId', 'listBuiltInSlotTypes_localeId' - The identifier of the language and locale of the slot types to list. The
-- string must match one of the supported locales. For more information,
-- see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
newListBuiltInSlotTypes ::
  -- | 'localeId'
  Prelude.Text ->
  ListBuiltInSlotTypes
newListBuiltInSlotTypes pLocaleId_ =
  ListBuiltInSlotTypes'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      localeId = pLocaleId_
    }

-- | The maximum number of built-in slot types to return in each page of
-- results. If there are fewer results than the max page size, only the
-- actual number of results are returned.
listBuiltInSlotTypes_maxResults :: Lens.Lens' ListBuiltInSlotTypes (Prelude.Maybe Prelude.Natural)
listBuiltInSlotTypes_maxResults = Lens.lens (\ListBuiltInSlotTypes' {maxResults} -> maxResults) (\s@ListBuiltInSlotTypes' {} a -> s {maxResults = a} :: ListBuiltInSlotTypes)

-- | If the response from the @ListBuiltInSlotTypes@ operation contains more
-- results than specified in the @maxResults@ parameter, a token is
-- returned in the response. Use that token in the @nextToken@ parameter to
-- return the next page of results.
listBuiltInSlotTypes_nextToken :: Lens.Lens' ListBuiltInSlotTypes (Prelude.Maybe Prelude.Text)
listBuiltInSlotTypes_nextToken = Lens.lens (\ListBuiltInSlotTypes' {nextToken} -> nextToken) (\s@ListBuiltInSlotTypes' {} a -> s {nextToken = a} :: ListBuiltInSlotTypes)

-- | Determines the sort order for the response from the
-- @ListBuiltInSlotTypes@ operation. You can choose to sort by the slot
-- type signature in either ascending or descending order.
listBuiltInSlotTypes_sortBy :: Lens.Lens' ListBuiltInSlotTypes (Prelude.Maybe BuiltInSlotTypeSortBy)
listBuiltInSlotTypes_sortBy = Lens.lens (\ListBuiltInSlotTypes' {sortBy} -> sortBy) (\s@ListBuiltInSlotTypes' {} a -> s {sortBy = a} :: ListBuiltInSlotTypes)

-- | The identifier of the language and locale of the slot types to list. The
-- string must match one of the supported locales. For more information,
-- see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
listBuiltInSlotTypes_localeId :: Lens.Lens' ListBuiltInSlotTypes Prelude.Text
listBuiltInSlotTypes_localeId = Lens.lens (\ListBuiltInSlotTypes' {localeId} -> localeId) (\s@ListBuiltInSlotTypes' {} a -> s {localeId = a} :: ListBuiltInSlotTypes)

instance Core.AWSRequest ListBuiltInSlotTypes where
  type
    AWSResponse ListBuiltInSlotTypes =
      ListBuiltInSlotTypesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBuiltInSlotTypesResponse'
            Prelude.<$> ( x
                            Data..?> "builtInSlotTypeSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "localeId")
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListBuiltInSlotTypes where
  hashWithSalt _salt ListBuiltInSlotTypes' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` localeId

instance Prelude.NFData ListBuiltInSlotTypes where
  rnf ListBuiltInSlotTypes' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf localeId

instance Data.ToHeaders ListBuiltInSlotTypes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListBuiltInSlotTypes where
  toJSON ListBuiltInSlotTypes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("sortBy" Data..=) Prelude.<$> sortBy
          ]
      )

instance Data.ToPath ListBuiltInSlotTypes where
  toPath ListBuiltInSlotTypes' {..} =
    Prelude.mconcat
      [ "/builtins/locales/",
        Data.toBS localeId,
        "/slottypes/"
      ]

instance Data.ToQuery ListBuiltInSlotTypes where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListBuiltInSlotTypesResponse' smart constructor.
data ListBuiltInSlotTypesResponse = ListBuiltInSlotTypesResponse'
  { -- | Summary information for the built-in slot types that meet the filter
    -- criteria specified in the request. The length of the list is specified
    -- in the @maxResults@ parameter of the request. If there are more slot
    -- types available, the @nextToken@ field contains a token to get the next
    -- page of results.
    builtInSlotTypeSummaries :: Prelude.Maybe [BuiltInSlotTypeSummary],
    -- | The language and locale of the slot types in the list.
    localeId :: Prelude.Maybe Prelude.Text,
    -- | A token that indicates whether there are more results to return in a
    -- response to the @ListBuiltInSlotTypes@ operation. If the @nextToken@
    -- field is present, you send the contents as the @nextToken@ parameter of
    -- a @LIstBuiltInSlotTypes@ operation request to get the next page of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBuiltInSlotTypesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'builtInSlotTypeSummaries', 'listBuiltInSlotTypesResponse_builtInSlotTypeSummaries' - Summary information for the built-in slot types that meet the filter
-- criteria specified in the request. The length of the list is specified
-- in the @maxResults@ parameter of the request. If there are more slot
-- types available, the @nextToken@ field contains a token to get the next
-- page of results.
--
-- 'localeId', 'listBuiltInSlotTypesResponse_localeId' - The language and locale of the slot types in the list.
--
-- 'nextToken', 'listBuiltInSlotTypesResponse_nextToken' - A token that indicates whether there are more results to return in a
-- response to the @ListBuiltInSlotTypes@ operation. If the @nextToken@
-- field is present, you send the contents as the @nextToken@ parameter of
-- a @LIstBuiltInSlotTypes@ operation request to get the next page of
-- results.
--
-- 'httpStatus', 'listBuiltInSlotTypesResponse_httpStatus' - The response's http status code.
newListBuiltInSlotTypesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListBuiltInSlotTypesResponse
newListBuiltInSlotTypesResponse pHttpStatus_ =
  ListBuiltInSlotTypesResponse'
    { builtInSlotTypeSummaries =
        Prelude.Nothing,
      localeId = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Summary information for the built-in slot types that meet the filter
-- criteria specified in the request. The length of the list is specified
-- in the @maxResults@ parameter of the request. If there are more slot
-- types available, the @nextToken@ field contains a token to get the next
-- page of results.
listBuiltInSlotTypesResponse_builtInSlotTypeSummaries :: Lens.Lens' ListBuiltInSlotTypesResponse (Prelude.Maybe [BuiltInSlotTypeSummary])
listBuiltInSlotTypesResponse_builtInSlotTypeSummaries = Lens.lens (\ListBuiltInSlotTypesResponse' {builtInSlotTypeSummaries} -> builtInSlotTypeSummaries) (\s@ListBuiltInSlotTypesResponse' {} a -> s {builtInSlotTypeSummaries = a} :: ListBuiltInSlotTypesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The language and locale of the slot types in the list.
listBuiltInSlotTypesResponse_localeId :: Lens.Lens' ListBuiltInSlotTypesResponse (Prelude.Maybe Prelude.Text)
listBuiltInSlotTypesResponse_localeId = Lens.lens (\ListBuiltInSlotTypesResponse' {localeId} -> localeId) (\s@ListBuiltInSlotTypesResponse' {} a -> s {localeId = a} :: ListBuiltInSlotTypesResponse)

-- | A token that indicates whether there are more results to return in a
-- response to the @ListBuiltInSlotTypes@ operation. If the @nextToken@
-- field is present, you send the contents as the @nextToken@ parameter of
-- a @LIstBuiltInSlotTypes@ operation request to get the next page of
-- results.
listBuiltInSlotTypesResponse_nextToken :: Lens.Lens' ListBuiltInSlotTypesResponse (Prelude.Maybe Prelude.Text)
listBuiltInSlotTypesResponse_nextToken = Lens.lens (\ListBuiltInSlotTypesResponse' {nextToken} -> nextToken) (\s@ListBuiltInSlotTypesResponse' {} a -> s {nextToken = a} :: ListBuiltInSlotTypesResponse)

-- | The response's http status code.
listBuiltInSlotTypesResponse_httpStatus :: Lens.Lens' ListBuiltInSlotTypesResponse Prelude.Int
listBuiltInSlotTypesResponse_httpStatus = Lens.lens (\ListBuiltInSlotTypesResponse' {httpStatus} -> httpStatus) (\s@ListBuiltInSlotTypesResponse' {} a -> s {httpStatus = a} :: ListBuiltInSlotTypesResponse)

instance Prelude.NFData ListBuiltInSlotTypesResponse where
  rnf ListBuiltInSlotTypesResponse' {..} =
    Prelude.rnf builtInSlotTypeSummaries
      `Prelude.seq` Prelude.rnf localeId
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
