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
-- Module      : Amazonka.Glue.ListCustomEntityTypes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the custom patterns that have been created.
module Amazonka.Glue.ListCustomEntityTypes
  ( -- * Creating a Request
    ListCustomEntityTypes (..),
    newListCustomEntityTypes,

    -- * Request Lenses
    listCustomEntityTypes_nextToken,
    listCustomEntityTypes_maxResults,

    -- * Destructuring the Response
    ListCustomEntityTypesResponse (..),
    newListCustomEntityTypesResponse,

    -- * Response Lenses
    listCustomEntityTypesResponse_nextToken,
    listCustomEntityTypesResponse_customEntityTypes,
    listCustomEntityTypesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListCustomEntityTypes' smart constructor.
data ListCustomEntityTypes = ListCustomEntityTypes'
  { -- | A paginated token to offset the results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCustomEntityTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCustomEntityTypes_nextToken' - A paginated token to offset the results.
--
-- 'maxResults', 'listCustomEntityTypes_maxResults' - The maximum number of results to return.
newListCustomEntityTypes ::
  ListCustomEntityTypes
newListCustomEntityTypes =
  ListCustomEntityTypes'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | A paginated token to offset the results.
listCustomEntityTypes_nextToken :: Lens.Lens' ListCustomEntityTypes (Prelude.Maybe Prelude.Text)
listCustomEntityTypes_nextToken = Lens.lens (\ListCustomEntityTypes' {nextToken} -> nextToken) (\s@ListCustomEntityTypes' {} a -> s {nextToken = a} :: ListCustomEntityTypes)

-- | The maximum number of results to return.
listCustomEntityTypes_maxResults :: Lens.Lens' ListCustomEntityTypes (Prelude.Maybe Prelude.Natural)
listCustomEntityTypes_maxResults = Lens.lens (\ListCustomEntityTypes' {maxResults} -> maxResults) (\s@ListCustomEntityTypes' {} a -> s {maxResults = a} :: ListCustomEntityTypes)

instance Core.AWSRequest ListCustomEntityTypes where
  type
    AWSResponse ListCustomEntityTypes =
      ListCustomEntityTypesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCustomEntityTypesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "CustomEntityTypes"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCustomEntityTypes where
  hashWithSalt _salt ListCustomEntityTypes' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListCustomEntityTypes where
  rnf ListCustomEntityTypes' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListCustomEntityTypes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSGlue.ListCustomEntityTypes" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListCustomEntityTypes where
  toJSON ListCustomEntityTypes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("MaxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath ListCustomEntityTypes where
  toPath = Prelude.const "/"

instance Data.ToQuery ListCustomEntityTypes where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListCustomEntityTypesResponse' smart constructor.
data ListCustomEntityTypesResponse = ListCustomEntityTypesResponse'
  { -- | A pagination token, if more results are available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of @CustomEntityType@ objects representing custom patterns.
    customEntityTypes :: Prelude.Maybe [CustomEntityType],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCustomEntityTypesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCustomEntityTypesResponse_nextToken' - A pagination token, if more results are available.
--
-- 'customEntityTypes', 'listCustomEntityTypesResponse_customEntityTypes' - A list of @CustomEntityType@ objects representing custom patterns.
--
-- 'httpStatus', 'listCustomEntityTypesResponse_httpStatus' - The response's http status code.
newListCustomEntityTypesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCustomEntityTypesResponse
newListCustomEntityTypesResponse pHttpStatus_ =
  ListCustomEntityTypesResponse'
    { nextToken =
        Prelude.Nothing,
      customEntityTypes = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A pagination token, if more results are available.
listCustomEntityTypesResponse_nextToken :: Lens.Lens' ListCustomEntityTypesResponse (Prelude.Maybe Prelude.Text)
listCustomEntityTypesResponse_nextToken = Lens.lens (\ListCustomEntityTypesResponse' {nextToken} -> nextToken) (\s@ListCustomEntityTypesResponse' {} a -> s {nextToken = a} :: ListCustomEntityTypesResponse)

-- | A list of @CustomEntityType@ objects representing custom patterns.
listCustomEntityTypesResponse_customEntityTypes :: Lens.Lens' ListCustomEntityTypesResponse (Prelude.Maybe [CustomEntityType])
listCustomEntityTypesResponse_customEntityTypes = Lens.lens (\ListCustomEntityTypesResponse' {customEntityTypes} -> customEntityTypes) (\s@ListCustomEntityTypesResponse' {} a -> s {customEntityTypes = a} :: ListCustomEntityTypesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listCustomEntityTypesResponse_httpStatus :: Lens.Lens' ListCustomEntityTypesResponse Prelude.Int
listCustomEntityTypesResponse_httpStatus = Lens.lens (\ListCustomEntityTypesResponse' {httpStatus} -> httpStatus) (\s@ListCustomEntityTypesResponse' {} a -> s {httpStatus = a} :: ListCustomEntityTypesResponse)

instance Prelude.NFData ListCustomEntityTypesResponse where
  rnf ListCustomEntityTypesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf customEntityTypes
      `Prelude.seq` Prelude.rnf httpStatus
