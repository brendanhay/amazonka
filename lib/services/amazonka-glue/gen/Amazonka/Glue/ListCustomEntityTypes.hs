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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    listCustomEntityTypes_maxResults,
    listCustomEntityTypes_nextToken,
    listCustomEntityTypes_tags,

    -- * Destructuring the Response
    ListCustomEntityTypesResponse (..),
    newListCustomEntityTypesResponse,

    -- * Response Lenses
    listCustomEntityTypesResponse_customEntityTypes,
    listCustomEntityTypesResponse_nextToken,
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
  { -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A paginated token to offset the results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of key-value pair tags.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
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
-- 'maxResults', 'listCustomEntityTypes_maxResults' - The maximum number of results to return.
--
-- 'nextToken', 'listCustomEntityTypes_nextToken' - A paginated token to offset the results.
--
-- 'tags', 'listCustomEntityTypes_tags' - A list of key-value pair tags.
newListCustomEntityTypes ::
  ListCustomEntityTypes
newListCustomEntityTypes =
  ListCustomEntityTypes'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The maximum number of results to return.
listCustomEntityTypes_maxResults :: Lens.Lens' ListCustomEntityTypes (Prelude.Maybe Prelude.Natural)
listCustomEntityTypes_maxResults = Lens.lens (\ListCustomEntityTypes' {maxResults} -> maxResults) (\s@ListCustomEntityTypes' {} a -> s {maxResults = a} :: ListCustomEntityTypes)

-- | A paginated token to offset the results.
listCustomEntityTypes_nextToken :: Lens.Lens' ListCustomEntityTypes (Prelude.Maybe Prelude.Text)
listCustomEntityTypes_nextToken = Lens.lens (\ListCustomEntityTypes' {nextToken} -> nextToken) (\s@ListCustomEntityTypes' {} a -> s {nextToken = a} :: ListCustomEntityTypes)

-- | A list of key-value pair tags.
listCustomEntityTypes_tags :: Lens.Lens' ListCustomEntityTypes (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
listCustomEntityTypes_tags = Lens.lens (\ListCustomEntityTypes' {tags} -> tags) (\s@ListCustomEntityTypes' {} a -> s {tags = a} :: ListCustomEntityTypes) Prelude.. Lens.mapping Lens.coerced

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
            Prelude.<$> ( x
                            Data..?> "CustomEntityTypes"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCustomEntityTypes where
  hashWithSalt _salt ListCustomEntityTypes' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` tags

instance Prelude.NFData ListCustomEntityTypes where
  rnf ListCustomEntityTypes' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf tags

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
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("Tags" Data..=) Prelude.<$> tags
          ]
      )

instance Data.ToPath ListCustomEntityTypes where
  toPath = Prelude.const "/"

instance Data.ToQuery ListCustomEntityTypes where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListCustomEntityTypesResponse' smart constructor.
data ListCustomEntityTypesResponse = ListCustomEntityTypesResponse'
  { -- | A list of @CustomEntityType@ objects representing custom patterns.
    customEntityTypes :: Prelude.Maybe [CustomEntityType],
    -- | A pagination token, if more results are available.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'customEntityTypes', 'listCustomEntityTypesResponse_customEntityTypes' - A list of @CustomEntityType@ objects representing custom patterns.
--
-- 'nextToken', 'listCustomEntityTypesResponse_nextToken' - A pagination token, if more results are available.
--
-- 'httpStatus', 'listCustomEntityTypesResponse_httpStatus' - The response's http status code.
newListCustomEntityTypesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCustomEntityTypesResponse
newListCustomEntityTypesResponse pHttpStatus_ =
  ListCustomEntityTypesResponse'
    { customEntityTypes =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of @CustomEntityType@ objects representing custom patterns.
listCustomEntityTypesResponse_customEntityTypes :: Lens.Lens' ListCustomEntityTypesResponse (Prelude.Maybe [CustomEntityType])
listCustomEntityTypesResponse_customEntityTypes = Lens.lens (\ListCustomEntityTypesResponse' {customEntityTypes} -> customEntityTypes) (\s@ListCustomEntityTypesResponse' {} a -> s {customEntityTypes = a} :: ListCustomEntityTypesResponse) Prelude.. Lens.mapping Lens.coerced

-- | A pagination token, if more results are available.
listCustomEntityTypesResponse_nextToken :: Lens.Lens' ListCustomEntityTypesResponse (Prelude.Maybe Prelude.Text)
listCustomEntityTypesResponse_nextToken = Lens.lens (\ListCustomEntityTypesResponse' {nextToken} -> nextToken) (\s@ListCustomEntityTypesResponse' {} a -> s {nextToken = a} :: ListCustomEntityTypesResponse)

-- | The response's http status code.
listCustomEntityTypesResponse_httpStatus :: Lens.Lens' ListCustomEntityTypesResponse Prelude.Int
listCustomEntityTypesResponse_httpStatus = Lens.lens (\ListCustomEntityTypesResponse' {httpStatus} -> httpStatus) (\s@ListCustomEntityTypesResponse' {} a -> s {httpStatus = a} :: ListCustomEntityTypesResponse)

instance Prelude.NFData ListCustomEntityTypesResponse where
  rnf ListCustomEntityTypesResponse' {..} =
    Prelude.rnf customEntityTypes
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
