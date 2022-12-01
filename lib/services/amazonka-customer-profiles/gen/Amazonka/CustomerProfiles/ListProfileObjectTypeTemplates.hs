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
-- Module      : Amazonka.CustomerProfiles.ListProfileObjectTypeTemplates
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the template information for object types.
module Amazonka.CustomerProfiles.ListProfileObjectTypeTemplates
  ( -- * Creating a Request
    ListProfileObjectTypeTemplates (..),
    newListProfileObjectTypeTemplates,

    -- * Request Lenses
    listProfileObjectTypeTemplates_nextToken,
    listProfileObjectTypeTemplates_maxResults,

    -- * Destructuring the Response
    ListProfileObjectTypeTemplatesResponse (..),
    newListProfileObjectTypeTemplatesResponse,

    -- * Response Lenses
    listProfileObjectTypeTemplatesResponse_items,
    listProfileObjectTypeTemplatesResponse_nextToken,
    listProfileObjectTypeTemplatesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListProfileObjectTypeTemplates' smart constructor.
data ListProfileObjectTypeTemplates = ListProfileObjectTypeTemplates'
  { -- | The pagination token from the previous ListObjectTypeTemplates API call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of objects returned per page.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProfileObjectTypeTemplates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listProfileObjectTypeTemplates_nextToken' - The pagination token from the previous ListObjectTypeTemplates API call.
--
-- 'maxResults', 'listProfileObjectTypeTemplates_maxResults' - The maximum number of objects returned per page.
newListProfileObjectTypeTemplates ::
  ListProfileObjectTypeTemplates
newListProfileObjectTypeTemplates =
  ListProfileObjectTypeTemplates'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The pagination token from the previous ListObjectTypeTemplates API call.
listProfileObjectTypeTemplates_nextToken :: Lens.Lens' ListProfileObjectTypeTemplates (Prelude.Maybe Prelude.Text)
listProfileObjectTypeTemplates_nextToken = Lens.lens (\ListProfileObjectTypeTemplates' {nextToken} -> nextToken) (\s@ListProfileObjectTypeTemplates' {} a -> s {nextToken = a} :: ListProfileObjectTypeTemplates)

-- | The maximum number of objects returned per page.
listProfileObjectTypeTemplates_maxResults :: Lens.Lens' ListProfileObjectTypeTemplates (Prelude.Maybe Prelude.Natural)
listProfileObjectTypeTemplates_maxResults = Lens.lens (\ListProfileObjectTypeTemplates' {maxResults} -> maxResults) (\s@ListProfileObjectTypeTemplates' {} a -> s {maxResults = a} :: ListProfileObjectTypeTemplates)

instance
  Core.AWSRequest
    ListProfileObjectTypeTemplates
  where
  type
    AWSResponse ListProfileObjectTypeTemplates =
      ListProfileObjectTypeTemplatesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProfileObjectTypeTemplatesResponse'
            Prelude.<$> (x Core..?> "Items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListProfileObjectTypeTemplates
  where
  hashWithSalt
    _salt
    ListProfileObjectTypeTemplates' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` maxResults

instance
  Prelude.NFData
    ListProfileObjectTypeTemplates
  where
  rnf ListProfileObjectTypeTemplates' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance
  Core.ToHeaders
    ListProfileObjectTypeTemplates
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListProfileObjectTypeTemplates where
  toPath = Prelude.const "/templates"

instance Core.ToQuery ListProfileObjectTypeTemplates where
  toQuery ListProfileObjectTypeTemplates' {..} =
    Prelude.mconcat
      [ "next-token" Core.=: nextToken,
        "max-results" Core.=: maxResults
      ]

-- | /See:/ 'newListProfileObjectTypeTemplatesResponse' smart constructor.
data ListProfileObjectTypeTemplatesResponse = ListProfileObjectTypeTemplatesResponse'
  { -- | The list of ListProfileObjectType template instances.
    items :: Prelude.Maybe [ListProfileObjectTypeTemplateItem],
    -- | The pagination token from the previous ListObjectTypeTemplates API call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProfileObjectTypeTemplatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'listProfileObjectTypeTemplatesResponse_items' - The list of ListProfileObjectType template instances.
--
-- 'nextToken', 'listProfileObjectTypeTemplatesResponse_nextToken' - The pagination token from the previous ListObjectTypeTemplates API call.
--
-- 'httpStatus', 'listProfileObjectTypeTemplatesResponse_httpStatus' - The response's http status code.
newListProfileObjectTypeTemplatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListProfileObjectTypeTemplatesResponse
newListProfileObjectTypeTemplatesResponse
  pHttpStatus_ =
    ListProfileObjectTypeTemplatesResponse'
      { items =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The list of ListProfileObjectType template instances.
listProfileObjectTypeTemplatesResponse_items :: Lens.Lens' ListProfileObjectTypeTemplatesResponse (Prelude.Maybe [ListProfileObjectTypeTemplateItem])
listProfileObjectTypeTemplatesResponse_items = Lens.lens (\ListProfileObjectTypeTemplatesResponse' {items} -> items) (\s@ListProfileObjectTypeTemplatesResponse' {} a -> s {items = a} :: ListProfileObjectTypeTemplatesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token from the previous ListObjectTypeTemplates API call.
listProfileObjectTypeTemplatesResponse_nextToken :: Lens.Lens' ListProfileObjectTypeTemplatesResponse (Prelude.Maybe Prelude.Text)
listProfileObjectTypeTemplatesResponse_nextToken = Lens.lens (\ListProfileObjectTypeTemplatesResponse' {nextToken} -> nextToken) (\s@ListProfileObjectTypeTemplatesResponse' {} a -> s {nextToken = a} :: ListProfileObjectTypeTemplatesResponse)

-- | The response's http status code.
listProfileObjectTypeTemplatesResponse_httpStatus :: Lens.Lens' ListProfileObjectTypeTemplatesResponse Prelude.Int
listProfileObjectTypeTemplatesResponse_httpStatus = Lens.lens (\ListProfileObjectTypeTemplatesResponse' {httpStatus} -> httpStatus) (\s@ListProfileObjectTypeTemplatesResponse' {} a -> s {httpStatus = a} :: ListProfileObjectTypeTemplatesResponse)

instance
  Prelude.NFData
    ListProfileObjectTypeTemplatesResponse
  where
  rnf ListProfileObjectTypeTemplatesResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
