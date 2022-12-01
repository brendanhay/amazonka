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
-- Module      : Amazonka.BackupGateway.ListHypervisors
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your hypervisors.
--
-- This operation returns paginated results.
module Amazonka.BackupGateway.ListHypervisors
  ( -- * Creating a Request
    ListHypervisors (..),
    newListHypervisors,

    -- * Request Lenses
    listHypervisors_nextToken,
    listHypervisors_maxResults,

    -- * Destructuring the Response
    ListHypervisorsResponse (..),
    newListHypervisorsResponse,

    -- * Response Lenses
    listHypervisorsResponse_nextToken,
    listHypervisorsResponse_hypervisors,
    listHypervisorsResponse_httpStatus,
  )
where

import Amazonka.BackupGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListHypervisors' smart constructor.
data ListHypervisors = ListHypervisors'
  { -- | The next item following a partial list of returned resources. For
    -- example, if a request is made to return @maxResults@ number of
    -- resources, @NextToken@ allows you to return more items in your list
    -- starting at the location pointed to by the next token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of hypervisors to list.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListHypervisors' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listHypervisors_nextToken' - The next item following a partial list of returned resources. For
-- example, if a request is made to return @maxResults@ number of
-- resources, @NextToken@ allows you to return more items in your list
-- starting at the location pointed to by the next token.
--
-- 'maxResults', 'listHypervisors_maxResults' - The maximum number of hypervisors to list.
newListHypervisors ::
  ListHypervisors
newListHypervisors =
  ListHypervisors'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The next item following a partial list of returned resources. For
-- example, if a request is made to return @maxResults@ number of
-- resources, @NextToken@ allows you to return more items in your list
-- starting at the location pointed to by the next token.
listHypervisors_nextToken :: Lens.Lens' ListHypervisors (Prelude.Maybe Prelude.Text)
listHypervisors_nextToken = Lens.lens (\ListHypervisors' {nextToken} -> nextToken) (\s@ListHypervisors' {} a -> s {nextToken = a} :: ListHypervisors)

-- | The maximum number of hypervisors to list.
listHypervisors_maxResults :: Lens.Lens' ListHypervisors (Prelude.Maybe Prelude.Natural)
listHypervisors_maxResults = Lens.lens (\ListHypervisors' {maxResults} -> maxResults) (\s@ListHypervisors' {} a -> s {maxResults = a} :: ListHypervisors)

instance Core.AWSPager ListHypervisors where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listHypervisorsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listHypervisorsResponse_hypervisors
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listHypervisors_nextToken
          Lens..~ rs
          Lens.^? listHypervisorsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListHypervisors where
  type
    AWSResponse ListHypervisors =
      ListHypervisorsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListHypervisorsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Hypervisors" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListHypervisors where
  hashWithSalt _salt ListHypervisors' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListHypervisors where
  rnf ListHypervisors' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListHypervisors where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "BackupOnPremises_v20210101.ListHypervisors" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListHypervisors where
  toJSON ListHypervisors' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListHypervisors where
  toPath = Prelude.const "/"

instance Core.ToQuery ListHypervisors where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListHypervisorsResponse' smart constructor.
data ListHypervisorsResponse = ListHypervisorsResponse'
  { -- | The next item following a partial list of returned resources. For
    -- example, if a request is made to return @maxResults@ number of
    -- resources, @NextToken@ allows you to return more items in your list
    -- starting at the location pointed to by the next token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of your @Hypervisor@ objects, ordered by their Amazon Resource
    -- Names (ARNs).
    hypervisors :: Prelude.Maybe [Hypervisor],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListHypervisorsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listHypervisorsResponse_nextToken' - The next item following a partial list of returned resources. For
-- example, if a request is made to return @maxResults@ number of
-- resources, @NextToken@ allows you to return more items in your list
-- starting at the location pointed to by the next token.
--
-- 'hypervisors', 'listHypervisorsResponse_hypervisors' - A list of your @Hypervisor@ objects, ordered by their Amazon Resource
-- Names (ARNs).
--
-- 'httpStatus', 'listHypervisorsResponse_httpStatus' - The response's http status code.
newListHypervisorsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListHypervisorsResponse
newListHypervisorsResponse pHttpStatus_ =
  ListHypervisorsResponse'
    { nextToken =
        Prelude.Nothing,
      hypervisors = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The next item following a partial list of returned resources. For
-- example, if a request is made to return @maxResults@ number of
-- resources, @NextToken@ allows you to return more items in your list
-- starting at the location pointed to by the next token.
listHypervisorsResponse_nextToken :: Lens.Lens' ListHypervisorsResponse (Prelude.Maybe Prelude.Text)
listHypervisorsResponse_nextToken = Lens.lens (\ListHypervisorsResponse' {nextToken} -> nextToken) (\s@ListHypervisorsResponse' {} a -> s {nextToken = a} :: ListHypervisorsResponse)

-- | A list of your @Hypervisor@ objects, ordered by their Amazon Resource
-- Names (ARNs).
listHypervisorsResponse_hypervisors :: Lens.Lens' ListHypervisorsResponse (Prelude.Maybe [Hypervisor])
listHypervisorsResponse_hypervisors = Lens.lens (\ListHypervisorsResponse' {hypervisors} -> hypervisors) (\s@ListHypervisorsResponse' {} a -> s {hypervisors = a} :: ListHypervisorsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listHypervisorsResponse_httpStatus :: Lens.Lens' ListHypervisorsResponse Prelude.Int
listHypervisorsResponse_httpStatus = Lens.lens (\ListHypervisorsResponse' {httpStatus} -> httpStatus) (\s@ListHypervisorsResponse' {} a -> s {httpStatus = a} :: ListHypervisorsResponse)

instance Prelude.NFData ListHypervisorsResponse where
  rnf ListHypervisorsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf hypervisors
      `Prelude.seq` Prelude.rnf httpStatus
