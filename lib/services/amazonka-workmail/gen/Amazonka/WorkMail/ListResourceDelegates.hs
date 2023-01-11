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
-- Module      : Amazonka.WorkMail.ListResourceDelegates
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the delegates associated with a resource. Users and groups can be
-- resource delegates and answer requests on behalf of the resource.
--
-- This operation returns paginated results.
module Amazonka.WorkMail.ListResourceDelegates
  ( -- * Creating a Request
    ListResourceDelegates (..),
    newListResourceDelegates,

    -- * Request Lenses
    listResourceDelegates_maxResults,
    listResourceDelegates_nextToken,
    listResourceDelegates_organizationId,
    listResourceDelegates_resourceId,

    -- * Destructuring the Response
    ListResourceDelegatesResponse (..),
    newListResourceDelegatesResponse,

    -- * Response Lenses
    listResourceDelegatesResponse_delegates,
    listResourceDelegatesResponse_nextToken,
    listResourceDelegatesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

-- | /See:/ 'newListResourceDelegates' smart constructor.
data ListResourceDelegates = ListResourceDelegates'
  { -- | The number of maximum results in a page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token used to paginate through the delegates associated with a
    -- resource.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the organization that contains the resource for which
    -- delegates are listed.
    organizationId :: Prelude.Text,
    -- | The identifier for the resource whose delegates are listed.
    resourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResourceDelegates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listResourceDelegates_maxResults' - The number of maximum results in a page.
--
-- 'nextToken', 'listResourceDelegates_nextToken' - The token used to paginate through the delegates associated with a
-- resource.
--
-- 'organizationId', 'listResourceDelegates_organizationId' - The identifier for the organization that contains the resource for which
-- delegates are listed.
--
-- 'resourceId', 'listResourceDelegates_resourceId' - The identifier for the resource whose delegates are listed.
newListResourceDelegates ::
  -- | 'organizationId'
  Prelude.Text ->
  -- | 'resourceId'
  Prelude.Text ->
  ListResourceDelegates
newListResourceDelegates
  pOrganizationId_
  pResourceId_ =
    ListResourceDelegates'
      { maxResults =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        organizationId = pOrganizationId_,
        resourceId = pResourceId_
      }

-- | The number of maximum results in a page.
listResourceDelegates_maxResults :: Lens.Lens' ListResourceDelegates (Prelude.Maybe Prelude.Natural)
listResourceDelegates_maxResults = Lens.lens (\ListResourceDelegates' {maxResults} -> maxResults) (\s@ListResourceDelegates' {} a -> s {maxResults = a} :: ListResourceDelegates)

-- | The token used to paginate through the delegates associated with a
-- resource.
listResourceDelegates_nextToken :: Lens.Lens' ListResourceDelegates (Prelude.Maybe Prelude.Text)
listResourceDelegates_nextToken = Lens.lens (\ListResourceDelegates' {nextToken} -> nextToken) (\s@ListResourceDelegates' {} a -> s {nextToken = a} :: ListResourceDelegates)

-- | The identifier for the organization that contains the resource for which
-- delegates are listed.
listResourceDelegates_organizationId :: Lens.Lens' ListResourceDelegates Prelude.Text
listResourceDelegates_organizationId = Lens.lens (\ListResourceDelegates' {organizationId} -> organizationId) (\s@ListResourceDelegates' {} a -> s {organizationId = a} :: ListResourceDelegates)

-- | The identifier for the resource whose delegates are listed.
listResourceDelegates_resourceId :: Lens.Lens' ListResourceDelegates Prelude.Text
listResourceDelegates_resourceId = Lens.lens (\ListResourceDelegates' {resourceId} -> resourceId) (\s@ListResourceDelegates' {} a -> s {resourceId = a} :: ListResourceDelegates)

instance Core.AWSPager ListResourceDelegates where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listResourceDelegatesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listResourceDelegatesResponse_delegates
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listResourceDelegates_nextToken
          Lens..~ rs
          Lens.^? listResourceDelegatesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListResourceDelegates where
  type
    AWSResponse ListResourceDelegates =
      ListResourceDelegatesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListResourceDelegatesResponse'
            Prelude.<$> (x Data..?> "Delegates" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListResourceDelegates where
  hashWithSalt _salt ListResourceDelegates' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` organizationId
      `Prelude.hashWithSalt` resourceId

instance Prelude.NFData ListResourceDelegates where
  rnf ListResourceDelegates' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf organizationId
      `Prelude.seq` Prelude.rnf resourceId

instance Data.ToHeaders ListResourceDelegates where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkMailService.ListResourceDelegates" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListResourceDelegates where
  toJSON ListResourceDelegates' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("OrganizationId" Data..= organizationId),
            Prelude.Just ("ResourceId" Data..= resourceId)
          ]
      )

instance Data.ToPath ListResourceDelegates where
  toPath = Prelude.const "/"

instance Data.ToQuery ListResourceDelegates where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListResourceDelegatesResponse' smart constructor.
data ListResourceDelegatesResponse = ListResourceDelegatesResponse'
  { -- | One page of the resource\'s delegates.
    delegates :: Prelude.Maybe [Delegate],
    -- | The token used to paginate through the delegates associated with a
    -- resource. While results are still available, it has an associated value.
    -- When the last page is reached, the token is empty.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResourceDelegatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'delegates', 'listResourceDelegatesResponse_delegates' - One page of the resource\'s delegates.
--
-- 'nextToken', 'listResourceDelegatesResponse_nextToken' - The token used to paginate through the delegates associated with a
-- resource. While results are still available, it has an associated value.
-- When the last page is reached, the token is empty.
--
-- 'httpStatus', 'listResourceDelegatesResponse_httpStatus' - The response's http status code.
newListResourceDelegatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListResourceDelegatesResponse
newListResourceDelegatesResponse pHttpStatus_ =
  ListResourceDelegatesResponse'
    { delegates =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | One page of the resource\'s delegates.
listResourceDelegatesResponse_delegates :: Lens.Lens' ListResourceDelegatesResponse (Prelude.Maybe [Delegate])
listResourceDelegatesResponse_delegates = Lens.lens (\ListResourceDelegatesResponse' {delegates} -> delegates) (\s@ListResourceDelegatesResponse' {} a -> s {delegates = a} :: ListResourceDelegatesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token used to paginate through the delegates associated with a
-- resource. While results are still available, it has an associated value.
-- When the last page is reached, the token is empty.
listResourceDelegatesResponse_nextToken :: Lens.Lens' ListResourceDelegatesResponse (Prelude.Maybe Prelude.Text)
listResourceDelegatesResponse_nextToken = Lens.lens (\ListResourceDelegatesResponse' {nextToken} -> nextToken) (\s@ListResourceDelegatesResponse' {} a -> s {nextToken = a} :: ListResourceDelegatesResponse)

-- | The response's http status code.
listResourceDelegatesResponse_httpStatus :: Lens.Lens' ListResourceDelegatesResponse Prelude.Int
listResourceDelegatesResponse_httpStatus = Lens.lens (\ListResourceDelegatesResponse' {httpStatus} -> httpStatus) (\s@ListResourceDelegatesResponse' {} a -> s {httpStatus = a} :: ListResourceDelegatesResponse)

instance Prelude.NFData ListResourceDelegatesResponse where
  rnf ListResourceDelegatesResponse' {..} =
    Prelude.rnf delegates
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
