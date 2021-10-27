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
-- Module      : Network.AWS.Backup.ListProtectedResources
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of resources successfully backed up by Backup,
-- including the time the resource was saved, an Amazon Resource Name (ARN)
-- of the resource, and a resource type.
module Network.AWS.Backup.ListProtectedResources
  ( -- * Creating a Request
    ListProtectedResources (..),
    newListProtectedResources,

    -- * Request Lenses
    listProtectedResources_nextToken,
    listProtectedResources_maxResults,

    -- * Destructuring the Response
    ListProtectedResourcesResponse (..),
    newListProtectedResourcesResponse,

    -- * Response Lenses
    listProtectedResourcesResponse_results,
    listProtectedResourcesResponse_nextToken,
    listProtectedResourcesResponse_httpStatus,
  )
where

import Network.AWS.Backup.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListProtectedResources' smart constructor.
data ListProtectedResources = ListProtectedResources'
  { -- | The next item following a partial list of returned items. For example,
    -- if a request is made to return @maxResults@ number of items, @NextToken@
    -- allows you to return more items in your list starting at the location
    -- pointed to by the next token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to be returned.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProtectedResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listProtectedResources_nextToken' - The next item following a partial list of returned items. For example,
-- if a request is made to return @maxResults@ number of items, @NextToken@
-- allows you to return more items in your list starting at the location
-- pointed to by the next token.
--
-- 'maxResults', 'listProtectedResources_maxResults' - The maximum number of items to be returned.
newListProtectedResources ::
  ListProtectedResources
newListProtectedResources =
  ListProtectedResources'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The next item following a partial list of returned items. For example,
-- if a request is made to return @maxResults@ number of items, @NextToken@
-- allows you to return more items in your list starting at the location
-- pointed to by the next token.
listProtectedResources_nextToken :: Lens.Lens' ListProtectedResources (Prelude.Maybe Prelude.Text)
listProtectedResources_nextToken = Lens.lens (\ListProtectedResources' {nextToken} -> nextToken) (\s@ListProtectedResources' {} a -> s {nextToken = a} :: ListProtectedResources)

-- | The maximum number of items to be returned.
listProtectedResources_maxResults :: Lens.Lens' ListProtectedResources (Prelude.Maybe Prelude.Natural)
listProtectedResources_maxResults = Lens.lens (\ListProtectedResources' {maxResults} -> maxResults) (\s@ListProtectedResources' {} a -> s {maxResults = a} :: ListProtectedResources)

instance Core.AWSRequest ListProtectedResources where
  type
    AWSResponse ListProtectedResources =
      ListProtectedResourcesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProtectedResourcesResponse'
            Prelude.<$> (x Core..?> "Results" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListProtectedResources

instance Prelude.NFData ListProtectedResources

instance Core.ToHeaders ListProtectedResources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListProtectedResources where
  toPath = Prelude.const "/resources/"

instance Core.ToQuery ListProtectedResources where
  toQuery ListProtectedResources' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListProtectedResourcesResponse' smart constructor.
data ListProtectedResourcesResponse = ListProtectedResourcesResponse'
  { -- | An array of resources successfully backed up by Backup including the
    -- time the resource was saved, an Amazon Resource Name (ARN) of the
    -- resource, and a resource type.
    results :: Prelude.Maybe [ProtectedResource],
    -- | The next item following a partial list of returned items. For example,
    -- if a request is made to return @maxResults@ number of items, @NextToken@
    -- allows you to return more items in your list starting at the location
    -- pointed to by the next token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProtectedResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'results', 'listProtectedResourcesResponse_results' - An array of resources successfully backed up by Backup including the
-- time the resource was saved, an Amazon Resource Name (ARN) of the
-- resource, and a resource type.
--
-- 'nextToken', 'listProtectedResourcesResponse_nextToken' - The next item following a partial list of returned items. For example,
-- if a request is made to return @maxResults@ number of items, @NextToken@
-- allows you to return more items in your list starting at the location
-- pointed to by the next token.
--
-- 'httpStatus', 'listProtectedResourcesResponse_httpStatus' - The response's http status code.
newListProtectedResourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListProtectedResourcesResponse
newListProtectedResourcesResponse pHttpStatus_ =
  ListProtectedResourcesResponse'
    { results =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of resources successfully backed up by Backup including the
-- time the resource was saved, an Amazon Resource Name (ARN) of the
-- resource, and a resource type.
listProtectedResourcesResponse_results :: Lens.Lens' ListProtectedResourcesResponse (Prelude.Maybe [ProtectedResource])
listProtectedResourcesResponse_results = Lens.lens (\ListProtectedResourcesResponse' {results} -> results) (\s@ListProtectedResourcesResponse' {} a -> s {results = a} :: ListProtectedResourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The next item following a partial list of returned items. For example,
-- if a request is made to return @maxResults@ number of items, @NextToken@
-- allows you to return more items in your list starting at the location
-- pointed to by the next token.
listProtectedResourcesResponse_nextToken :: Lens.Lens' ListProtectedResourcesResponse (Prelude.Maybe Prelude.Text)
listProtectedResourcesResponse_nextToken = Lens.lens (\ListProtectedResourcesResponse' {nextToken} -> nextToken) (\s@ListProtectedResourcesResponse' {} a -> s {nextToken = a} :: ListProtectedResourcesResponse)

-- | The response's http status code.
listProtectedResourcesResponse_httpStatus :: Lens.Lens' ListProtectedResourcesResponse Prelude.Int
listProtectedResourcesResponse_httpStatus = Lens.lens (\ListProtectedResourcesResponse' {httpStatus} -> httpStatus) (\s@ListProtectedResourcesResponse' {} a -> s {httpStatus = a} :: ListProtectedResourcesResponse)

instance
  Prelude.NFData
    ListProtectedResourcesResponse
