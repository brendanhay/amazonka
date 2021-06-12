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
-- Module      : Network.AWS.Glue.ListRegistries
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of registries that you have created, with minimal
-- registry information. Registries in the @Deleting@ status will not be
-- included in the results. Empty results will be returned if there are no
-- registries available.
--
-- This operation returns paginated results.
module Network.AWS.Glue.ListRegistries
  ( -- * Creating a Request
    ListRegistries (..),
    newListRegistries,

    -- * Request Lenses
    listRegistries_nextToken,
    listRegistries_maxResults,

    -- * Destructuring the Response
    ListRegistriesResponse (..),
    newListRegistriesResponse,

    -- * Response Lenses
    listRegistriesResponse_nextToken,
    listRegistriesResponse_registries,
    listRegistriesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListRegistries' smart constructor.
data ListRegistries = ListRegistries'
  { -- | A continuation token, if this is a continuation call.
    nextToken :: Core.Maybe Core.Text,
    -- | Maximum number of results required per page. If the value is not
    -- supplied, this will be defaulted to 25 per page.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListRegistries' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRegistries_nextToken' - A continuation token, if this is a continuation call.
--
-- 'maxResults', 'listRegistries_maxResults' - Maximum number of results required per page. If the value is not
-- supplied, this will be defaulted to 25 per page.
newListRegistries ::
  ListRegistries
newListRegistries =
  ListRegistries'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | A continuation token, if this is a continuation call.
listRegistries_nextToken :: Lens.Lens' ListRegistries (Core.Maybe Core.Text)
listRegistries_nextToken = Lens.lens (\ListRegistries' {nextToken} -> nextToken) (\s@ListRegistries' {} a -> s {nextToken = a} :: ListRegistries)

-- | Maximum number of results required per page. If the value is not
-- supplied, this will be defaulted to 25 per page.
listRegistries_maxResults :: Lens.Lens' ListRegistries (Core.Maybe Core.Natural)
listRegistries_maxResults = Lens.lens (\ListRegistries' {maxResults} -> maxResults) (\s@ListRegistries' {} a -> s {maxResults = a} :: ListRegistries)

instance Core.AWSPager ListRegistries where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRegistriesResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listRegistriesResponse_registries Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listRegistries_nextToken
          Lens..~ rs
          Lens.^? listRegistriesResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListRegistries where
  type
    AWSResponse ListRegistries =
      ListRegistriesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRegistriesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Registries" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListRegistries

instance Core.NFData ListRegistries

instance Core.ToHeaders ListRegistries where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.ListRegistries" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListRegistries where
  toJSON ListRegistries' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults
          ]
      )

instance Core.ToPath ListRegistries where
  toPath = Core.const "/"

instance Core.ToQuery ListRegistries where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListRegistriesResponse' smart constructor.
data ListRegistriesResponse = ListRegistriesResponse'
  { -- | A continuation token for paginating the returned list of tokens,
    -- returned if the current segment of the list is not the last.
    nextToken :: Core.Maybe Core.Text,
    -- | An array of @RegistryDetailedListItem@ objects containing minimal
    -- details of each registry.
    registries :: Core.Maybe [RegistryListItem],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListRegistriesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRegistriesResponse_nextToken' - A continuation token for paginating the returned list of tokens,
-- returned if the current segment of the list is not the last.
--
-- 'registries', 'listRegistriesResponse_registries' - An array of @RegistryDetailedListItem@ objects containing minimal
-- details of each registry.
--
-- 'httpStatus', 'listRegistriesResponse_httpStatus' - The response's http status code.
newListRegistriesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListRegistriesResponse
newListRegistriesResponse pHttpStatus_ =
  ListRegistriesResponse'
    { nextToken = Core.Nothing,
      registries = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A continuation token for paginating the returned list of tokens,
-- returned if the current segment of the list is not the last.
listRegistriesResponse_nextToken :: Lens.Lens' ListRegistriesResponse (Core.Maybe Core.Text)
listRegistriesResponse_nextToken = Lens.lens (\ListRegistriesResponse' {nextToken} -> nextToken) (\s@ListRegistriesResponse' {} a -> s {nextToken = a} :: ListRegistriesResponse)

-- | An array of @RegistryDetailedListItem@ objects containing minimal
-- details of each registry.
listRegistriesResponse_registries :: Lens.Lens' ListRegistriesResponse (Core.Maybe [RegistryListItem])
listRegistriesResponse_registries = Lens.lens (\ListRegistriesResponse' {registries} -> registries) (\s@ListRegistriesResponse' {} a -> s {registries = a} :: ListRegistriesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listRegistriesResponse_httpStatus :: Lens.Lens' ListRegistriesResponse Core.Int
listRegistriesResponse_httpStatus = Lens.lens (\ListRegistriesResponse' {httpStatus} -> httpStatus) (\s@ListRegistriesResponse' {} a -> s {httpStatus = a} :: ListRegistriesResponse)

instance Core.NFData ListRegistriesResponse
