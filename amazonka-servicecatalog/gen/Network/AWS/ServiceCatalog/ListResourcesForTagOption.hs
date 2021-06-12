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
-- Module      : Network.AWS.ServiceCatalog.ListResourcesForTagOption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the resources associated with the specified TagOption.
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalog.ListResourcesForTagOption
  ( -- * Creating a Request
    ListResourcesForTagOption (..),
    newListResourcesForTagOption,

    -- * Request Lenses
    listResourcesForTagOption_pageSize,
    listResourcesForTagOption_pageToken,
    listResourcesForTagOption_resourceType,
    listResourcesForTagOption_tagOptionId,

    -- * Destructuring the Response
    ListResourcesForTagOptionResponse (..),
    newListResourcesForTagOptionResponse,

    -- * Response Lenses
    listResourcesForTagOptionResponse_pageToken,
    listResourcesForTagOptionResponse_resourceDetails,
    listResourcesForTagOptionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newListResourcesForTagOption' smart constructor.
data ListResourcesForTagOption = ListResourcesForTagOption'
  { -- | The maximum number of items to return with this call.
    pageSize :: Core.Maybe Core.Natural,
    -- | The page token for the next set of results. To retrieve the first set of
    -- results, use null.
    pageToken :: Core.Maybe Core.Text,
    -- | The resource type.
    --
    -- -   @Portfolio@
    --
    -- -   @Product@
    resourceType :: Core.Maybe Core.Text,
    -- | The TagOption identifier.
    tagOptionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListResourcesForTagOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageSize', 'listResourcesForTagOption_pageSize' - The maximum number of items to return with this call.
--
-- 'pageToken', 'listResourcesForTagOption_pageToken' - The page token for the next set of results. To retrieve the first set of
-- results, use null.
--
-- 'resourceType', 'listResourcesForTagOption_resourceType' - The resource type.
--
-- -   @Portfolio@
--
-- -   @Product@
--
-- 'tagOptionId', 'listResourcesForTagOption_tagOptionId' - The TagOption identifier.
newListResourcesForTagOption ::
  -- | 'tagOptionId'
  Core.Text ->
  ListResourcesForTagOption
newListResourcesForTagOption pTagOptionId_ =
  ListResourcesForTagOption'
    { pageSize = Core.Nothing,
      pageToken = Core.Nothing,
      resourceType = Core.Nothing,
      tagOptionId = pTagOptionId_
    }

-- | The maximum number of items to return with this call.
listResourcesForTagOption_pageSize :: Lens.Lens' ListResourcesForTagOption (Core.Maybe Core.Natural)
listResourcesForTagOption_pageSize = Lens.lens (\ListResourcesForTagOption' {pageSize} -> pageSize) (\s@ListResourcesForTagOption' {} a -> s {pageSize = a} :: ListResourcesForTagOption)

-- | The page token for the next set of results. To retrieve the first set of
-- results, use null.
listResourcesForTagOption_pageToken :: Lens.Lens' ListResourcesForTagOption (Core.Maybe Core.Text)
listResourcesForTagOption_pageToken = Lens.lens (\ListResourcesForTagOption' {pageToken} -> pageToken) (\s@ListResourcesForTagOption' {} a -> s {pageToken = a} :: ListResourcesForTagOption)

-- | The resource type.
--
-- -   @Portfolio@
--
-- -   @Product@
listResourcesForTagOption_resourceType :: Lens.Lens' ListResourcesForTagOption (Core.Maybe Core.Text)
listResourcesForTagOption_resourceType = Lens.lens (\ListResourcesForTagOption' {resourceType} -> resourceType) (\s@ListResourcesForTagOption' {} a -> s {resourceType = a} :: ListResourcesForTagOption)

-- | The TagOption identifier.
listResourcesForTagOption_tagOptionId :: Lens.Lens' ListResourcesForTagOption Core.Text
listResourcesForTagOption_tagOptionId = Lens.lens (\ListResourcesForTagOption' {tagOptionId} -> tagOptionId) (\s@ListResourcesForTagOption' {} a -> s {tagOptionId = a} :: ListResourcesForTagOption)

instance Core.AWSPager ListResourcesForTagOption where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listResourcesForTagOptionResponse_pageToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listResourcesForTagOptionResponse_resourceDetails
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listResourcesForTagOption_pageToken
          Lens..~ rs
          Lens.^? listResourcesForTagOptionResponse_pageToken
            Core.. Lens._Just

instance Core.AWSRequest ListResourcesForTagOption where
  type
    AWSResponse ListResourcesForTagOption =
      ListResourcesForTagOptionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListResourcesForTagOptionResponse'
            Core.<$> (x Core..?> "PageToken")
            Core.<*> (x Core..?> "ResourceDetails" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListResourcesForTagOption

instance Core.NFData ListResourcesForTagOption

instance Core.ToHeaders ListResourcesForTagOption where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.ListResourcesForTagOption" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListResourcesForTagOption where
  toJSON ListResourcesForTagOption' {..} =
    Core.object
      ( Core.catMaybes
          [ ("PageSize" Core..=) Core.<$> pageSize,
            ("PageToken" Core..=) Core.<$> pageToken,
            ("ResourceType" Core..=) Core.<$> resourceType,
            Core.Just ("TagOptionId" Core..= tagOptionId)
          ]
      )

instance Core.ToPath ListResourcesForTagOption where
  toPath = Core.const "/"

instance Core.ToQuery ListResourcesForTagOption where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListResourcesForTagOptionResponse' smart constructor.
data ListResourcesForTagOptionResponse = ListResourcesForTagOptionResponse'
  { -- | The page token for the next set of results. To retrieve the first set of
    -- results, use null.
    pageToken :: Core.Maybe Core.Text,
    -- | Information about the resources.
    resourceDetails :: Core.Maybe [ResourceDetail],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListResourcesForTagOptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageToken', 'listResourcesForTagOptionResponse_pageToken' - The page token for the next set of results. To retrieve the first set of
-- results, use null.
--
-- 'resourceDetails', 'listResourcesForTagOptionResponse_resourceDetails' - Information about the resources.
--
-- 'httpStatus', 'listResourcesForTagOptionResponse_httpStatus' - The response's http status code.
newListResourcesForTagOptionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListResourcesForTagOptionResponse
newListResourcesForTagOptionResponse pHttpStatus_ =
  ListResourcesForTagOptionResponse'
    { pageToken =
        Core.Nothing,
      resourceDetails = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The page token for the next set of results. To retrieve the first set of
-- results, use null.
listResourcesForTagOptionResponse_pageToken :: Lens.Lens' ListResourcesForTagOptionResponse (Core.Maybe Core.Text)
listResourcesForTagOptionResponse_pageToken = Lens.lens (\ListResourcesForTagOptionResponse' {pageToken} -> pageToken) (\s@ListResourcesForTagOptionResponse' {} a -> s {pageToken = a} :: ListResourcesForTagOptionResponse)

-- | Information about the resources.
listResourcesForTagOptionResponse_resourceDetails :: Lens.Lens' ListResourcesForTagOptionResponse (Core.Maybe [ResourceDetail])
listResourcesForTagOptionResponse_resourceDetails = Lens.lens (\ListResourcesForTagOptionResponse' {resourceDetails} -> resourceDetails) (\s@ListResourcesForTagOptionResponse' {} a -> s {resourceDetails = a} :: ListResourcesForTagOptionResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listResourcesForTagOptionResponse_httpStatus :: Lens.Lens' ListResourcesForTagOptionResponse Core.Int
listResourcesForTagOptionResponse_httpStatus = Lens.lens (\ListResourcesForTagOptionResponse' {httpStatus} -> httpStatus) (\s@ListResourcesForTagOptionResponse' {} a -> s {httpStatus = a} :: ListResourcesForTagOptionResponse)

instance
  Core.NFData
    ListResourcesForTagOptionResponse
