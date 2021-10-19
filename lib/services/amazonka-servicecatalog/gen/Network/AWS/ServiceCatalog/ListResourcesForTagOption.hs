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
    listResourcesForTagOption_resourceType,
    listResourcesForTagOption_pageToken,
    listResourcesForTagOption_pageSize,
    listResourcesForTagOption_tagOptionId,

    -- * Destructuring the Response
    ListResourcesForTagOptionResponse (..),
    newListResourcesForTagOptionResponse,

    -- * Response Lenses
    listResourcesForTagOptionResponse_resourceDetails,
    listResourcesForTagOptionResponse_pageToken,
    listResourcesForTagOptionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newListResourcesForTagOption' smart constructor.
data ListResourcesForTagOption = ListResourcesForTagOption'
  { -- | The resource type.
    --
    -- -   @Portfolio@
    --
    -- -   @Product@
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | The page token for the next set of results. To retrieve the first set of
    -- results, use null.
    pageToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return with this call.
    pageSize :: Prelude.Maybe Prelude.Natural,
    -- | The TagOption identifier.
    tagOptionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResourcesForTagOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'listResourcesForTagOption_resourceType' - The resource type.
--
-- -   @Portfolio@
--
-- -   @Product@
--
-- 'pageToken', 'listResourcesForTagOption_pageToken' - The page token for the next set of results. To retrieve the first set of
-- results, use null.
--
-- 'pageSize', 'listResourcesForTagOption_pageSize' - The maximum number of items to return with this call.
--
-- 'tagOptionId', 'listResourcesForTagOption_tagOptionId' - The TagOption identifier.
newListResourcesForTagOption ::
  -- | 'tagOptionId'
  Prelude.Text ->
  ListResourcesForTagOption
newListResourcesForTagOption pTagOptionId_ =
  ListResourcesForTagOption'
    { resourceType =
        Prelude.Nothing,
      pageToken = Prelude.Nothing,
      pageSize = Prelude.Nothing,
      tagOptionId = pTagOptionId_
    }

-- | The resource type.
--
-- -   @Portfolio@
--
-- -   @Product@
listResourcesForTagOption_resourceType :: Lens.Lens' ListResourcesForTagOption (Prelude.Maybe Prelude.Text)
listResourcesForTagOption_resourceType = Lens.lens (\ListResourcesForTagOption' {resourceType} -> resourceType) (\s@ListResourcesForTagOption' {} a -> s {resourceType = a} :: ListResourcesForTagOption)

-- | The page token for the next set of results. To retrieve the first set of
-- results, use null.
listResourcesForTagOption_pageToken :: Lens.Lens' ListResourcesForTagOption (Prelude.Maybe Prelude.Text)
listResourcesForTagOption_pageToken = Lens.lens (\ListResourcesForTagOption' {pageToken} -> pageToken) (\s@ListResourcesForTagOption' {} a -> s {pageToken = a} :: ListResourcesForTagOption)

-- | The maximum number of items to return with this call.
listResourcesForTagOption_pageSize :: Lens.Lens' ListResourcesForTagOption (Prelude.Maybe Prelude.Natural)
listResourcesForTagOption_pageSize = Lens.lens (\ListResourcesForTagOption' {pageSize} -> pageSize) (\s@ListResourcesForTagOption' {} a -> s {pageSize = a} :: ListResourcesForTagOption)

-- | The TagOption identifier.
listResourcesForTagOption_tagOptionId :: Lens.Lens' ListResourcesForTagOption Prelude.Text
listResourcesForTagOption_tagOptionId = Lens.lens (\ListResourcesForTagOption' {tagOptionId} -> tagOptionId) (\s@ListResourcesForTagOption' {} a -> s {tagOptionId = a} :: ListResourcesForTagOption)

instance Core.AWSPager ListResourcesForTagOption where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listResourcesForTagOptionResponse_pageToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listResourcesForTagOptionResponse_resourceDetails
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listResourcesForTagOption_pageToken
          Lens..~ rs
          Lens.^? listResourcesForTagOptionResponse_pageToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListResourcesForTagOption where
  type
    AWSResponse ListResourcesForTagOption =
      ListResourcesForTagOptionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListResourcesForTagOptionResponse'
            Prelude.<$> ( x Core..?> "ResourceDetails"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "PageToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListResourcesForTagOption

instance Prelude.NFData ListResourcesForTagOption

instance Core.ToHeaders ListResourcesForTagOption where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.ListResourcesForTagOption" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListResourcesForTagOption where
  toJSON ListResourcesForTagOption' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ResourceType" Core..=) Prelude.<$> resourceType,
            ("PageToken" Core..=) Prelude.<$> pageToken,
            ("PageSize" Core..=) Prelude.<$> pageSize,
            Prelude.Just ("TagOptionId" Core..= tagOptionId)
          ]
      )

instance Core.ToPath ListResourcesForTagOption where
  toPath = Prelude.const "/"

instance Core.ToQuery ListResourcesForTagOption where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListResourcesForTagOptionResponse' smart constructor.
data ListResourcesForTagOptionResponse = ListResourcesForTagOptionResponse'
  { -- | Information about the resources.
    resourceDetails :: Prelude.Maybe [ResourceDetail],
    -- | The page token for the next set of results. To retrieve the first set of
    -- results, use null.
    pageToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResourcesForTagOptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceDetails', 'listResourcesForTagOptionResponse_resourceDetails' - Information about the resources.
--
-- 'pageToken', 'listResourcesForTagOptionResponse_pageToken' - The page token for the next set of results. To retrieve the first set of
-- results, use null.
--
-- 'httpStatus', 'listResourcesForTagOptionResponse_httpStatus' - The response's http status code.
newListResourcesForTagOptionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListResourcesForTagOptionResponse
newListResourcesForTagOptionResponse pHttpStatus_ =
  ListResourcesForTagOptionResponse'
    { resourceDetails =
        Prelude.Nothing,
      pageToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the resources.
listResourcesForTagOptionResponse_resourceDetails :: Lens.Lens' ListResourcesForTagOptionResponse (Prelude.Maybe [ResourceDetail])
listResourcesForTagOptionResponse_resourceDetails = Lens.lens (\ListResourcesForTagOptionResponse' {resourceDetails} -> resourceDetails) (\s@ListResourcesForTagOptionResponse' {} a -> s {resourceDetails = a} :: ListResourcesForTagOptionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The page token for the next set of results. To retrieve the first set of
-- results, use null.
listResourcesForTagOptionResponse_pageToken :: Lens.Lens' ListResourcesForTagOptionResponse (Prelude.Maybe Prelude.Text)
listResourcesForTagOptionResponse_pageToken = Lens.lens (\ListResourcesForTagOptionResponse' {pageToken} -> pageToken) (\s@ListResourcesForTagOptionResponse' {} a -> s {pageToken = a} :: ListResourcesForTagOptionResponse)

-- | The response's http status code.
listResourcesForTagOptionResponse_httpStatus :: Lens.Lens' ListResourcesForTagOptionResponse Prelude.Int
listResourcesForTagOptionResponse_httpStatus = Lens.lens (\ListResourcesForTagOptionResponse' {httpStatus} -> httpStatus) (\s@ListResourcesForTagOptionResponse' {} a -> s {httpStatus = a} :: ListResourcesForTagOptionResponse)

instance
  Prelude.NFData
    ListResourcesForTagOptionResponse
