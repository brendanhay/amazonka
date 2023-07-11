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
-- Module      : Amazonka.ServiceCatalog.ListResourcesForTagOption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the resources associated with the specified TagOption.
--
-- This operation returns paginated results.
module Amazonka.ServiceCatalog.ListResourcesForTagOption
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newListResourcesForTagOption' smart constructor.
data ListResourcesForTagOption = ListResourcesForTagOption'
  { -- | The maximum number of items to return with this call.
    pageSize :: Prelude.Maybe Prelude.Natural,
    -- | The page token for the next set of results. To retrieve the first set of
    -- results, use null.
    pageToken :: Prelude.Maybe Prelude.Text,
    -- | The resource type.
    --
    -- -   @Portfolio@
    --
    -- -   @Product@
    resourceType :: Prelude.Maybe Prelude.Text,
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
  Prelude.Text ->
  ListResourcesForTagOption
newListResourcesForTagOption pTagOptionId_ =
  ListResourcesForTagOption'
    { pageSize =
        Prelude.Nothing,
      pageToken = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      tagOptionId = pTagOptionId_
    }

-- | The maximum number of items to return with this call.
listResourcesForTagOption_pageSize :: Lens.Lens' ListResourcesForTagOption (Prelude.Maybe Prelude.Natural)
listResourcesForTagOption_pageSize = Lens.lens (\ListResourcesForTagOption' {pageSize} -> pageSize) (\s@ListResourcesForTagOption' {} a -> s {pageSize = a} :: ListResourcesForTagOption)

-- | The page token for the next set of results. To retrieve the first set of
-- results, use null.
listResourcesForTagOption_pageToken :: Lens.Lens' ListResourcesForTagOption (Prelude.Maybe Prelude.Text)
listResourcesForTagOption_pageToken = Lens.lens (\ListResourcesForTagOption' {pageToken} -> pageToken) (\s@ListResourcesForTagOption' {} a -> s {pageToken = a} :: ListResourcesForTagOption)

-- | The resource type.
--
-- -   @Portfolio@
--
-- -   @Product@
listResourcesForTagOption_resourceType :: Lens.Lens' ListResourcesForTagOption (Prelude.Maybe Prelude.Text)
listResourcesForTagOption_resourceType = Lens.lens (\ListResourcesForTagOption' {resourceType} -> resourceType) (\s@ListResourcesForTagOption' {} a -> s {resourceType = a} :: ListResourcesForTagOption)

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
        Prelude.Just
          Prelude.$ rq
          Prelude.& listResourcesForTagOption_pageToken
          Lens..~ rs
          Lens.^? listResourcesForTagOptionResponse_pageToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListResourcesForTagOption where
  type
    AWSResponse ListResourcesForTagOption =
      ListResourcesForTagOptionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListResourcesForTagOptionResponse'
            Prelude.<$> (x Data..?> "PageToken")
            Prelude.<*> ( x
                            Data..?> "ResourceDetails"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListResourcesForTagOption where
  hashWithSalt _salt ListResourcesForTagOption' {..} =
    _salt
      `Prelude.hashWithSalt` pageSize
      `Prelude.hashWithSalt` pageToken
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` tagOptionId

instance Prelude.NFData ListResourcesForTagOption where
  rnf ListResourcesForTagOption' {..} =
    Prelude.rnf pageSize
      `Prelude.seq` Prelude.rnf pageToken
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf tagOptionId

instance Data.ToHeaders ListResourcesForTagOption where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWS242ServiceCatalogService.ListResourcesForTagOption" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListResourcesForTagOption where
  toJSON ListResourcesForTagOption' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PageSize" Data..=) Prelude.<$> pageSize,
            ("PageToken" Data..=) Prelude.<$> pageToken,
            ("ResourceType" Data..=) Prelude.<$> resourceType,
            Prelude.Just ("TagOptionId" Data..= tagOptionId)
          ]
      )

instance Data.ToPath ListResourcesForTagOption where
  toPath = Prelude.const "/"

instance Data.ToQuery ListResourcesForTagOption where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListResourcesForTagOptionResponse' smart constructor.
data ListResourcesForTagOptionResponse = ListResourcesForTagOptionResponse'
  { -- | The page token for the next set of results. To retrieve the first set of
    -- results, use null.
    pageToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the resources.
    resourceDetails :: Prelude.Maybe [ResourceDetail],
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
-- 'pageToken', 'listResourcesForTagOptionResponse_pageToken' - The page token for the next set of results. To retrieve the first set of
-- results, use null.
--
-- 'resourceDetails', 'listResourcesForTagOptionResponse_resourceDetails' - Information about the resources.
--
-- 'httpStatus', 'listResourcesForTagOptionResponse_httpStatus' - The response's http status code.
newListResourcesForTagOptionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListResourcesForTagOptionResponse
newListResourcesForTagOptionResponse pHttpStatus_ =
  ListResourcesForTagOptionResponse'
    { pageToken =
        Prelude.Nothing,
      resourceDetails = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The page token for the next set of results. To retrieve the first set of
-- results, use null.
listResourcesForTagOptionResponse_pageToken :: Lens.Lens' ListResourcesForTagOptionResponse (Prelude.Maybe Prelude.Text)
listResourcesForTagOptionResponse_pageToken = Lens.lens (\ListResourcesForTagOptionResponse' {pageToken} -> pageToken) (\s@ListResourcesForTagOptionResponse' {} a -> s {pageToken = a} :: ListResourcesForTagOptionResponse)

-- | Information about the resources.
listResourcesForTagOptionResponse_resourceDetails :: Lens.Lens' ListResourcesForTagOptionResponse (Prelude.Maybe [ResourceDetail])
listResourcesForTagOptionResponse_resourceDetails = Lens.lens (\ListResourcesForTagOptionResponse' {resourceDetails} -> resourceDetails) (\s@ListResourcesForTagOptionResponse' {} a -> s {resourceDetails = a} :: ListResourcesForTagOptionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listResourcesForTagOptionResponse_httpStatus :: Lens.Lens' ListResourcesForTagOptionResponse Prelude.Int
listResourcesForTagOptionResponse_httpStatus = Lens.lens (\ListResourcesForTagOptionResponse' {httpStatus} -> httpStatus) (\s@ListResourcesForTagOptionResponse' {} a -> s {httpStatus = a} :: ListResourcesForTagOptionResponse)

instance
  Prelude.NFData
    ListResourcesForTagOptionResponse
  where
  rnf ListResourcesForTagOptionResponse' {..} =
    Prelude.rnf pageToken
      `Prelude.seq` Prelude.rnf resourceDetails
      `Prelude.seq` Prelude.rnf httpStatus
