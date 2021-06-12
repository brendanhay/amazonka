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
-- Module      : Network.AWS.ResourceGroupsTagging.GetTagValues
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all tag values for the specified key that are used in the
-- specified AWS Region for the calling AWS account.
--
-- This operation supports pagination, where the response can be sent in
-- multiple pages. You should check the @PaginationToken@ response
-- parameter to determine if there are additional results available to
-- return. Repeat the query, passing the @PaginationToken@ response
-- parameter value as an input to the next request until you recieve a
-- @null@ value. A null value for @PaginationToken@ indicates that there
-- are no more results waiting to be returned.
--
-- This operation returns paginated results.
module Network.AWS.ResourceGroupsTagging.GetTagValues
  ( -- * Creating a Request
    GetTagValues (..),
    newGetTagValues,

    -- * Request Lenses
    getTagValues_paginationToken,
    getTagValues_key,

    -- * Destructuring the Response
    GetTagValuesResponse (..),
    newGetTagValuesResponse,

    -- * Response Lenses
    getTagValuesResponse_paginationToken,
    getTagValuesResponse_tagValues,
    getTagValuesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import Network.AWS.ResourceGroupsTagging.Types
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetTagValues' smart constructor.
data GetTagValues = GetTagValues'
  { -- | Specifies a @PaginationToken@ response value from a previous request to
    -- indicate that you want the next page of results. Leave this parameter
    -- empty in your initial request.
    paginationToken :: Core.Maybe Core.Text,
    -- | Specifies the tag key for which you want to list all existing values
    -- that are currently used in the specified AWS Region for the calling AWS
    -- account.
    key :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetTagValues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'paginationToken', 'getTagValues_paginationToken' - Specifies a @PaginationToken@ response value from a previous request to
-- indicate that you want the next page of results. Leave this parameter
-- empty in your initial request.
--
-- 'key', 'getTagValues_key' - Specifies the tag key for which you want to list all existing values
-- that are currently used in the specified AWS Region for the calling AWS
-- account.
newGetTagValues ::
  -- | 'key'
  Core.Text ->
  GetTagValues
newGetTagValues pKey_ =
  GetTagValues'
    { paginationToken = Core.Nothing,
      key = pKey_
    }

-- | Specifies a @PaginationToken@ response value from a previous request to
-- indicate that you want the next page of results. Leave this parameter
-- empty in your initial request.
getTagValues_paginationToken :: Lens.Lens' GetTagValues (Core.Maybe Core.Text)
getTagValues_paginationToken = Lens.lens (\GetTagValues' {paginationToken} -> paginationToken) (\s@GetTagValues' {} a -> s {paginationToken = a} :: GetTagValues)

-- | Specifies the tag key for which you want to list all existing values
-- that are currently used in the specified AWS Region for the calling AWS
-- account.
getTagValues_key :: Lens.Lens' GetTagValues Core.Text
getTagValues_key = Lens.lens (\GetTagValues' {key} -> key) (\s@GetTagValues' {} a -> s {key = a} :: GetTagValues)

instance Core.AWSPager GetTagValues where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getTagValuesResponse_paginationToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getTagValuesResponse_tagValues Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getTagValues_paginationToken
          Lens..~ rs
          Lens.^? getTagValuesResponse_paginationToken
            Core.. Lens._Just

instance Core.AWSRequest GetTagValues where
  type AWSResponse GetTagValues = GetTagValuesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTagValuesResponse'
            Core.<$> (x Core..?> "PaginationToken")
            Core.<*> (x Core..?> "TagValues" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetTagValues

instance Core.NFData GetTagValues

instance Core.ToHeaders GetTagValues where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ResourceGroupsTaggingAPI_20170126.GetTagValues" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetTagValues where
  toJSON GetTagValues' {..} =
    Core.object
      ( Core.catMaybes
          [ ("PaginationToken" Core..=)
              Core.<$> paginationToken,
            Core.Just ("Key" Core..= key)
          ]
      )

instance Core.ToPath GetTagValues where
  toPath = Core.const "/"

instance Core.ToQuery GetTagValues where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetTagValuesResponse' smart constructor.
data GetTagValuesResponse = GetTagValuesResponse'
  { -- | A string that indicates that there is more data available than this
    -- response contains. To receive the next part of the response, specify
    -- this response value as the @PaginationToken@ value in the request for
    -- the next page.
    paginationToken :: Core.Maybe Core.Text,
    -- | A list of all tag values for the specified key currently used in the
    -- specified AWS Region for the calling AWS account.
    tagValues :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetTagValuesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'paginationToken', 'getTagValuesResponse_paginationToken' - A string that indicates that there is more data available than this
-- response contains. To receive the next part of the response, specify
-- this response value as the @PaginationToken@ value in the request for
-- the next page.
--
-- 'tagValues', 'getTagValuesResponse_tagValues' - A list of all tag values for the specified key currently used in the
-- specified AWS Region for the calling AWS account.
--
-- 'httpStatus', 'getTagValuesResponse_httpStatus' - The response's http status code.
newGetTagValuesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetTagValuesResponse
newGetTagValuesResponse pHttpStatus_ =
  GetTagValuesResponse'
    { paginationToken =
        Core.Nothing,
      tagValues = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A string that indicates that there is more data available than this
-- response contains. To receive the next part of the response, specify
-- this response value as the @PaginationToken@ value in the request for
-- the next page.
getTagValuesResponse_paginationToken :: Lens.Lens' GetTagValuesResponse (Core.Maybe Core.Text)
getTagValuesResponse_paginationToken = Lens.lens (\GetTagValuesResponse' {paginationToken} -> paginationToken) (\s@GetTagValuesResponse' {} a -> s {paginationToken = a} :: GetTagValuesResponse)

-- | A list of all tag values for the specified key currently used in the
-- specified AWS Region for the calling AWS account.
getTagValuesResponse_tagValues :: Lens.Lens' GetTagValuesResponse (Core.Maybe [Core.Text])
getTagValuesResponse_tagValues = Lens.lens (\GetTagValuesResponse' {tagValues} -> tagValues) (\s@GetTagValuesResponse' {} a -> s {tagValues = a} :: GetTagValuesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getTagValuesResponse_httpStatus :: Lens.Lens' GetTagValuesResponse Core.Int
getTagValuesResponse_httpStatus = Lens.lens (\GetTagValuesResponse' {httpStatus} -> httpStatus) (\s@GetTagValuesResponse' {} a -> s {httpStatus = a} :: GetTagValuesResponse)

instance Core.NFData GetTagValuesResponse
