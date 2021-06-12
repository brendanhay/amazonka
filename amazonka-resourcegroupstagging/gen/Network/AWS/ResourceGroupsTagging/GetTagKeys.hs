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
-- Module      : Network.AWS.ResourceGroupsTagging.GetTagKeys
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all tag keys currently in use in the specified Region for the
-- calling AWS account.
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
module Network.AWS.ResourceGroupsTagging.GetTagKeys
  ( -- * Creating a Request
    GetTagKeys (..),
    newGetTagKeys,

    -- * Request Lenses
    getTagKeys_paginationToken,

    -- * Destructuring the Response
    GetTagKeysResponse (..),
    newGetTagKeysResponse,

    -- * Response Lenses
    getTagKeysResponse_tagKeys,
    getTagKeysResponse_paginationToken,
    getTagKeysResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import Network.AWS.ResourceGroupsTagging.Types
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetTagKeys' smart constructor.
data GetTagKeys = GetTagKeys'
  { -- | Specifies a @PaginationToken@ response value from a previous request to
    -- indicate that you want the next page of results. Leave this parameter
    -- empty in your initial request.
    paginationToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetTagKeys' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'paginationToken', 'getTagKeys_paginationToken' - Specifies a @PaginationToken@ response value from a previous request to
-- indicate that you want the next page of results. Leave this parameter
-- empty in your initial request.
newGetTagKeys ::
  GetTagKeys
newGetTagKeys =
  GetTagKeys' {paginationToken = Core.Nothing}

-- | Specifies a @PaginationToken@ response value from a previous request to
-- indicate that you want the next page of results. Leave this parameter
-- empty in your initial request.
getTagKeys_paginationToken :: Lens.Lens' GetTagKeys (Core.Maybe Core.Text)
getTagKeys_paginationToken = Lens.lens (\GetTagKeys' {paginationToken} -> paginationToken) (\s@GetTagKeys' {} a -> s {paginationToken = a} :: GetTagKeys)

instance Core.AWSPager GetTagKeys where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getTagKeysResponse_paginationToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getTagKeysResponse_tagKeys Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getTagKeys_paginationToken
          Lens..~ rs
          Lens.^? getTagKeysResponse_paginationToken Core.. Lens._Just

instance Core.AWSRequest GetTagKeys where
  type AWSResponse GetTagKeys = GetTagKeysResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTagKeysResponse'
            Core.<$> (x Core..?> "TagKeys" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "PaginationToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetTagKeys

instance Core.NFData GetTagKeys

instance Core.ToHeaders GetTagKeys where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ResourceGroupsTaggingAPI_20170126.GetTagKeys" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetTagKeys where
  toJSON GetTagKeys' {..} =
    Core.object
      ( Core.catMaybes
          [ ("PaginationToken" Core..=)
              Core.<$> paginationToken
          ]
      )

instance Core.ToPath GetTagKeys where
  toPath = Core.const "/"

instance Core.ToQuery GetTagKeys where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetTagKeysResponse' smart constructor.
data GetTagKeysResponse = GetTagKeysResponse'
  { -- | A list of all tag keys in the AWS account.
    tagKeys :: Core.Maybe [Core.Text],
    -- | A string that indicates that there is more data available than this
    -- response contains. To receive the next part of the response, specify
    -- this response value as the @PaginationToken@ value in the request for
    -- the next page.
    paginationToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetTagKeysResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagKeys', 'getTagKeysResponse_tagKeys' - A list of all tag keys in the AWS account.
--
-- 'paginationToken', 'getTagKeysResponse_paginationToken' - A string that indicates that there is more data available than this
-- response contains. To receive the next part of the response, specify
-- this response value as the @PaginationToken@ value in the request for
-- the next page.
--
-- 'httpStatus', 'getTagKeysResponse_httpStatus' - The response's http status code.
newGetTagKeysResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetTagKeysResponse
newGetTagKeysResponse pHttpStatus_ =
  GetTagKeysResponse'
    { tagKeys = Core.Nothing,
      paginationToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of all tag keys in the AWS account.
getTagKeysResponse_tagKeys :: Lens.Lens' GetTagKeysResponse (Core.Maybe [Core.Text])
getTagKeysResponse_tagKeys = Lens.lens (\GetTagKeysResponse' {tagKeys} -> tagKeys) (\s@GetTagKeysResponse' {} a -> s {tagKeys = a} :: GetTagKeysResponse) Core.. Lens.mapping Lens._Coerce

-- | A string that indicates that there is more data available than this
-- response contains. To receive the next part of the response, specify
-- this response value as the @PaginationToken@ value in the request for
-- the next page.
getTagKeysResponse_paginationToken :: Lens.Lens' GetTagKeysResponse (Core.Maybe Core.Text)
getTagKeysResponse_paginationToken = Lens.lens (\GetTagKeysResponse' {paginationToken} -> paginationToken) (\s@GetTagKeysResponse' {} a -> s {paginationToken = a} :: GetTagKeysResponse)

-- | The response's http status code.
getTagKeysResponse_httpStatus :: Lens.Lens' GetTagKeysResponse Core.Int
getTagKeysResponse_httpStatus = Lens.lens (\GetTagKeysResponse' {httpStatus} -> httpStatus) (\s@GetTagKeysResponse' {} a -> s {httpStatus = a} :: GetTagKeysResponse)

instance Core.NFData GetTagKeysResponse
