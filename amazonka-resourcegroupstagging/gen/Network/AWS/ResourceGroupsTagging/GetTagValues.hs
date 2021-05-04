{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import Network.AWS.ResourceGroupsTagging.Types
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetTagValues' smart constructor.
data GetTagValues = GetTagValues'
  { -- | Specifies a @PaginationToken@ response value from a previous request to
    -- indicate that you want the next page of results. Leave this parameter
    -- empty in your initial request.
    paginationToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies the tag key for which you want to list all existing values
    -- that are currently used in the specified AWS Region for the calling AWS
    -- account.
    key :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  GetTagValues
newGetTagValues pKey_ =
  GetTagValues'
    { paginationToken = Prelude.Nothing,
      key = pKey_
    }

-- | Specifies a @PaginationToken@ response value from a previous request to
-- indicate that you want the next page of results. Leave this parameter
-- empty in your initial request.
getTagValues_paginationToken :: Lens.Lens' GetTagValues (Prelude.Maybe Prelude.Text)
getTagValues_paginationToken = Lens.lens (\GetTagValues' {paginationToken} -> paginationToken) (\s@GetTagValues' {} a -> s {paginationToken = a} :: GetTagValues)

-- | Specifies the tag key for which you want to list all existing values
-- that are currently used in the specified AWS Region for the calling AWS
-- account.
getTagValues_key :: Lens.Lens' GetTagValues Prelude.Text
getTagValues_key = Lens.lens (\GetTagValues' {key} -> key) (\s@GetTagValues' {} a -> s {key = a} :: GetTagValues)

instance Pager.AWSPager GetTagValues where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? getTagValuesResponse_paginationToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? getTagValuesResponse_tagValues Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& getTagValues_paginationToken
          Lens..~ rs
          Lens.^? getTagValuesResponse_paginationToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest GetTagValues where
  type Rs GetTagValues = GetTagValuesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTagValuesResponse'
            Prelude.<$> (x Prelude..?> "PaginationToken")
            Prelude.<*> ( x Prelude..?> "TagValues"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetTagValues

instance Prelude.NFData GetTagValues

instance Prelude.ToHeaders GetTagValues where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "ResourceGroupsTaggingAPI_20170126.GetTagValues" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetTagValues where
  toJSON GetTagValues' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("PaginationToken" Prelude..=)
              Prelude.<$> paginationToken,
            Prelude.Just ("Key" Prelude..= key)
          ]
      )

instance Prelude.ToPath GetTagValues where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetTagValues where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetTagValuesResponse' smart constructor.
data GetTagValuesResponse = GetTagValuesResponse'
  { -- | A string that indicates that there is more data available than this
    -- response contains. To receive the next part of the response, specify
    -- this response value as the @PaginationToken@ value in the request for
    -- the next page.
    paginationToken :: Prelude.Maybe Prelude.Text,
    -- | A list of all tag values for the specified key currently used in the
    -- specified AWS Region for the calling AWS account.
    tagValues :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  GetTagValuesResponse
newGetTagValuesResponse pHttpStatus_ =
  GetTagValuesResponse'
    { paginationToken =
        Prelude.Nothing,
      tagValues = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A string that indicates that there is more data available than this
-- response contains. To receive the next part of the response, specify
-- this response value as the @PaginationToken@ value in the request for
-- the next page.
getTagValuesResponse_paginationToken :: Lens.Lens' GetTagValuesResponse (Prelude.Maybe Prelude.Text)
getTagValuesResponse_paginationToken = Lens.lens (\GetTagValuesResponse' {paginationToken} -> paginationToken) (\s@GetTagValuesResponse' {} a -> s {paginationToken = a} :: GetTagValuesResponse)

-- | A list of all tag values for the specified key currently used in the
-- specified AWS Region for the calling AWS account.
getTagValuesResponse_tagValues :: Lens.Lens' GetTagValuesResponse (Prelude.Maybe [Prelude.Text])
getTagValuesResponse_tagValues = Lens.lens (\GetTagValuesResponse' {tagValues} -> tagValues) (\s@GetTagValuesResponse' {} a -> s {tagValues = a} :: GetTagValuesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
getTagValuesResponse_httpStatus :: Lens.Lens' GetTagValuesResponse Prelude.Int
getTagValuesResponse_httpStatus = Lens.lens (\GetTagValuesResponse' {httpStatus} -> httpStatus) (\s@GetTagValuesResponse' {} a -> s {httpStatus = a} :: GetTagValuesResponse)

instance Prelude.NFData GetTagValuesResponse
