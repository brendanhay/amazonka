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
-- Module      : Amazonka.CloudFront.ListResponseHeadersPolicies
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of response headers policies.
--
-- You can optionally apply a filter to get only the managed policies
-- created by Amazon Web Services, or only the custom policies created in
-- your Amazon Web Services account.
--
-- You can optionally specify the maximum number of items to receive in the
-- response. If the total number of items in the list exceeds the maximum
-- that you specify, or the default maximum, the response is paginated. To
-- get the next page of items, send a subsequent request that specifies the
-- @NextMarker@ value from the current response as the @Marker@ value in
-- the subsequent request.
module Amazonka.CloudFront.ListResponseHeadersPolicies
  ( -- * Creating a Request
    ListResponseHeadersPolicies (..),
    newListResponseHeadersPolicies,

    -- * Request Lenses
    listResponseHeadersPolicies_type,
    listResponseHeadersPolicies_marker,
    listResponseHeadersPolicies_maxItems,

    -- * Destructuring the Response
    ListResponseHeadersPoliciesResponse (..),
    newListResponseHeadersPoliciesResponse,

    -- * Response Lenses
    listResponseHeadersPoliciesResponse_responseHeadersPolicyList,
    listResponseHeadersPoliciesResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListResponseHeadersPolicies' smart constructor.
data ListResponseHeadersPolicies = ListResponseHeadersPolicies'
  { -- | A filter to get only the specified kind of response headers policies.
    -- Valid values are:
    --
    -- -   @managed@ – Gets only the managed policies created by Amazon Web
    --     Services.
    --
    -- -   @custom@ – Gets only the custom policies created in your Amazon Web
    --     Services account.
    type' :: Prelude.Maybe ResponseHeadersPolicyType,
    -- | Use this field when paginating results to indicate where to begin in
    -- your list of response headers policies. The response includes response
    -- headers policies in the list that occur after the marker. To get the
    -- next page of the list, set this field’s value to the value of
    -- @NextMarker@ from the current page’s response.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of response headers policies that you want to get in
    -- the response.
    maxItems :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResponseHeadersPolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'listResponseHeadersPolicies_type' - A filter to get only the specified kind of response headers policies.
-- Valid values are:
--
-- -   @managed@ – Gets only the managed policies created by Amazon Web
--     Services.
--
-- -   @custom@ – Gets only the custom policies created in your Amazon Web
--     Services account.
--
-- 'marker', 'listResponseHeadersPolicies_marker' - Use this field when paginating results to indicate where to begin in
-- your list of response headers policies. The response includes response
-- headers policies in the list that occur after the marker. To get the
-- next page of the list, set this field’s value to the value of
-- @NextMarker@ from the current page’s response.
--
-- 'maxItems', 'listResponseHeadersPolicies_maxItems' - The maximum number of response headers policies that you want to get in
-- the response.
newListResponseHeadersPolicies ::
  ListResponseHeadersPolicies
newListResponseHeadersPolicies =
  ListResponseHeadersPolicies'
    { type' =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      maxItems = Prelude.Nothing
    }

-- | A filter to get only the specified kind of response headers policies.
-- Valid values are:
--
-- -   @managed@ – Gets only the managed policies created by Amazon Web
--     Services.
--
-- -   @custom@ – Gets only the custom policies created in your Amazon Web
--     Services account.
listResponseHeadersPolicies_type :: Lens.Lens' ListResponseHeadersPolicies (Prelude.Maybe ResponseHeadersPolicyType)
listResponseHeadersPolicies_type = Lens.lens (\ListResponseHeadersPolicies' {type'} -> type') (\s@ListResponseHeadersPolicies' {} a -> s {type' = a} :: ListResponseHeadersPolicies)

-- | Use this field when paginating results to indicate where to begin in
-- your list of response headers policies. The response includes response
-- headers policies in the list that occur after the marker. To get the
-- next page of the list, set this field’s value to the value of
-- @NextMarker@ from the current page’s response.
listResponseHeadersPolicies_marker :: Lens.Lens' ListResponseHeadersPolicies (Prelude.Maybe Prelude.Text)
listResponseHeadersPolicies_marker = Lens.lens (\ListResponseHeadersPolicies' {marker} -> marker) (\s@ListResponseHeadersPolicies' {} a -> s {marker = a} :: ListResponseHeadersPolicies)

-- | The maximum number of response headers policies that you want to get in
-- the response.
listResponseHeadersPolicies_maxItems :: Lens.Lens' ListResponseHeadersPolicies (Prelude.Maybe Prelude.Text)
listResponseHeadersPolicies_maxItems = Lens.lens (\ListResponseHeadersPolicies' {maxItems} -> maxItems) (\s@ListResponseHeadersPolicies' {} a -> s {maxItems = a} :: ListResponseHeadersPolicies)

instance Core.AWSRequest ListResponseHeadersPolicies where
  type
    AWSResponse ListResponseHeadersPolicies =
      ListResponseHeadersPoliciesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ListResponseHeadersPoliciesResponse'
            Prelude.<$> (Data.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListResponseHeadersPolicies where
  hashWithSalt _salt ListResponseHeadersPolicies' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxItems

instance Prelude.NFData ListResponseHeadersPolicies where
  rnf ListResponseHeadersPolicies' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxItems

instance Data.ToHeaders ListResponseHeadersPolicies where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListResponseHeadersPolicies where
  toPath =
    Prelude.const "/2020-05-31/response-headers-policy"

instance Data.ToQuery ListResponseHeadersPolicies where
  toQuery ListResponseHeadersPolicies' {..} =
    Prelude.mconcat
      [ "Type" Data.=: type',
        "Marker" Data.=: marker,
        "MaxItems" Data.=: maxItems
      ]

-- | /See:/ 'newListResponseHeadersPoliciesResponse' smart constructor.
data ListResponseHeadersPoliciesResponse = ListResponseHeadersPoliciesResponse'
  { -- | A list of response headers policies.
    responseHeadersPolicyList :: Prelude.Maybe ResponseHeadersPolicyList,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResponseHeadersPoliciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'responseHeadersPolicyList', 'listResponseHeadersPoliciesResponse_responseHeadersPolicyList' - A list of response headers policies.
--
-- 'httpStatus', 'listResponseHeadersPoliciesResponse_httpStatus' - The response's http status code.
newListResponseHeadersPoliciesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListResponseHeadersPoliciesResponse
newListResponseHeadersPoliciesResponse pHttpStatus_ =
  ListResponseHeadersPoliciesResponse'
    { responseHeadersPolicyList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of response headers policies.
listResponseHeadersPoliciesResponse_responseHeadersPolicyList :: Lens.Lens' ListResponseHeadersPoliciesResponse (Prelude.Maybe ResponseHeadersPolicyList)
listResponseHeadersPoliciesResponse_responseHeadersPolicyList = Lens.lens (\ListResponseHeadersPoliciesResponse' {responseHeadersPolicyList} -> responseHeadersPolicyList) (\s@ListResponseHeadersPoliciesResponse' {} a -> s {responseHeadersPolicyList = a} :: ListResponseHeadersPoliciesResponse)

-- | The response's http status code.
listResponseHeadersPoliciesResponse_httpStatus :: Lens.Lens' ListResponseHeadersPoliciesResponse Prelude.Int
listResponseHeadersPoliciesResponse_httpStatus = Lens.lens (\ListResponseHeadersPoliciesResponse' {httpStatus} -> httpStatus) (\s@ListResponseHeadersPoliciesResponse' {} a -> s {httpStatus = a} :: ListResponseHeadersPoliciesResponse)

instance
  Prelude.NFData
    ListResponseHeadersPoliciesResponse
  where
  rnf ListResponseHeadersPoliciesResponse' {..} =
    Prelude.rnf responseHeadersPolicyList
      `Prelude.seq` Prelude.rnf httpStatus
