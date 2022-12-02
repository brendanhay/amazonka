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
-- Module      : Amazonka.CloudFront.ListCachePolicies
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of cache policies.
--
-- You can optionally apply a filter to return only the managed policies
-- created by Amazon Web Services, or only the custom policies created in
-- your Amazon Web Services account.
--
-- You can optionally specify the maximum number of items to receive in the
-- response. If the total number of items in the list exceeds the maximum
-- that you specify, or the default maximum, the response is paginated. To
-- get the next page of items, send a subsequent request that specifies the
-- @NextMarker@ value from the current response as the @Marker@ value in
-- the subsequent request.
module Amazonka.CloudFront.ListCachePolicies
  ( -- * Creating a Request
    ListCachePolicies (..),
    newListCachePolicies,

    -- * Request Lenses
    listCachePolicies_type,
    listCachePolicies_marker,
    listCachePolicies_maxItems,

    -- * Destructuring the Response
    ListCachePoliciesResponse (..),
    newListCachePoliciesResponse,

    -- * Response Lenses
    listCachePoliciesResponse_cachePolicyList,
    listCachePoliciesResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListCachePolicies' smart constructor.
data ListCachePolicies = ListCachePolicies'
  { -- | A filter to return only the specified kinds of cache policies. Valid
    -- values are:
    --
    -- -   @managed@ – Returns only the managed policies created by Amazon Web
    --     Services.
    --
    -- -   @custom@ – Returns only the custom policies created in your Amazon
    --     Web Services account.
    type' :: Prelude.Maybe CachePolicyType,
    -- | Use this field when paginating results to indicate where to begin in
    -- your list of cache policies. The response includes cache policies in the
    -- list that occur after the marker. To get the next page of the list, set
    -- this field’s value to the value of @NextMarker@ from the current page’s
    -- response.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of cache policies that you want in the response.
    maxItems :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCachePolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'listCachePolicies_type' - A filter to return only the specified kinds of cache policies. Valid
-- values are:
--
-- -   @managed@ – Returns only the managed policies created by Amazon Web
--     Services.
--
-- -   @custom@ – Returns only the custom policies created in your Amazon
--     Web Services account.
--
-- 'marker', 'listCachePolicies_marker' - Use this field when paginating results to indicate where to begin in
-- your list of cache policies. The response includes cache policies in the
-- list that occur after the marker. To get the next page of the list, set
-- this field’s value to the value of @NextMarker@ from the current page’s
-- response.
--
-- 'maxItems', 'listCachePolicies_maxItems' - The maximum number of cache policies that you want in the response.
newListCachePolicies ::
  ListCachePolicies
newListCachePolicies =
  ListCachePolicies'
    { type' = Prelude.Nothing,
      marker = Prelude.Nothing,
      maxItems = Prelude.Nothing
    }

-- | A filter to return only the specified kinds of cache policies. Valid
-- values are:
--
-- -   @managed@ – Returns only the managed policies created by Amazon Web
--     Services.
--
-- -   @custom@ – Returns only the custom policies created in your Amazon
--     Web Services account.
listCachePolicies_type :: Lens.Lens' ListCachePolicies (Prelude.Maybe CachePolicyType)
listCachePolicies_type = Lens.lens (\ListCachePolicies' {type'} -> type') (\s@ListCachePolicies' {} a -> s {type' = a} :: ListCachePolicies)

-- | Use this field when paginating results to indicate where to begin in
-- your list of cache policies. The response includes cache policies in the
-- list that occur after the marker. To get the next page of the list, set
-- this field’s value to the value of @NextMarker@ from the current page’s
-- response.
listCachePolicies_marker :: Lens.Lens' ListCachePolicies (Prelude.Maybe Prelude.Text)
listCachePolicies_marker = Lens.lens (\ListCachePolicies' {marker} -> marker) (\s@ListCachePolicies' {} a -> s {marker = a} :: ListCachePolicies)

-- | The maximum number of cache policies that you want in the response.
listCachePolicies_maxItems :: Lens.Lens' ListCachePolicies (Prelude.Maybe Prelude.Text)
listCachePolicies_maxItems = Lens.lens (\ListCachePolicies' {maxItems} -> maxItems) (\s@ListCachePolicies' {} a -> s {maxItems = a} :: ListCachePolicies)

instance Core.AWSRequest ListCachePolicies where
  type
    AWSResponse ListCachePolicies =
      ListCachePoliciesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ListCachePoliciesResponse'
            Prelude.<$> (Data.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCachePolicies where
  hashWithSalt _salt ListCachePolicies' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxItems

instance Prelude.NFData ListCachePolicies where
  rnf ListCachePolicies' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxItems

instance Data.ToHeaders ListCachePolicies where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListCachePolicies where
  toPath = Prelude.const "/2020-05-31/cache-policy"

instance Data.ToQuery ListCachePolicies where
  toQuery ListCachePolicies' {..} =
    Prelude.mconcat
      [ "Type" Data.=: type',
        "Marker" Data.=: marker,
        "MaxItems" Data.=: maxItems
      ]

-- | /See:/ 'newListCachePoliciesResponse' smart constructor.
data ListCachePoliciesResponse = ListCachePoliciesResponse'
  { -- | A list of cache policies.
    cachePolicyList :: Prelude.Maybe CachePolicyList,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCachePoliciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cachePolicyList', 'listCachePoliciesResponse_cachePolicyList' - A list of cache policies.
--
-- 'httpStatus', 'listCachePoliciesResponse_httpStatus' - The response's http status code.
newListCachePoliciesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCachePoliciesResponse
newListCachePoliciesResponse pHttpStatus_ =
  ListCachePoliciesResponse'
    { cachePolicyList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of cache policies.
listCachePoliciesResponse_cachePolicyList :: Lens.Lens' ListCachePoliciesResponse (Prelude.Maybe CachePolicyList)
listCachePoliciesResponse_cachePolicyList = Lens.lens (\ListCachePoliciesResponse' {cachePolicyList} -> cachePolicyList) (\s@ListCachePoliciesResponse' {} a -> s {cachePolicyList = a} :: ListCachePoliciesResponse)

-- | The response's http status code.
listCachePoliciesResponse_httpStatus :: Lens.Lens' ListCachePoliciesResponse Prelude.Int
listCachePoliciesResponse_httpStatus = Lens.lens (\ListCachePoliciesResponse' {httpStatus} -> httpStatus) (\s@ListCachePoliciesResponse' {} a -> s {httpStatus = a} :: ListCachePoliciesResponse)

instance Prelude.NFData ListCachePoliciesResponse where
  rnf ListCachePoliciesResponse' {..} =
    Prelude.rnf cachePolicyList
      `Prelude.seq` Prelude.rnf httpStatus
