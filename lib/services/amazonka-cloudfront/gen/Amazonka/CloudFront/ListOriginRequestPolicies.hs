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
-- Module      : Amazonka.CloudFront.ListOriginRequestPolicies
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of origin request policies.
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
module Amazonka.CloudFront.ListOriginRequestPolicies
  ( -- * Creating a Request
    ListOriginRequestPolicies (..),
    newListOriginRequestPolicies,

    -- * Request Lenses
    listOriginRequestPolicies_marker,
    listOriginRequestPolicies_maxItems,
    listOriginRequestPolicies_type,

    -- * Destructuring the Response
    ListOriginRequestPoliciesResponse (..),
    newListOriginRequestPoliciesResponse,

    -- * Response Lenses
    listOriginRequestPoliciesResponse_originRequestPolicyList,
    listOriginRequestPoliciesResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListOriginRequestPolicies' smart constructor.
data ListOriginRequestPolicies = ListOriginRequestPolicies'
  { -- | Use this field when paginating results to indicate where to begin in
    -- your list of origin request policies. The response includes origin
    -- request policies in the list that occur after the marker. To get the
    -- next page of the list, set this field\'s value to the value of
    -- @NextMarker@ from the current page\'s response.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of origin request policies that you want in the
    -- response.
    maxItems :: Prelude.Maybe Prelude.Text,
    -- | A filter to return only the specified kinds of origin request policies.
    -- Valid values are:
    --
    -- -   @managed@ – Returns only the managed policies created by Amazon Web
    --     Services.
    --
    -- -   @custom@ – Returns only the custom policies created in your Amazon
    --     Web Services account.
    type' :: Prelude.Maybe OriginRequestPolicyType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListOriginRequestPolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listOriginRequestPolicies_marker' - Use this field when paginating results to indicate where to begin in
-- your list of origin request policies. The response includes origin
-- request policies in the list that occur after the marker. To get the
-- next page of the list, set this field\'s value to the value of
-- @NextMarker@ from the current page\'s response.
--
-- 'maxItems', 'listOriginRequestPolicies_maxItems' - The maximum number of origin request policies that you want in the
-- response.
--
-- 'type'', 'listOriginRequestPolicies_type' - A filter to return only the specified kinds of origin request policies.
-- Valid values are:
--
-- -   @managed@ – Returns only the managed policies created by Amazon Web
--     Services.
--
-- -   @custom@ – Returns only the custom policies created in your Amazon
--     Web Services account.
newListOriginRequestPolicies ::
  ListOriginRequestPolicies
newListOriginRequestPolicies =
  ListOriginRequestPolicies'
    { marker =
        Prelude.Nothing,
      maxItems = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | Use this field when paginating results to indicate where to begin in
-- your list of origin request policies. The response includes origin
-- request policies in the list that occur after the marker. To get the
-- next page of the list, set this field\'s value to the value of
-- @NextMarker@ from the current page\'s response.
listOriginRequestPolicies_marker :: Lens.Lens' ListOriginRequestPolicies (Prelude.Maybe Prelude.Text)
listOriginRequestPolicies_marker = Lens.lens (\ListOriginRequestPolicies' {marker} -> marker) (\s@ListOriginRequestPolicies' {} a -> s {marker = a} :: ListOriginRequestPolicies)

-- | The maximum number of origin request policies that you want in the
-- response.
listOriginRequestPolicies_maxItems :: Lens.Lens' ListOriginRequestPolicies (Prelude.Maybe Prelude.Text)
listOriginRequestPolicies_maxItems = Lens.lens (\ListOriginRequestPolicies' {maxItems} -> maxItems) (\s@ListOriginRequestPolicies' {} a -> s {maxItems = a} :: ListOriginRequestPolicies)

-- | A filter to return only the specified kinds of origin request policies.
-- Valid values are:
--
-- -   @managed@ – Returns only the managed policies created by Amazon Web
--     Services.
--
-- -   @custom@ – Returns only the custom policies created in your Amazon
--     Web Services account.
listOriginRequestPolicies_type :: Lens.Lens' ListOriginRequestPolicies (Prelude.Maybe OriginRequestPolicyType)
listOriginRequestPolicies_type = Lens.lens (\ListOriginRequestPolicies' {type'} -> type') (\s@ListOriginRequestPolicies' {} a -> s {type' = a} :: ListOriginRequestPolicies)

instance Core.AWSRequest ListOriginRequestPolicies where
  type
    AWSResponse ListOriginRequestPolicies =
      ListOriginRequestPoliciesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ListOriginRequestPoliciesResponse'
            Prelude.<$> (Data.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListOriginRequestPolicies where
  hashWithSalt _salt ListOriginRequestPolicies' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxItems
      `Prelude.hashWithSalt` type'

instance Prelude.NFData ListOriginRequestPolicies where
  rnf ListOriginRequestPolicies' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxItems
      `Prelude.seq` Prelude.rnf type'

instance Data.ToHeaders ListOriginRequestPolicies where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListOriginRequestPolicies where
  toPath =
    Prelude.const "/2020-05-31/origin-request-policy"

instance Data.ToQuery ListOriginRequestPolicies where
  toQuery ListOriginRequestPolicies' {..} =
    Prelude.mconcat
      [ "Marker" Data.=: marker,
        "MaxItems" Data.=: maxItems,
        "Type" Data.=: type'
      ]

-- | /See:/ 'newListOriginRequestPoliciesResponse' smart constructor.
data ListOriginRequestPoliciesResponse = ListOriginRequestPoliciesResponse'
  { -- | A list of origin request policies.
    originRequestPolicyList :: Prelude.Maybe OriginRequestPolicyList,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListOriginRequestPoliciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'originRequestPolicyList', 'listOriginRequestPoliciesResponse_originRequestPolicyList' - A list of origin request policies.
--
-- 'httpStatus', 'listOriginRequestPoliciesResponse_httpStatus' - The response's http status code.
newListOriginRequestPoliciesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListOriginRequestPoliciesResponse
newListOriginRequestPoliciesResponse pHttpStatus_ =
  ListOriginRequestPoliciesResponse'
    { originRequestPolicyList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of origin request policies.
listOriginRequestPoliciesResponse_originRequestPolicyList :: Lens.Lens' ListOriginRequestPoliciesResponse (Prelude.Maybe OriginRequestPolicyList)
listOriginRequestPoliciesResponse_originRequestPolicyList = Lens.lens (\ListOriginRequestPoliciesResponse' {originRequestPolicyList} -> originRequestPolicyList) (\s@ListOriginRequestPoliciesResponse' {} a -> s {originRequestPolicyList = a} :: ListOriginRequestPoliciesResponse)

-- | The response's http status code.
listOriginRequestPoliciesResponse_httpStatus :: Lens.Lens' ListOriginRequestPoliciesResponse Prelude.Int
listOriginRequestPoliciesResponse_httpStatus = Lens.lens (\ListOriginRequestPoliciesResponse' {httpStatus} -> httpStatus) (\s@ListOriginRequestPoliciesResponse' {} a -> s {httpStatus = a} :: ListOriginRequestPoliciesResponse)

instance
  Prelude.NFData
    ListOriginRequestPoliciesResponse
  where
  rnf ListOriginRequestPoliciesResponse' {..} =
    Prelude.rnf originRequestPolicyList
      `Prelude.seq` Prelude.rnf httpStatus
