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
-- Module      : Amazonka.CloudFront.ListContinuousDeploymentPolicies
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the continuous deployment policies in your Amazon Web
-- Services account.
--
-- You can optionally specify the maximum number of items to receive in the
-- response. If the total number of items in the list exceeds the maximum
-- that you specify, or the default maximum, the response is paginated. To
-- get the next page of items, send a subsequent request that specifies the
-- @NextMarker@ value from the current response as the @Marker@ value in
-- the subsequent request.
module Amazonka.CloudFront.ListContinuousDeploymentPolicies
  ( -- * Creating a Request
    ListContinuousDeploymentPolicies (..),
    newListContinuousDeploymentPolicies,

    -- * Request Lenses
    listContinuousDeploymentPolicies_marker,
    listContinuousDeploymentPolicies_maxItems,

    -- * Destructuring the Response
    ListContinuousDeploymentPoliciesResponse (..),
    newListContinuousDeploymentPoliciesResponse,

    -- * Response Lenses
    listContinuousDeploymentPoliciesResponse_continuousDeploymentPolicyList,
    listContinuousDeploymentPoliciesResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListContinuousDeploymentPolicies' smart constructor.
data ListContinuousDeploymentPolicies = ListContinuousDeploymentPolicies'
  { -- | Use this field when paginating results to indicate where to begin in
    -- your list of continuous deployment policies. The response includes
    -- policies in the list that occur after the marker. To get the next page
    -- of the list, set this field\'s value to the value of @NextMarker@ from
    -- the current page\'s response.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of continuous deployment policies that you want
    -- returned in the response.
    maxItems :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListContinuousDeploymentPolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listContinuousDeploymentPolicies_marker' - Use this field when paginating results to indicate where to begin in
-- your list of continuous deployment policies. The response includes
-- policies in the list that occur after the marker. To get the next page
-- of the list, set this field\'s value to the value of @NextMarker@ from
-- the current page\'s response.
--
-- 'maxItems', 'listContinuousDeploymentPolicies_maxItems' - The maximum number of continuous deployment policies that you want
-- returned in the response.
newListContinuousDeploymentPolicies ::
  ListContinuousDeploymentPolicies
newListContinuousDeploymentPolicies =
  ListContinuousDeploymentPolicies'
    { marker =
        Prelude.Nothing,
      maxItems = Prelude.Nothing
    }

-- | Use this field when paginating results to indicate where to begin in
-- your list of continuous deployment policies. The response includes
-- policies in the list that occur after the marker. To get the next page
-- of the list, set this field\'s value to the value of @NextMarker@ from
-- the current page\'s response.
listContinuousDeploymentPolicies_marker :: Lens.Lens' ListContinuousDeploymentPolicies (Prelude.Maybe Prelude.Text)
listContinuousDeploymentPolicies_marker = Lens.lens (\ListContinuousDeploymentPolicies' {marker} -> marker) (\s@ListContinuousDeploymentPolicies' {} a -> s {marker = a} :: ListContinuousDeploymentPolicies)

-- | The maximum number of continuous deployment policies that you want
-- returned in the response.
listContinuousDeploymentPolicies_maxItems :: Lens.Lens' ListContinuousDeploymentPolicies (Prelude.Maybe Prelude.Text)
listContinuousDeploymentPolicies_maxItems = Lens.lens (\ListContinuousDeploymentPolicies' {maxItems} -> maxItems) (\s@ListContinuousDeploymentPolicies' {} a -> s {maxItems = a} :: ListContinuousDeploymentPolicies)

instance
  Core.AWSRequest
    ListContinuousDeploymentPolicies
  where
  type
    AWSResponse ListContinuousDeploymentPolicies =
      ListContinuousDeploymentPoliciesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ListContinuousDeploymentPoliciesResponse'
            Prelude.<$> (Data.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListContinuousDeploymentPolicies
  where
  hashWithSalt
    _salt
    ListContinuousDeploymentPolicies' {..} =
      _salt `Prelude.hashWithSalt` marker
        `Prelude.hashWithSalt` maxItems

instance
  Prelude.NFData
    ListContinuousDeploymentPolicies
  where
  rnf ListContinuousDeploymentPolicies' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxItems

instance
  Data.ToHeaders
    ListContinuousDeploymentPolicies
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListContinuousDeploymentPolicies where
  toPath =
    Prelude.const
      "/2020-05-31/continuous-deployment-policy"

instance
  Data.ToQuery
    ListContinuousDeploymentPolicies
  where
  toQuery ListContinuousDeploymentPolicies' {..} =
    Prelude.mconcat
      [ "Marker" Data.=: marker,
        "MaxItems" Data.=: maxItems
      ]

-- | /See:/ 'newListContinuousDeploymentPoliciesResponse' smart constructor.
data ListContinuousDeploymentPoliciesResponse = ListContinuousDeploymentPoliciesResponse'
  { -- | A list of continuous deployment policies.
    continuousDeploymentPolicyList :: Prelude.Maybe ContinuousDeploymentPolicyList,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListContinuousDeploymentPoliciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'continuousDeploymentPolicyList', 'listContinuousDeploymentPoliciesResponse_continuousDeploymentPolicyList' - A list of continuous deployment policies.
--
-- 'httpStatus', 'listContinuousDeploymentPoliciesResponse_httpStatus' - The response's http status code.
newListContinuousDeploymentPoliciesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListContinuousDeploymentPoliciesResponse
newListContinuousDeploymentPoliciesResponse
  pHttpStatus_ =
    ListContinuousDeploymentPoliciesResponse'
      { continuousDeploymentPolicyList =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A list of continuous deployment policies.
listContinuousDeploymentPoliciesResponse_continuousDeploymentPolicyList :: Lens.Lens' ListContinuousDeploymentPoliciesResponse (Prelude.Maybe ContinuousDeploymentPolicyList)
listContinuousDeploymentPoliciesResponse_continuousDeploymentPolicyList = Lens.lens (\ListContinuousDeploymentPoliciesResponse' {continuousDeploymentPolicyList} -> continuousDeploymentPolicyList) (\s@ListContinuousDeploymentPoliciesResponse' {} a -> s {continuousDeploymentPolicyList = a} :: ListContinuousDeploymentPoliciesResponse)

-- | The response's http status code.
listContinuousDeploymentPoliciesResponse_httpStatus :: Lens.Lens' ListContinuousDeploymentPoliciesResponse Prelude.Int
listContinuousDeploymentPoliciesResponse_httpStatus = Lens.lens (\ListContinuousDeploymentPoliciesResponse' {httpStatus} -> httpStatus) (\s@ListContinuousDeploymentPoliciesResponse' {} a -> s {httpStatus = a} :: ListContinuousDeploymentPoliciesResponse)

instance
  Prelude.NFData
    ListContinuousDeploymentPoliciesResponse
  where
  rnf ListContinuousDeploymentPoliciesResponse' {..} =
    Prelude.rnf continuousDeploymentPolicyList
      `Prelude.seq` Prelude.rnf httpStatus
