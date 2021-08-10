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
-- Module      : Network.AWS.Route53.ListTrafficPolicyVersions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about all of the versions for a specified traffic
-- policy.
--
-- Traffic policy versions are listed in numerical order by
-- @VersionNumber@.
module Network.AWS.Route53.ListTrafficPolicyVersions
  ( -- * Creating a Request
    ListTrafficPolicyVersions (..),
    newListTrafficPolicyVersions,

    -- * Request Lenses
    listTrafficPolicyVersions_trafficPolicyVersionMarker,
    listTrafficPolicyVersions_maxItems,
    listTrafficPolicyVersions_id,

    -- * Destructuring the Response
    ListTrafficPolicyVersionsResponse (..),
    newListTrafficPolicyVersionsResponse,

    -- * Response Lenses
    listTrafficPolicyVersionsResponse_httpStatus,
    listTrafficPolicyVersionsResponse_trafficPolicies,
    listTrafficPolicyVersionsResponse_isTruncated,
    listTrafficPolicyVersionsResponse_trafficPolicyVersionMarker,
    listTrafficPolicyVersionsResponse_maxItems,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53.Types

-- | A complex type that contains the information about the request to list
-- your traffic policies.
--
-- /See:/ 'newListTrafficPolicyVersions' smart constructor.
data ListTrafficPolicyVersions = ListTrafficPolicyVersions'
  { -- | For your first request to @ListTrafficPolicyVersions@, don\'t include
    -- the @TrafficPolicyVersionMarker@ parameter.
    --
    -- If you have more traffic policy versions than the value of @MaxItems@,
    -- @ListTrafficPolicyVersions@ returns only the first group of @MaxItems@
    -- versions. To get more traffic policy versions, submit another
    -- @ListTrafficPolicyVersions@ request. For the value of
    -- @TrafficPolicyVersionMarker@, specify the value of
    -- @TrafficPolicyVersionMarker@ in the previous response.
    trafficPolicyVersionMarker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of traffic policy versions that you want Amazon Route
    -- 53 to include in the response body for this request. If the specified
    -- traffic policy has more than @MaxItems@ versions, the value of
    -- @IsTruncated@ in the response is @true@, and the value of the
    -- @TrafficPolicyVersionMarker@ element is the ID of the first version that
    -- Route 53 will return if you submit another request.
    maxItems :: Prelude.Maybe Prelude.Text,
    -- | Specify the value of @Id@ of the traffic policy for which you want to
    -- list all versions.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTrafficPolicyVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trafficPolicyVersionMarker', 'listTrafficPolicyVersions_trafficPolicyVersionMarker' - For your first request to @ListTrafficPolicyVersions@, don\'t include
-- the @TrafficPolicyVersionMarker@ parameter.
--
-- If you have more traffic policy versions than the value of @MaxItems@,
-- @ListTrafficPolicyVersions@ returns only the first group of @MaxItems@
-- versions. To get more traffic policy versions, submit another
-- @ListTrafficPolicyVersions@ request. For the value of
-- @TrafficPolicyVersionMarker@, specify the value of
-- @TrafficPolicyVersionMarker@ in the previous response.
--
-- 'maxItems', 'listTrafficPolicyVersions_maxItems' - The maximum number of traffic policy versions that you want Amazon Route
-- 53 to include in the response body for this request. If the specified
-- traffic policy has more than @MaxItems@ versions, the value of
-- @IsTruncated@ in the response is @true@, and the value of the
-- @TrafficPolicyVersionMarker@ element is the ID of the first version that
-- Route 53 will return if you submit another request.
--
-- 'id', 'listTrafficPolicyVersions_id' - Specify the value of @Id@ of the traffic policy for which you want to
-- list all versions.
newListTrafficPolicyVersions ::
  -- | 'id'
  Prelude.Text ->
  ListTrafficPolicyVersions
newListTrafficPolicyVersions pId_ =
  ListTrafficPolicyVersions'
    { trafficPolicyVersionMarker =
        Prelude.Nothing,
      maxItems = Prelude.Nothing,
      id = pId_
    }

-- | For your first request to @ListTrafficPolicyVersions@, don\'t include
-- the @TrafficPolicyVersionMarker@ parameter.
--
-- If you have more traffic policy versions than the value of @MaxItems@,
-- @ListTrafficPolicyVersions@ returns only the first group of @MaxItems@
-- versions. To get more traffic policy versions, submit another
-- @ListTrafficPolicyVersions@ request. For the value of
-- @TrafficPolicyVersionMarker@, specify the value of
-- @TrafficPolicyVersionMarker@ in the previous response.
listTrafficPolicyVersions_trafficPolicyVersionMarker :: Lens.Lens' ListTrafficPolicyVersions (Prelude.Maybe Prelude.Text)
listTrafficPolicyVersions_trafficPolicyVersionMarker = Lens.lens (\ListTrafficPolicyVersions' {trafficPolicyVersionMarker} -> trafficPolicyVersionMarker) (\s@ListTrafficPolicyVersions' {} a -> s {trafficPolicyVersionMarker = a} :: ListTrafficPolicyVersions)

-- | The maximum number of traffic policy versions that you want Amazon Route
-- 53 to include in the response body for this request. If the specified
-- traffic policy has more than @MaxItems@ versions, the value of
-- @IsTruncated@ in the response is @true@, and the value of the
-- @TrafficPolicyVersionMarker@ element is the ID of the first version that
-- Route 53 will return if you submit another request.
listTrafficPolicyVersions_maxItems :: Lens.Lens' ListTrafficPolicyVersions (Prelude.Maybe Prelude.Text)
listTrafficPolicyVersions_maxItems = Lens.lens (\ListTrafficPolicyVersions' {maxItems} -> maxItems) (\s@ListTrafficPolicyVersions' {} a -> s {maxItems = a} :: ListTrafficPolicyVersions)

-- | Specify the value of @Id@ of the traffic policy for which you want to
-- list all versions.
listTrafficPolicyVersions_id :: Lens.Lens' ListTrafficPolicyVersions Prelude.Text
listTrafficPolicyVersions_id = Lens.lens (\ListTrafficPolicyVersions' {id} -> id) (\s@ListTrafficPolicyVersions' {} a -> s {id = a} :: ListTrafficPolicyVersions)

instance Core.AWSRequest ListTrafficPolicyVersions where
  type
    AWSResponse ListTrafficPolicyVersions =
      ListTrafficPolicyVersionsResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ListTrafficPolicyVersionsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..@? "TrafficPolicies" Core..!@ Prelude.mempty
                            Prelude.>>= Core.parseXMLList "TrafficPolicy"
                        )
            Prelude.<*> (x Core..@ "IsTruncated")
            Prelude.<*> (x Core..@ "TrafficPolicyVersionMarker")
            Prelude.<*> (x Core..@ "MaxItems")
      )

instance Prelude.Hashable ListTrafficPolicyVersions

instance Prelude.NFData ListTrafficPolicyVersions

instance Core.ToHeaders ListTrafficPolicyVersions where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListTrafficPolicyVersions where
  toPath ListTrafficPolicyVersions' {..} =
    Prelude.mconcat
      [ "/2013-04-01/trafficpolicies/",
        Core.toBS id,
        "/versions"
      ]

instance Core.ToQuery ListTrafficPolicyVersions where
  toQuery ListTrafficPolicyVersions' {..} =
    Prelude.mconcat
      [ "trafficpolicyversion"
          Core.=: trafficPolicyVersionMarker,
        "maxitems" Core.=: maxItems
      ]

-- | A complex type that contains the response information for the request.
--
-- /See:/ 'newListTrafficPolicyVersionsResponse' smart constructor.
data ListTrafficPolicyVersionsResponse = ListTrafficPolicyVersionsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list that contains one @TrafficPolicy@ element for each traffic policy
    -- version that is associated with the specified traffic policy.
    trafficPolicies :: [TrafficPolicy],
    -- | A flag that indicates whether there are more traffic policies to be
    -- listed. If the response was truncated, you can get the next group of
    -- traffic policies by submitting another @ListTrafficPolicyVersions@
    -- request and specifying the value of @NextMarker@ in the @marker@
    -- parameter.
    isTruncated :: Prelude.Bool,
    -- | If @IsTruncated@ is @true@, the value of @TrafficPolicyVersionMarker@
    -- identifies the first traffic policy that Amazon Route 53 will return if
    -- you submit another request. Call @ListTrafficPolicyVersions@ again and
    -- specify the value of @TrafficPolicyVersionMarker@ in the
    -- @TrafficPolicyVersionMarker@ request parameter.
    --
    -- This element is present only if @IsTruncated@ is @true@.
    trafficPolicyVersionMarker :: Prelude.Text,
    -- | The value that you specified for the @maxitems@ parameter in the
    -- @ListTrafficPolicyVersions@ request that produced the current response.
    maxItems :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTrafficPolicyVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'listTrafficPolicyVersionsResponse_httpStatus' - The response's http status code.
--
-- 'trafficPolicies', 'listTrafficPolicyVersionsResponse_trafficPolicies' - A list that contains one @TrafficPolicy@ element for each traffic policy
-- version that is associated with the specified traffic policy.
--
-- 'isTruncated', 'listTrafficPolicyVersionsResponse_isTruncated' - A flag that indicates whether there are more traffic policies to be
-- listed. If the response was truncated, you can get the next group of
-- traffic policies by submitting another @ListTrafficPolicyVersions@
-- request and specifying the value of @NextMarker@ in the @marker@
-- parameter.
--
-- 'trafficPolicyVersionMarker', 'listTrafficPolicyVersionsResponse_trafficPolicyVersionMarker' - If @IsTruncated@ is @true@, the value of @TrafficPolicyVersionMarker@
-- identifies the first traffic policy that Amazon Route 53 will return if
-- you submit another request. Call @ListTrafficPolicyVersions@ again and
-- specify the value of @TrafficPolicyVersionMarker@ in the
-- @TrafficPolicyVersionMarker@ request parameter.
--
-- This element is present only if @IsTruncated@ is @true@.
--
-- 'maxItems', 'listTrafficPolicyVersionsResponse_maxItems' - The value that you specified for the @maxitems@ parameter in the
-- @ListTrafficPolicyVersions@ request that produced the current response.
newListTrafficPolicyVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'isTruncated'
  Prelude.Bool ->
  -- | 'trafficPolicyVersionMarker'
  Prelude.Text ->
  -- | 'maxItems'
  Prelude.Text ->
  ListTrafficPolicyVersionsResponse
newListTrafficPolicyVersionsResponse
  pHttpStatus_
  pIsTruncated_
  pTrafficPolicyVersionMarker_
  pMaxItems_ =
    ListTrafficPolicyVersionsResponse'
      { httpStatus =
          pHttpStatus_,
        trafficPolicies = Prelude.mempty,
        isTruncated = pIsTruncated_,
        trafficPolicyVersionMarker =
          pTrafficPolicyVersionMarker_,
        maxItems = pMaxItems_
      }

-- | The response's http status code.
listTrafficPolicyVersionsResponse_httpStatus :: Lens.Lens' ListTrafficPolicyVersionsResponse Prelude.Int
listTrafficPolicyVersionsResponse_httpStatus = Lens.lens (\ListTrafficPolicyVersionsResponse' {httpStatus} -> httpStatus) (\s@ListTrafficPolicyVersionsResponse' {} a -> s {httpStatus = a} :: ListTrafficPolicyVersionsResponse)

-- | A list that contains one @TrafficPolicy@ element for each traffic policy
-- version that is associated with the specified traffic policy.
listTrafficPolicyVersionsResponse_trafficPolicies :: Lens.Lens' ListTrafficPolicyVersionsResponse [TrafficPolicy]
listTrafficPolicyVersionsResponse_trafficPolicies = Lens.lens (\ListTrafficPolicyVersionsResponse' {trafficPolicies} -> trafficPolicies) (\s@ListTrafficPolicyVersionsResponse' {} a -> s {trafficPolicies = a} :: ListTrafficPolicyVersionsResponse) Prelude.. Lens._Coerce

-- | A flag that indicates whether there are more traffic policies to be
-- listed. If the response was truncated, you can get the next group of
-- traffic policies by submitting another @ListTrafficPolicyVersions@
-- request and specifying the value of @NextMarker@ in the @marker@
-- parameter.
listTrafficPolicyVersionsResponse_isTruncated :: Lens.Lens' ListTrafficPolicyVersionsResponse Prelude.Bool
listTrafficPolicyVersionsResponse_isTruncated = Lens.lens (\ListTrafficPolicyVersionsResponse' {isTruncated} -> isTruncated) (\s@ListTrafficPolicyVersionsResponse' {} a -> s {isTruncated = a} :: ListTrafficPolicyVersionsResponse)

-- | If @IsTruncated@ is @true@, the value of @TrafficPolicyVersionMarker@
-- identifies the first traffic policy that Amazon Route 53 will return if
-- you submit another request. Call @ListTrafficPolicyVersions@ again and
-- specify the value of @TrafficPolicyVersionMarker@ in the
-- @TrafficPolicyVersionMarker@ request parameter.
--
-- This element is present only if @IsTruncated@ is @true@.
listTrafficPolicyVersionsResponse_trafficPolicyVersionMarker :: Lens.Lens' ListTrafficPolicyVersionsResponse Prelude.Text
listTrafficPolicyVersionsResponse_trafficPolicyVersionMarker = Lens.lens (\ListTrafficPolicyVersionsResponse' {trafficPolicyVersionMarker} -> trafficPolicyVersionMarker) (\s@ListTrafficPolicyVersionsResponse' {} a -> s {trafficPolicyVersionMarker = a} :: ListTrafficPolicyVersionsResponse)

-- | The value that you specified for the @maxitems@ parameter in the
-- @ListTrafficPolicyVersions@ request that produced the current response.
listTrafficPolicyVersionsResponse_maxItems :: Lens.Lens' ListTrafficPolicyVersionsResponse Prelude.Text
listTrafficPolicyVersionsResponse_maxItems = Lens.lens (\ListTrafficPolicyVersionsResponse' {maxItems} -> maxItems) (\s@ListTrafficPolicyVersionsResponse' {} a -> s {maxItems = a} :: ListTrafficPolicyVersionsResponse)

instance
  Prelude.NFData
    ListTrafficPolicyVersionsResponse
