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
-- Module      : Network.AWS.Route53.ListReusableDelegationSets
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of the reusable delegation sets that are associated
-- with the current AWS account.
module Network.AWS.Route53.ListReusableDelegationSets
  ( -- * Creating a Request
    ListReusableDelegationSets (..),
    newListReusableDelegationSets,

    -- * Request Lenses
    listReusableDelegationSets_maxItems,
    listReusableDelegationSets_marker,

    -- * Destructuring the Response
    ListReusableDelegationSetsResponse (..),
    newListReusableDelegationSetsResponse,

    -- * Response Lenses
    listReusableDelegationSetsResponse_nextMarker,
    listReusableDelegationSetsResponse_httpStatus,
    listReusableDelegationSetsResponse_delegationSets,
    listReusableDelegationSetsResponse_marker,
    listReusableDelegationSetsResponse_isTruncated,
    listReusableDelegationSetsResponse_maxItems,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53.Types

-- | A request to get a list of the reusable delegation sets that are
-- associated with the current AWS account.
--
-- /See:/ 'newListReusableDelegationSets' smart constructor.
data ListReusableDelegationSets = ListReusableDelegationSets'
  { -- | The number of reusable delegation sets that you want Amazon Route 53 to
    -- return in the response to this request. If you specify a value greater
    -- than 100, Route 53 returns only the first 100 reusable delegation sets.
    maxItems :: Core.Maybe Core.Text,
    -- | If the value of @IsTruncated@ in the previous response was @true@, you
    -- have more reusable delegation sets. To get another group, submit another
    -- @ListReusableDelegationSets@ request.
    --
    -- For the value of @marker@, specify the value of @NextMarker@ from the
    -- previous response, which is the ID of the first reusable delegation set
    -- that Amazon Route 53 will return if you submit another request.
    --
    -- If the value of @IsTruncated@ in the previous response was @false@,
    -- there are no more reusable delegation sets to get.
    marker :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListReusableDelegationSets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxItems', 'listReusableDelegationSets_maxItems' - The number of reusable delegation sets that you want Amazon Route 53 to
-- return in the response to this request. If you specify a value greater
-- than 100, Route 53 returns only the first 100 reusable delegation sets.
--
-- 'marker', 'listReusableDelegationSets_marker' - If the value of @IsTruncated@ in the previous response was @true@, you
-- have more reusable delegation sets. To get another group, submit another
-- @ListReusableDelegationSets@ request.
--
-- For the value of @marker@, specify the value of @NextMarker@ from the
-- previous response, which is the ID of the first reusable delegation set
-- that Amazon Route 53 will return if you submit another request.
--
-- If the value of @IsTruncated@ in the previous response was @false@,
-- there are no more reusable delegation sets to get.
newListReusableDelegationSets ::
  ListReusableDelegationSets
newListReusableDelegationSets =
  ListReusableDelegationSets'
    { maxItems =
        Core.Nothing,
      marker = Core.Nothing
    }

-- | The number of reusable delegation sets that you want Amazon Route 53 to
-- return in the response to this request. If you specify a value greater
-- than 100, Route 53 returns only the first 100 reusable delegation sets.
listReusableDelegationSets_maxItems :: Lens.Lens' ListReusableDelegationSets (Core.Maybe Core.Text)
listReusableDelegationSets_maxItems = Lens.lens (\ListReusableDelegationSets' {maxItems} -> maxItems) (\s@ListReusableDelegationSets' {} a -> s {maxItems = a} :: ListReusableDelegationSets)

-- | If the value of @IsTruncated@ in the previous response was @true@, you
-- have more reusable delegation sets. To get another group, submit another
-- @ListReusableDelegationSets@ request.
--
-- For the value of @marker@, specify the value of @NextMarker@ from the
-- previous response, which is the ID of the first reusable delegation set
-- that Amazon Route 53 will return if you submit another request.
--
-- If the value of @IsTruncated@ in the previous response was @false@,
-- there are no more reusable delegation sets to get.
listReusableDelegationSets_marker :: Lens.Lens' ListReusableDelegationSets (Core.Maybe Core.Text)
listReusableDelegationSets_marker = Lens.lens (\ListReusableDelegationSets' {marker} -> marker) (\s@ListReusableDelegationSets' {} a -> s {marker = a} :: ListReusableDelegationSets)

instance Core.AWSRequest ListReusableDelegationSets where
  type
    AWSResponse ListReusableDelegationSets =
      ListReusableDelegationSetsResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ListReusableDelegationSetsResponse'
            Core.<$> (x Core..@? "NextMarker")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> ( x Core..@? "DelegationSets" Core..!@ Core.mempty
                         Core.>>= Core.parseXMLList "DelegationSet"
                     )
            Core.<*> (x Core..@ "Marker")
            Core.<*> (x Core..@ "IsTruncated")
            Core.<*> (x Core..@ "MaxItems")
      )

instance Core.Hashable ListReusableDelegationSets

instance Core.NFData ListReusableDelegationSets

instance Core.ToHeaders ListReusableDelegationSets where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListReusableDelegationSets where
  toPath = Core.const "/2013-04-01/delegationset"

instance Core.ToQuery ListReusableDelegationSets where
  toQuery ListReusableDelegationSets' {..} =
    Core.mconcat
      [ "maxitems" Core.=: maxItems,
        "marker" Core.=: marker
      ]

-- | A complex type that contains information about the reusable delegation
-- sets that are associated with the current AWS account.
--
-- /See:/ 'newListReusableDelegationSetsResponse' smart constructor.
data ListReusableDelegationSetsResponse = ListReusableDelegationSetsResponse'
  { -- | If @IsTruncated@ is @true@, the value of @NextMarker@ identifies the
    -- next reusable delegation set that Amazon Route 53 will return if you
    -- submit another @ListReusableDelegationSets@ request and specify the
    -- value of @NextMarker@ in the @marker@ parameter.
    nextMarker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A complex type that contains one @DelegationSet@ element for each
    -- reusable delegation set that was created by the current AWS account.
    delegationSets :: [DelegationSet],
    -- | For the second and subsequent calls to @ListReusableDelegationSets@,
    -- @Marker@ is the value that you specified for the @marker@ parameter in
    -- the request that produced the current response.
    marker :: Core.Text,
    -- | A flag that indicates whether there are more reusable delegation sets to
    -- be listed.
    isTruncated :: Core.Bool,
    -- | The value that you specified for the @maxitems@ parameter in the call to
    -- @ListReusableDelegationSets@ that produced the current response.
    maxItems :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListReusableDelegationSetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextMarker', 'listReusableDelegationSetsResponse_nextMarker' - If @IsTruncated@ is @true@, the value of @NextMarker@ identifies the
-- next reusable delegation set that Amazon Route 53 will return if you
-- submit another @ListReusableDelegationSets@ request and specify the
-- value of @NextMarker@ in the @marker@ parameter.
--
-- 'httpStatus', 'listReusableDelegationSetsResponse_httpStatus' - The response's http status code.
--
-- 'delegationSets', 'listReusableDelegationSetsResponse_delegationSets' - A complex type that contains one @DelegationSet@ element for each
-- reusable delegation set that was created by the current AWS account.
--
-- 'marker', 'listReusableDelegationSetsResponse_marker' - For the second and subsequent calls to @ListReusableDelegationSets@,
-- @Marker@ is the value that you specified for the @marker@ parameter in
-- the request that produced the current response.
--
-- 'isTruncated', 'listReusableDelegationSetsResponse_isTruncated' - A flag that indicates whether there are more reusable delegation sets to
-- be listed.
--
-- 'maxItems', 'listReusableDelegationSetsResponse_maxItems' - The value that you specified for the @maxitems@ parameter in the call to
-- @ListReusableDelegationSets@ that produced the current response.
newListReusableDelegationSetsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'marker'
  Core.Text ->
  -- | 'isTruncated'
  Core.Bool ->
  -- | 'maxItems'
  Core.Text ->
  ListReusableDelegationSetsResponse
newListReusableDelegationSetsResponse
  pHttpStatus_
  pMarker_
  pIsTruncated_
  pMaxItems_ =
    ListReusableDelegationSetsResponse'
      { nextMarker =
          Core.Nothing,
        httpStatus = pHttpStatus_,
        delegationSets = Core.mempty,
        marker = pMarker_,
        isTruncated = pIsTruncated_,
        maxItems = pMaxItems_
      }

-- | If @IsTruncated@ is @true@, the value of @NextMarker@ identifies the
-- next reusable delegation set that Amazon Route 53 will return if you
-- submit another @ListReusableDelegationSets@ request and specify the
-- value of @NextMarker@ in the @marker@ parameter.
listReusableDelegationSetsResponse_nextMarker :: Lens.Lens' ListReusableDelegationSetsResponse (Core.Maybe Core.Text)
listReusableDelegationSetsResponse_nextMarker = Lens.lens (\ListReusableDelegationSetsResponse' {nextMarker} -> nextMarker) (\s@ListReusableDelegationSetsResponse' {} a -> s {nextMarker = a} :: ListReusableDelegationSetsResponse)

-- | The response's http status code.
listReusableDelegationSetsResponse_httpStatus :: Lens.Lens' ListReusableDelegationSetsResponse Core.Int
listReusableDelegationSetsResponse_httpStatus = Lens.lens (\ListReusableDelegationSetsResponse' {httpStatus} -> httpStatus) (\s@ListReusableDelegationSetsResponse' {} a -> s {httpStatus = a} :: ListReusableDelegationSetsResponse)

-- | A complex type that contains one @DelegationSet@ element for each
-- reusable delegation set that was created by the current AWS account.
listReusableDelegationSetsResponse_delegationSets :: Lens.Lens' ListReusableDelegationSetsResponse [DelegationSet]
listReusableDelegationSetsResponse_delegationSets = Lens.lens (\ListReusableDelegationSetsResponse' {delegationSets} -> delegationSets) (\s@ListReusableDelegationSetsResponse' {} a -> s {delegationSets = a} :: ListReusableDelegationSetsResponse) Core.. Lens._Coerce

-- | For the second and subsequent calls to @ListReusableDelegationSets@,
-- @Marker@ is the value that you specified for the @marker@ parameter in
-- the request that produced the current response.
listReusableDelegationSetsResponse_marker :: Lens.Lens' ListReusableDelegationSetsResponse Core.Text
listReusableDelegationSetsResponse_marker = Lens.lens (\ListReusableDelegationSetsResponse' {marker} -> marker) (\s@ListReusableDelegationSetsResponse' {} a -> s {marker = a} :: ListReusableDelegationSetsResponse)

-- | A flag that indicates whether there are more reusable delegation sets to
-- be listed.
listReusableDelegationSetsResponse_isTruncated :: Lens.Lens' ListReusableDelegationSetsResponse Core.Bool
listReusableDelegationSetsResponse_isTruncated = Lens.lens (\ListReusableDelegationSetsResponse' {isTruncated} -> isTruncated) (\s@ListReusableDelegationSetsResponse' {} a -> s {isTruncated = a} :: ListReusableDelegationSetsResponse)

-- | The value that you specified for the @maxitems@ parameter in the call to
-- @ListReusableDelegationSets@ that produced the current response.
listReusableDelegationSetsResponse_maxItems :: Lens.Lens' ListReusableDelegationSetsResponse Core.Text
listReusableDelegationSetsResponse_maxItems = Lens.lens (\ListReusableDelegationSetsResponse' {maxItems} -> maxItems) (\s@ListReusableDelegationSetsResponse' {} a -> s {maxItems = a} :: ListReusableDelegationSetsResponse)

instance
  Core.NFData
    ListReusableDelegationSetsResponse
