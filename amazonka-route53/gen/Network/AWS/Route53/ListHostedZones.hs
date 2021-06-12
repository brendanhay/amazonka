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
-- Module      : Network.AWS.Route53.ListHostedZones
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of the public and private hosted zones that are
-- associated with the current AWS account. The response includes a
-- @HostedZones@ child element for each hosted zone.
--
-- Amazon Route 53 returns a maximum of 100 items in each response. If you
-- have a lot of hosted zones, you can use the @maxitems@ parameter to list
-- them in groups of up to 100.
--
-- This operation returns paginated results.
module Network.AWS.Route53.ListHostedZones
  ( -- * Creating a Request
    ListHostedZones (..),
    newListHostedZones,

    -- * Request Lenses
    listHostedZones_delegationSetId,
    listHostedZones_maxItems,
    listHostedZones_marker,

    -- * Destructuring the Response
    ListHostedZonesResponse (..),
    newListHostedZonesResponse,

    -- * Response Lenses
    listHostedZonesResponse_nextMarker,
    listHostedZonesResponse_marker,
    listHostedZonesResponse_httpStatus,
    listHostedZonesResponse_hostedZones,
    listHostedZonesResponse_isTruncated,
    listHostedZonesResponse_maxItems,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53.Types

-- | A request to retrieve a list of the public and private hosted zones that
-- are associated with the current AWS account.
--
-- /See:/ 'newListHostedZones' smart constructor.
data ListHostedZones = ListHostedZones'
  { -- | If you\'re using reusable delegation sets and you want to list all of
    -- the hosted zones that are associated with a reusable delegation set,
    -- specify the ID of that reusable delegation set.
    delegationSetId :: Core.Maybe ResourceId,
    -- | (Optional) The maximum number of hosted zones that you want Amazon Route
    -- 53 to return. If you have more than @maxitems@ hosted zones, the value
    -- of @IsTruncated@ in the response is @true@, and the value of
    -- @NextMarker@ is the hosted zone ID of the first hosted zone that Route
    -- 53 will return if you submit another request.
    maxItems :: Core.Maybe Core.Text,
    -- | If the value of @IsTruncated@ in the previous response was @true@, you
    -- have more hosted zones. To get more hosted zones, submit another
    -- @ListHostedZones@ request.
    --
    -- For the value of @marker@, specify the value of @NextMarker@ from the
    -- previous response, which is the ID of the first hosted zone that Amazon
    -- Route 53 will return if you submit another request.
    --
    -- If the value of @IsTruncated@ in the previous response was @false@,
    -- there are no more hosted zones to get.
    marker :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListHostedZones' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'delegationSetId', 'listHostedZones_delegationSetId' - If you\'re using reusable delegation sets and you want to list all of
-- the hosted zones that are associated with a reusable delegation set,
-- specify the ID of that reusable delegation set.
--
-- 'maxItems', 'listHostedZones_maxItems' - (Optional) The maximum number of hosted zones that you want Amazon Route
-- 53 to return. If you have more than @maxitems@ hosted zones, the value
-- of @IsTruncated@ in the response is @true@, and the value of
-- @NextMarker@ is the hosted zone ID of the first hosted zone that Route
-- 53 will return if you submit another request.
--
-- 'marker', 'listHostedZones_marker' - If the value of @IsTruncated@ in the previous response was @true@, you
-- have more hosted zones. To get more hosted zones, submit another
-- @ListHostedZones@ request.
--
-- For the value of @marker@, specify the value of @NextMarker@ from the
-- previous response, which is the ID of the first hosted zone that Amazon
-- Route 53 will return if you submit another request.
--
-- If the value of @IsTruncated@ in the previous response was @false@,
-- there are no more hosted zones to get.
newListHostedZones ::
  ListHostedZones
newListHostedZones =
  ListHostedZones'
    { delegationSetId = Core.Nothing,
      maxItems = Core.Nothing,
      marker = Core.Nothing
    }

-- | If you\'re using reusable delegation sets and you want to list all of
-- the hosted zones that are associated with a reusable delegation set,
-- specify the ID of that reusable delegation set.
listHostedZones_delegationSetId :: Lens.Lens' ListHostedZones (Core.Maybe ResourceId)
listHostedZones_delegationSetId = Lens.lens (\ListHostedZones' {delegationSetId} -> delegationSetId) (\s@ListHostedZones' {} a -> s {delegationSetId = a} :: ListHostedZones)

-- | (Optional) The maximum number of hosted zones that you want Amazon Route
-- 53 to return. If you have more than @maxitems@ hosted zones, the value
-- of @IsTruncated@ in the response is @true@, and the value of
-- @NextMarker@ is the hosted zone ID of the first hosted zone that Route
-- 53 will return if you submit another request.
listHostedZones_maxItems :: Lens.Lens' ListHostedZones (Core.Maybe Core.Text)
listHostedZones_maxItems = Lens.lens (\ListHostedZones' {maxItems} -> maxItems) (\s@ListHostedZones' {} a -> s {maxItems = a} :: ListHostedZones)

-- | If the value of @IsTruncated@ in the previous response was @true@, you
-- have more hosted zones. To get more hosted zones, submit another
-- @ListHostedZones@ request.
--
-- For the value of @marker@, specify the value of @NextMarker@ from the
-- previous response, which is the ID of the first hosted zone that Amazon
-- Route 53 will return if you submit another request.
--
-- If the value of @IsTruncated@ in the previous response was @false@,
-- there are no more hosted zones to get.
listHostedZones_marker :: Lens.Lens' ListHostedZones (Core.Maybe Core.Text)
listHostedZones_marker = Lens.lens (\ListHostedZones' {marker} -> marker) (\s@ListHostedZones' {} a -> s {marker = a} :: ListHostedZones)

instance Core.AWSPager ListHostedZones where
  page rq rs
    | Core.stop
        (rs Lens.^. listHostedZonesResponse_isTruncated) =
      Core.Nothing
    | Core.isNothing
        ( rs
            Lens.^? listHostedZonesResponse_nextMarker Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listHostedZones_marker
          Lens..~ rs
          Lens.^? listHostedZonesResponse_nextMarker Core.. Lens._Just

instance Core.AWSRequest ListHostedZones where
  type
    AWSResponse ListHostedZones =
      ListHostedZonesResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ListHostedZonesResponse'
            Core.<$> (x Core..@? "NextMarker")
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> ( x Core..@? "HostedZones" Core..!@ Core.mempty
                         Core.>>= Core.parseXMLList "HostedZone"
                     )
            Core.<*> (x Core..@ "IsTruncated")
            Core.<*> (x Core..@ "MaxItems")
      )

instance Core.Hashable ListHostedZones

instance Core.NFData ListHostedZones

instance Core.ToHeaders ListHostedZones where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListHostedZones where
  toPath = Core.const "/2013-04-01/hostedzone"

instance Core.ToQuery ListHostedZones where
  toQuery ListHostedZones' {..} =
    Core.mconcat
      [ "delegationsetid" Core.=: delegationSetId,
        "maxitems" Core.=: maxItems,
        "marker" Core.=: marker
      ]

-- | /See:/ 'newListHostedZonesResponse' smart constructor.
data ListHostedZonesResponse = ListHostedZonesResponse'
  { -- | If @IsTruncated@ is @true@, the value of @NextMarker@ identifies the
    -- first hosted zone in the next group of hosted zones. Submit another
    -- @ListHostedZones@ request, and specify the value of @NextMarker@ from
    -- the response in the @marker@ parameter.
    --
    -- This element is present only if @IsTruncated@ is @true@.
    nextMarker :: Core.Maybe Core.Text,
    -- | For the second and subsequent calls to @ListHostedZones@, @Marker@ is
    -- the value that you specified for the @marker@ parameter in the request
    -- that produced the current response.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A complex type that contains general information about the hosted zone.
    hostedZones :: [HostedZone],
    -- | A flag indicating whether there are more hosted zones to be listed. If
    -- the response was truncated, you can get more hosted zones by submitting
    -- another @ListHostedZones@ request and specifying the value of
    -- @NextMarker@ in the @marker@ parameter.
    isTruncated :: Core.Bool,
    -- | The value that you specified for the @maxitems@ parameter in the call to
    -- @ListHostedZones@ that produced the current response.
    maxItems :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListHostedZonesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextMarker', 'listHostedZonesResponse_nextMarker' - If @IsTruncated@ is @true@, the value of @NextMarker@ identifies the
-- first hosted zone in the next group of hosted zones. Submit another
-- @ListHostedZones@ request, and specify the value of @NextMarker@ from
-- the response in the @marker@ parameter.
--
-- This element is present only if @IsTruncated@ is @true@.
--
-- 'marker', 'listHostedZonesResponse_marker' - For the second and subsequent calls to @ListHostedZones@, @Marker@ is
-- the value that you specified for the @marker@ parameter in the request
-- that produced the current response.
--
-- 'httpStatus', 'listHostedZonesResponse_httpStatus' - The response's http status code.
--
-- 'hostedZones', 'listHostedZonesResponse_hostedZones' - A complex type that contains general information about the hosted zone.
--
-- 'isTruncated', 'listHostedZonesResponse_isTruncated' - A flag indicating whether there are more hosted zones to be listed. If
-- the response was truncated, you can get more hosted zones by submitting
-- another @ListHostedZones@ request and specifying the value of
-- @NextMarker@ in the @marker@ parameter.
--
-- 'maxItems', 'listHostedZonesResponse_maxItems' - The value that you specified for the @maxitems@ parameter in the call to
-- @ListHostedZones@ that produced the current response.
newListHostedZonesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'isTruncated'
  Core.Bool ->
  -- | 'maxItems'
  Core.Text ->
  ListHostedZonesResponse
newListHostedZonesResponse
  pHttpStatus_
  pIsTruncated_
  pMaxItems_ =
    ListHostedZonesResponse'
      { nextMarker = Core.Nothing,
        marker = Core.Nothing,
        httpStatus = pHttpStatus_,
        hostedZones = Core.mempty,
        isTruncated = pIsTruncated_,
        maxItems = pMaxItems_
      }

-- | If @IsTruncated@ is @true@, the value of @NextMarker@ identifies the
-- first hosted zone in the next group of hosted zones. Submit another
-- @ListHostedZones@ request, and specify the value of @NextMarker@ from
-- the response in the @marker@ parameter.
--
-- This element is present only if @IsTruncated@ is @true@.
listHostedZonesResponse_nextMarker :: Lens.Lens' ListHostedZonesResponse (Core.Maybe Core.Text)
listHostedZonesResponse_nextMarker = Lens.lens (\ListHostedZonesResponse' {nextMarker} -> nextMarker) (\s@ListHostedZonesResponse' {} a -> s {nextMarker = a} :: ListHostedZonesResponse)

-- | For the second and subsequent calls to @ListHostedZones@, @Marker@ is
-- the value that you specified for the @marker@ parameter in the request
-- that produced the current response.
listHostedZonesResponse_marker :: Lens.Lens' ListHostedZonesResponse (Core.Maybe Core.Text)
listHostedZonesResponse_marker = Lens.lens (\ListHostedZonesResponse' {marker} -> marker) (\s@ListHostedZonesResponse' {} a -> s {marker = a} :: ListHostedZonesResponse)

-- | The response's http status code.
listHostedZonesResponse_httpStatus :: Lens.Lens' ListHostedZonesResponse Core.Int
listHostedZonesResponse_httpStatus = Lens.lens (\ListHostedZonesResponse' {httpStatus} -> httpStatus) (\s@ListHostedZonesResponse' {} a -> s {httpStatus = a} :: ListHostedZonesResponse)

-- | A complex type that contains general information about the hosted zone.
listHostedZonesResponse_hostedZones :: Lens.Lens' ListHostedZonesResponse [HostedZone]
listHostedZonesResponse_hostedZones = Lens.lens (\ListHostedZonesResponse' {hostedZones} -> hostedZones) (\s@ListHostedZonesResponse' {} a -> s {hostedZones = a} :: ListHostedZonesResponse) Core.. Lens._Coerce

-- | A flag indicating whether there are more hosted zones to be listed. If
-- the response was truncated, you can get more hosted zones by submitting
-- another @ListHostedZones@ request and specifying the value of
-- @NextMarker@ in the @marker@ parameter.
listHostedZonesResponse_isTruncated :: Lens.Lens' ListHostedZonesResponse Core.Bool
listHostedZonesResponse_isTruncated = Lens.lens (\ListHostedZonesResponse' {isTruncated} -> isTruncated) (\s@ListHostedZonesResponse' {} a -> s {isTruncated = a} :: ListHostedZonesResponse)

-- | The value that you specified for the @maxitems@ parameter in the call to
-- @ListHostedZones@ that produced the current response.
listHostedZonesResponse_maxItems :: Lens.Lens' ListHostedZonesResponse Core.Text
listHostedZonesResponse_maxItems = Lens.lens (\ListHostedZonesResponse' {maxItems} -> maxItems) (\s@ListHostedZonesResponse' {} a -> s {maxItems = a} :: ListHostedZonesResponse)

instance Core.NFData ListHostedZonesResponse
