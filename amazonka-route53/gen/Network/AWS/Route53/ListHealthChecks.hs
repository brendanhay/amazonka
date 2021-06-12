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
-- Module      : Network.AWS.Route53.ListHealthChecks
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve a list of the health checks that are associated with the
-- current AWS account.
--
-- This operation returns paginated results.
module Network.AWS.Route53.ListHealthChecks
  ( -- * Creating a Request
    ListHealthChecks (..),
    newListHealthChecks,

    -- * Request Lenses
    listHealthChecks_maxItems,
    listHealthChecks_marker,

    -- * Destructuring the Response
    ListHealthChecksResponse (..),
    newListHealthChecksResponse,

    -- * Response Lenses
    listHealthChecksResponse_nextMarker,
    listHealthChecksResponse_httpStatus,
    listHealthChecksResponse_healthChecks,
    listHealthChecksResponse_marker,
    listHealthChecksResponse_isTruncated,
    listHealthChecksResponse_maxItems,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53.Types

-- | A request to retrieve a list of the health checks that are associated
-- with the current AWS account.
--
-- /See:/ 'newListHealthChecks' smart constructor.
data ListHealthChecks = ListHealthChecks'
  { -- | The maximum number of health checks that you want @ListHealthChecks@ to
    -- return in response to the current request. Amazon Route 53 returns a
    -- maximum of 100 items. If you set @MaxItems@ to a value greater than 100,
    -- Route 53 returns only the first 100 health checks.
    maxItems :: Core.Maybe Core.Text,
    -- | If the value of @IsTruncated@ in the previous response was @true@, you
    -- have more health checks. To get another group, submit another
    -- @ListHealthChecks@ request.
    --
    -- For the value of @marker@, specify the value of @NextMarker@ from the
    -- previous response, which is the ID of the first health check that Amazon
    -- Route 53 will return if you submit another request.
    --
    -- If the value of @IsTruncated@ in the previous response was @false@,
    -- there are no more health checks to get.
    marker :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListHealthChecks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxItems', 'listHealthChecks_maxItems' - The maximum number of health checks that you want @ListHealthChecks@ to
-- return in response to the current request. Amazon Route 53 returns a
-- maximum of 100 items. If you set @MaxItems@ to a value greater than 100,
-- Route 53 returns only the first 100 health checks.
--
-- 'marker', 'listHealthChecks_marker' - If the value of @IsTruncated@ in the previous response was @true@, you
-- have more health checks. To get another group, submit another
-- @ListHealthChecks@ request.
--
-- For the value of @marker@, specify the value of @NextMarker@ from the
-- previous response, which is the ID of the first health check that Amazon
-- Route 53 will return if you submit another request.
--
-- If the value of @IsTruncated@ in the previous response was @false@,
-- there are no more health checks to get.
newListHealthChecks ::
  ListHealthChecks
newListHealthChecks =
  ListHealthChecks'
    { maxItems = Core.Nothing,
      marker = Core.Nothing
    }

-- | The maximum number of health checks that you want @ListHealthChecks@ to
-- return in response to the current request. Amazon Route 53 returns a
-- maximum of 100 items. If you set @MaxItems@ to a value greater than 100,
-- Route 53 returns only the first 100 health checks.
listHealthChecks_maxItems :: Lens.Lens' ListHealthChecks (Core.Maybe Core.Text)
listHealthChecks_maxItems = Lens.lens (\ListHealthChecks' {maxItems} -> maxItems) (\s@ListHealthChecks' {} a -> s {maxItems = a} :: ListHealthChecks)

-- | If the value of @IsTruncated@ in the previous response was @true@, you
-- have more health checks. To get another group, submit another
-- @ListHealthChecks@ request.
--
-- For the value of @marker@, specify the value of @NextMarker@ from the
-- previous response, which is the ID of the first health check that Amazon
-- Route 53 will return if you submit another request.
--
-- If the value of @IsTruncated@ in the previous response was @false@,
-- there are no more health checks to get.
listHealthChecks_marker :: Lens.Lens' ListHealthChecks (Core.Maybe Core.Text)
listHealthChecks_marker = Lens.lens (\ListHealthChecks' {marker} -> marker) (\s@ListHealthChecks' {} a -> s {marker = a} :: ListHealthChecks)

instance Core.AWSPager ListHealthChecks where
  page rq rs
    | Core.stop
        (rs Lens.^. listHealthChecksResponse_isTruncated) =
      Core.Nothing
    | Core.isNothing
        ( rs
            Lens.^? listHealthChecksResponse_nextMarker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listHealthChecks_marker
          Lens..~ rs
          Lens.^? listHealthChecksResponse_nextMarker Core.. Lens._Just

instance Core.AWSRequest ListHealthChecks where
  type
    AWSResponse ListHealthChecks =
      ListHealthChecksResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ListHealthChecksResponse'
            Core.<$> (x Core..@? "NextMarker")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> ( x Core..@? "HealthChecks" Core..!@ Core.mempty
                         Core.>>= Core.parseXMLList "HealthCheck"
                     )
            Core.<*> (x Core..@ "Marker")
            Core.<*> (x Core..@ "IsTruncated")
            Core.<*> (x Core..@ "MaxItems")
      )

instance Core.Hashable ListHealthChecks

instance Core.NFData ListHealthChecks

instance Core.ToHeaders ListHealthChecks where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListHealthChecks where
  toPath = Core.const "/2013-04-01/healthcheck"

instance Core.ToQuery ListHealthChecks where
  toQuery ListHealthChecks' {..} =
    Core.mconcat
      [ "maxitems" Core.=: maxItems,
        "marker" Core.=: marker
      ]

-- | A complex type that contains the response to a @ListHealthChecks@
-- request.
--
-- /See:/ 'newListHealthChecksResponse' smart constructor.
data ListHealthChecksResponse = ListHealthChecksResponse'
  { -- | If @IsTruncated@ is @true@, the value of @NextMarker@ identifies the
    -- first health check that Amazon Route 53 returns if you submit another
    -- @ListHealthChecks@ request and specify the value of @NextMarker@ in the
    -- @marker@ parameter.
    nextMarker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A complex type that contains one @HealthCheck@ element for each health
    -- check that is associated with the current AWS account.
    healthChecks :: [HealthCheck],
    -- | For the second and subsequent calls to @ListHealthChecks@, @Marker@ is
    -- the value that you specified for the @marker@ parameter in the previous
    -- request.
    marker :: Core.Text,
    -- | A flag that indicates whether there are more health checks to be listed.
    -- If the response was truncated, you can get the next group of health
    -- checks by submitting another @ListHealthChecks@ request and specifying
    -- the value of @NextMarker@ in the @marker@ parameter.
    isTruncated :: Core.Bool,
    -- | The value that you specified for the @maxitems@ parameter in the call to
    -- @ListHealthChecks@ that produced the current response.
    maxItems :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListHealthChecksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextMarker', 'listHealthChecksResponse_nextMarker' - If @IsTruncated@ is @true@, the value of @NextMarker@ identifies the
-- first health check that Amazon Route 53 returns if you submit another
-- @ListHealthChecks@ request and specify the value of @NextMarker@ in the
-- @marker@ parameter.
--
-- 'httpStatus', 'listHealthChecksResponse_httpStatus' - The response's http status code.
--
-- 'healthChecks', 'listHealthChecksResponse_healthChecks' - A complex type that contains one @HealthCheck@ element for each health
-- check that is associated with the current AWS account.
--
-- 'marker', 'listHealthChecksResponse_marker' - For the second and subsequent calls to @ListHealthChecks@, @Marker@ is
-- the value that you specified for the @marker@ parameter in the previous
-- request.
--
-- 'isTruncated', 'listHealthChecksResponse_isTruncated' - A flag that indicates whether there are more health checks to be listed.
-- If the response was truncated, you can get the next group of health
-- checks by submitting another @ListHealthChecks@ request and specifying
-- the value of @NextMarker@ in the @marker@ parameter.
--
-- 'maxItems', 'listHealthChecksResponse_maxItems' - The value that you specified for the @maxitems@ parameter in the call to
-- @ListHealthChecks@ that produced the current response.
newListHealthChecksResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'marker'
  Core.Text ->
  -- | 'isTruncated'
  Core.Bool ->
  -- | 'maxItems'
  Core.Text ->
  ListHealthChecksResponse
newListHealthChecksResponse
  pHttpStatus_
  pMarker_
  pIsTruncated_
  pMaxItems_ =
    ListHealthChecksResponse'
      { nextMarker =
          Core.Nothing,
        httpStatus = pHttpStatus_,
        healthChecks = Core.mempty,
        marker = pMarker_,
        isTruncated = pIsTruncated_,
        maxItems = pMaxItems_
      }

-- | If @IsTruncated@ is @true@, the value of @NextMarker@ identifies the
-- first health check that Amazon Route 53 returns if you submit another
-- @ListHealthChecks@ request and specify the value of @NextMarker@ in the
-- @marker@ parameter.
listHealthChecksResponse_nextMarker :: Lens.Lens' ListHealthChecksResponse (Core.Maybe Core.Text)
listHealthChecksResponse_nextMarker = Lens.lens (\ListHealthChecksResponse' {nextMarker} -> nextMarker) (\s@ListHealthChecksResponse' {} a -> s {nextMarker = a} :: ListHealthChecksResponse)

-- | The response's http status code.
listHealthChecksResponse_httpStatus :: Lens.Lens' ListHealthChecksResponse Core.Int
listHealthChecksResponse_httpStatus = Lens.lens (\ListHealthChecksResponse' {httpStatus} -> httpStatus) (\s@ListHealthChecksResponse' {} a -> s {httpStatus = a} :: ListHealthChecksResponse)

-- | A complex type that contains one @HealthCheck@ element for each health
-- check that is associated with the current AWS account.
listHealthChecksResponse_healthChecks :: Lens.Lens' ListHealthChecksResponse [HealthCheck]
listHealthChecksResponse_healthChecks = Lens.lens (\ListHealthChecksResponse' {healthChecks} -> healthChecks) (\s@ListHealthChecksResponse' {} a -> s {healthChecks = a} :: ListHealthChecksResponse) Core.. Lens._Coerce

-- | For the second and subsequent calls to @ListHealthChecks@, @Marker@ is
-- the value that you specified for the @marker@ parameter in the previous
-- request.
listHealthChecksResponse_marker :: Lens.Lens' ListHealthChecksResponse Core.Text
listHealthChecksResponse_marker = Lens.lens (\ListHealthChecksResponse' {marker} -> marker) (\s@ListHealthChecksResponse' {} a -> s {marker = a} :: ListHealthChecksResponse)

-- | A flag that indicates whether there are more health checks to be listed.
-- If the response was truncated, you can get the next group of health
-- checks by submitting another @ListHealthChecks@ request and specifying
-- the value of @NextMarker@ in the @marker@ parameter.
listHealthChecksResponse_isTruncated :: Lens.Lens' ListHealthChecksResponse Core.Bool
listHealthChecksResponse_isTruncated = Lens.lens (\ListHealthChecksResponse' {isTruncated} -> isTruncated) (\s@ListHealthChecksResponse' {} a -> s {isTruncated = a} :: ListHealthChecksResponse)

-- | The value that you specified for the @maxitems@ parameter in the call to
-- @ListHealthChecks@ that produced the current response.
listHealthChecksResponse_maxItems :: Lens.Lens' ListHealthChecksResponse Core.Text
listHealthChecksResponse_maxItems = Lens.lens (\ListHealthChecksResponse' {maxItems} -> maxItems) (\s@ListHealthChecksResponse' {} a -> s {maxItems = a} :: ListHealthChecksResponse)

instance Core.NFData ListHealthChecksResponse
