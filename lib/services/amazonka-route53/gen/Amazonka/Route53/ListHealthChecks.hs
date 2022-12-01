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
-- Module      : Amazonka.Route53.ListHealthChecks
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve a list of the health checks that are associated with the
-- current Amazon Web Services account.
--
-- This operation returns paginated results.
module Amazonka.Route53.ListHealthChecks
  ( -- * Creating a Request
    ListHealthChecks (..),
    newListHealthChecks,

    -- * Request Lenses
    listHealthChecks_marker,
    listHealthChecks_maxItems,

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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53.Types

-- | A request to retrieve a list of the health checks that are associated
-- with the current Amazon Web Services account.
--
-- /See:/ 'newListHealthChecks' smart constructor.
data ListHealthChecks = ListHealthChecks'
  { -- | If the value of @IsTruncated@ in the previous response was @true@, you
    -- have more health checks. To get another group, submit another
    -- @ListHealthChecks@ request.
    --
    -- For the value of @marker@, specify the value of @NextMarker@ from the
    -- previous response, which is the ID of the first health check that Amazon
    -- Route 53 will return if you submit another request.
    --
    -- If the value of @IsTruncated@ in the previous response was @false@,
    -- there are no more health checks to get.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of health checks that you want @ListHealthChecks@ to
    -- return in response to the current request. Amazon Route 53 returns a
    -- maximum of 100 items. If you set @MaxItems@ to a value greater than 100,
    -- Route 53 returns only the first 100 health checks.
    maxItems :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListHealthChecks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
--
-- 'maxItems', 'listHealthChecks_maxItems' - The maximum number of health checks that you want @ListHealthChecks@ to
-- return in response to the current request. Amazon Route 53 returns a
-- maximum of 100 items. If you set @MaxItems@ to a value greater than 100,
-- Route 53 returns only the first 100 health checks.
newListHealthChecks ::
  ListHealthChecks
newListHealthChecks =
  ListHealthChecks'
    { marker = Prelude.Nothing,
      maxItems = Prelude.Nothing
    }

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
listHealthChecks_marker :: Lens.Lens' ListHealthChecks (Prelude.Maybe Prelude.Text)
listHealthChecks_marker = Lens.lens (\ListHealthChecks' {marker} -> marker) (\s@ListHealthChecks' {} a -> s {marker = a} :: ListHealthChecks)

-- | The maximum number of health checks that you want @ListHealthChecks@ to
-- return in response to the current request. Amazon Route 53 returns a
-- maximum of 100 items. If you set @MaxItems@ to a value greater than 100,
-- Route 53 returns only the first 100 health checks.
listHealthChecks_maxItems :: Lens.Lens' ListHealthChecks (Prelude.Maybe Prelude.Text)
listHealthChecks_maxItems = Lens.lens (\ListHealthChecks' {maxItems} -> maxItems) (\s@ListHealthChecks' {} a -> s {maxItems = a} :: ListHealthChecks)

instance Core.AWSPager ListHealthChecks where
  page rq rs
    | Core.stop
        (rs Lens.^. listHealthChecksResponse_isTruncated) =
      Prelude.Nothing
    | Prelude.isNothing
        ( rs
            Lens.^? listHealthChecksResponse_nextMarker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listHealthChecks_marker
          Lens..~ rs
          Lens.^? listHealthChecksResponse_nextMarker
            Prelude.. Lens._Just

instance Core.AWSRequest ListHealthChecks where
  type
    AWSResponse ListHealthChecks =
      ListHealthChecksResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ListHealthChecksResponse'
            Prelude.<$> (x Core..@? "NextMarker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..@? "HealthChecks" Core..!@ Prelude.mempty
                            Prelude.>>= Core.parseXMLList "HealthCheck"
                        )
            Prelude.<*> (x Core..@ "Marker")
            Prelude.<*> (x Core..@ "IsTruncated")
            Prelude.<*> (x Core..@ "MaxItems")
      )

instance Prelude.Hashable ListHealthChecks where
  hashWithSalt _salt ListHealthChecks' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxItems

instance Prelude.NFData ListHealthChecks where
  rnf ListHealthChecks' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxItems

instance Core.ToHeaders ListHealthChecks where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListHealthChecks where
  toPath = Prelude.const "/2013-04-01/healthcheck"

instance Core.ToQuery ListHealthChecks where
  toQuery ListHealthChecks' {..} =
    Prelude.mconcat
      [ "marker" Core.=: marker,
        "maxitems" Core.=: maxItems
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
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A complex type that contains one @HealthCheck@ element for each health
    -- check that is associated with the current Amazon Web Services account.
    healthChecks :: [HealthCheck],
    -- | For the second and subsequent calls to @ListHealthChecks@, @Marker@ is
    -- the value that you specified for the @marker@ parameter in the previous
    -- request.
    marker :: Prelude.Text,
    -- | A flag that indicates whether there are more health checks to be listed.
    -- If the response was truncated, you can get the next group of health
    -- checks by submitting another @ListHealthChecks@ request and specifying
    -- the value of @NextMarker@ in the @marker@ parameter.
    isTruncated :: Prelude.Bool,
    -- | The value that you specified for the @maxitems@ parameter in the call to
    -- @ListHealthChecks@ that produced the current response.
    maxItems :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- check that is associated with the current Amazon Web Services account.
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
  Prelude.Int ->
  -- | 'marker'
  Prelude.Text ->
  -- | 'isTruncated'
  Prelude.Bool ->
  -- | 'maxItems'
  Prelude.Text ->
  ListHealthChecksResponse
newListHealthChecksResponse
  pHttpStatus_
  pMarker_
  pIsTruncated_
  pMaxItems_ =
    ListHealthChecksResponse'
      { nextMarker =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        healthChecks = Prelude.mempty,
        marker = pMarker_,
        isTruncated = pIsTruncated_,
        maxItems = pMaxItems_
      }

-- | If @IsTruncated@ is @true@, the value of @NextMarker@ identifies the
-- first health check that Amazon Route 53 returns if you submit another
-- @ListHealthChecks@ request and specify the value of @NextMarker@ in the
-- @marker@ parameter.
listHealthChecksResponse_nextMarker :: Lens.Lens' ListHealthChecksResponse (Prelude.Maybe Prelude.Text)
listHealthChecksResponse_nextMarker = Lens.lens (\ListHealthChecksResponse' {nextMarker} -> nextMarker) (\s@ListHealthChecksResponse' {} a -> s {nextMarker = a} :: ListHealthChecksResponse)

-- | The response's http status code.
listHealthChecksResponse_httpStatus :: Lens.Lens' ListHealthChecksResponse Prelude.Int
listHealthChecksResponse_httpStatus = Lens.lens (\ListHealthChecksResponse' {httpStatus} -> httpStatus) (\s@ListHealthChecksResponse' {} a -> s {httpStatus = a} :: ListHealthChecksResponse)

-- | A complex type that contains one @HealthCheck@ element for each health
-- check that is associated with the current Amazon Web Services account.
listHealthChecksResponse_healthChecks :: Lens.Lens' ListHealthChecksResponse [HealthCheck]
listHealthChecksResponse_healthChecks = Lens.lens (\ListHealthChecksResponse' {healthChecks} -> healthChecks) (\s@ListHealthChecksResponse' {} a -> s {healthChecks = a} :: ListHealthChecksResponse) Prelude.. Lens.coerced

-- | For the second and subsequent calls to @ListHealthChecks@, @Marker@ is
-- the value that you specified for the @marker@ parameter in the previous
-- request.
listHealthChecksResponse_marker :: Lens.Lens' ListHealthChecksResponse Prelude.Text
listHealthChecksResponse_marker = Lens.lens (\ListHealthChecksResponse' {marker} -> marker) (\s@ListHealthChecksResponse' {} a -> s {marker = a} :: ListHealthChecksResponse)

-- | A flag that indicates whether there are more health checks to be listed.
-- If the response was truncated, you can get the next group of health
-- checks by submitting another @ListHealthChecks@ request and specifying
-- the value of @NextMarker@ in the @marker@ parameter.
listHealthChecksResponse_isTruncated :: Lens.Lens' ListHealthChecksResponse Prelude.Bool
listHealthChecksResponse_isTruncated = Lens.lens (\ListHealthChecksResponse' {isTruncated} -> isTruncated) (\s@ListHealthChecksResponse' {} a -> s {isTruncated = a} :: ListHealthChecksResponse)

-- | The value that you specified for the @maxitems@ parameter in the call to
-- @ListHealthChecks@ that produced the current response.
listHealthChecksResponse_maxItems :: Lens.Lens' ListHealthChecksResponse Prelude.Text
listHealthChecksResponse_maxItems = Lens.lens (\ListHealthChecksResponse' {maxItems} -> maxItems) (\s@ListHealthChecksResponse' {} a -> s {maxItems = a} :: ListHealthChecksResponse)

instance Prelude.NFData ListHealthChecksResponse where
  rnf ListHealthChecksResponse' {..} =
    Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf healthChecks
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf isTruncated
      `Prelude.seq` Prelude.rnf maxItems
