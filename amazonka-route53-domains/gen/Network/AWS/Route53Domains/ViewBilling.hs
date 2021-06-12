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
-- Module      : Network.AWS.Route53Domains.ViewBilling
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all the domain-related billing records for the current AWS
-- account for a specified period
--
-- This operation returns paginated results.
module Network.AWS.Route53Domains.ViewBilling
  ( -- * Creating a Request
    ViewBilling (..),
    newViewBilling,

    -- * Request Lenses
    viewBilling_end,
    viewBilling_start,
    viewBilling_maxItems,
    viewBilling_marker,

    -- * Destructuring the Response
    ViewBillingResponse (..),
    newViewBillingResponse,

    -- * Response Lenses
    viewBillingResponse_billingRecords,
    viewBillingResponse_nextPageMarker,
    viewBillingResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53Domains.Types

-- | The ViewBilling request includes the following elements.
--
-- /See:/ 'newViewBilling' smart constructor.
data ViewBilling = ViewBilling'
  { -- | The end date and time for the time period for which you want a list of
    -- billing records. Specify the date and time in Unix time format and
    -- Coordinated Universal time (UTC).
    end :: Core.Maybe Core.POSIX,
    -- | The beginning date and time for the time period for which you want a
    -- list of billing records. Specify the date and time in Unix time format
    -- and Coordinated Universal time (UTC).
    start :: Core.Maybe Core.POSIX,
    -- | The number of billing records to be returned.
    --
    -- Default: 20
    maxItems :: Core.Maybe Core.Int,
    -- | For an initial request for a list of billing records, omit this element.
    -- If the number of billing records that are associated with the current
    -- AWS account during the specified period is greater than the value that
    -- you specified for @MaxItems@, you can use @Marker@ to return additional
    -- billing records. Get the value of @NextPageMarker@ from the previous
    -- response, and submit another request that includes the value of
    -- @NextPageMarker@ in the @Marker@ element.
    --
    -- Constraints: The marker must match the value of @NextPageMarker@ that
    -- was returned in the previous response.
    marker :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ViewBilling' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'end', 'viewBilling_end' - The end date and time for the time period for which you want a list of
-- billing records. Specify the date and time in Unix time format and
-- Coordinated Universal time (UTC).
--
-- 'start', 'viewBilling_start' - The beginning date and time for the time period for which you want a
-- list of billing records. Specify the date and time in Unix time format
-- and Coordinated Universal time (UTC).
--
-- 'maxItems', 'viewBilling_maxItems' - The number of billing records to be returned.
--
-- Default: 20
--
-- 'marker', 'viewBilling_marker' - For an initial request for a list of billing records, omit this element.
-- If the number of billing records that are associated with the current
-- AWS account during the specified period is greater than the value that
-- you specified for @MaxItems@, you can use @Marker@ to return additional
-- billing records. Get the value of @NextPageMarker@ from the previous
-- response, and submit another request that includes the value of
-- @NextPageMarker@ in the @Marker@ element.
--
-- Constraints: The marker must match the value of @NextPageMarker@ that
-- was returned in the previous response.
newViewBilling ::
  ViewBilling
newViewBilling =
  ViewBilling'
    { end = Core.Nothing,
      start = Core.Nothing,
      maxItems = Core.Nothing,
      marker = Core.Nothing
    }

-- | The end date and time for the time period for which you want a list of
-- billing records. Specify the date and time in Unix time format and
-- Coordinated Universal time (UTC).
viewBilling_end :: Lens.Lens' ViewBilling (Core.Maybe Core.UTCTime)
viewBilling_end = Lens.lens (\ViewBilling' {end} -> end) (\s@ViewBilling' {} a -> s {end = a} :: ViewBilling) Core.. Lens.mapping Core._Time

-- | The beginning date and time for the time period for which you want a
-- list of billing records. Specify the date and time in Unix time format
-- and Coordinated Universal time (UTC).
viewBilling_start :: Lens.Lens' ViewBilling (Core.Maybe Core.UTCTime)
viewBilling_start = Lens.lens (\ViewBilling' {start} -> start) (\s@ViewBilling' {} a -> s {start = a} :: ViewBilling) Core.. Lens.mapping Core._Time

-- | The number of billing records to be returned.
--
-- Default: 20
viewBilling_maxItems :: Lens.Lens' ViewBilling (Core.Maybe Core.Int)
viewBilling_maxItems = Lens.lens (\ViewBilling' {maxItems} -> maxItems) (\s@ViewBilling' {} a -> s {maxItems = a} :: ViewBilling)

-- | For an initial request for a list of billing records, omit this element.
-- If the number of billing records that are associated with the current
-- AWS account during the specified period is greater than the value that
-- you specified for @MaxItems@, you can use @Marker@ to return additional
-- billing records. Get the value of @NextPageMarker@ from the previous
-- response, and submit another request that includes the value of
-- @NextPageMarker@ in the @Marker@ element.
--
-- Constraints: The marker must match the value of @NextPageMarker@ that
-- was returned in the previous response.
viewBilling_marker :: Lens.Lens' ViewBilling (Core.Maybe Core.Text)
viewBilling_marker = Lens.lens (\ViewBilling' {marker} -> marker) (\s@ViewBilling' {} a -> s {marker = a} :: ViewBilling)

instance Core.AWSPager ViewBilling where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? viewBillingResponse_nextPageMarker Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? viewBillingResponse_billingRecords Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& viewBilling_marker
          Lens..~ rs
          Lens.^? viewBillingResponse_nextPageMarker Core.. Lens._Just

instance Core.AWSRequest ViewBilling where
  type AWSResponse ViewBilling = ViewBillingResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ViewBillingResponse'
            Core.<$> (x Core..?> "BillingRecords" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "NextPageMarker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ViewBilling

instance Core.NFData ViewBilling

instance Core.ToHeaders ViewBilling where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Route53Domains_v20140515.ViewBilling" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ViewBilling where
  toJSON ViewBilling' {..} =
    Core.object
      ( Core.catMaybes
          [ ("End" Core..=) Core.<$> end,
            ("Start" Core..=) Core.<$> start,
            ("MaxItems" Core..=) Core.<$> maxItems,
            ("Marker" Core..=) Core.<$> marker
          ]
      )

instance Core.ToPath ViewBilling where
  toPath = Core.const "/"

instance Core.ToQuery ViewBilling where
  toQuery = Core.const Core.mempty

-- | The ViewBilling response includes the following elements.
--
-- /See:/ 'newViewBillingResponse' smart constructor.
data ViewBillingResponse = ViewBillingResponse'
  { -- | A summary of billing records.
    billingRecords :: Core.Maybe [BillingRecord],
    -- | If there are more billing records than you specified for @MaxItems@ in
    -- the request, submit another request and include the value of
    -- @NextPageMarker@ in the value of @Marker@.
    nextPageMarker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ViewBillingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'billingRecords', 'viewBillingResponse_billingRecords' - A summary of billing records.
--
-- 'nextPageMarker', 'viewBillingResponse_nextPageMarker' - If there are more billing records than you specified for @MaxItems@ in
-- the request, submit another request and include the value of
-- @NextPageMarker@ in the value of @Marker@.
--
-- 'httpStatus', 'viewBillingResponse_httpStatus' - The response's http status code.
newViewBillingResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ViewBillingResponse
newViewBillingResponse pHttpStatus_ =
  ViewBillingResponse'
    { billingRecords = Core.Nothing,
      nextPageMarker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A summary of billing records.
viewBillingResponse_billingRecords :: Lens.Lens' ViewBillingResponse (Core.Maybe [BillingRecord])
viewBillingResponse_billingRecords = Lens.lens (\ViewBillingResponse' {billingRecords} -> billingRecords) (\s@ViewBillingResponse' {} a -> s {billingRecords = a} :: ViewBillingResponse) Core.. Lens.mapping Lens._Coerce

-- | If there are more billing records than you specified for @MaxItems@ in
-- the request, submit another request and include the value of
-- @NextPageMarker@ in the value of @Marker@.
viewBillingResponse_nextPageMarker :: Lens.Lens' ViewBillingResponse (Core.Maybe Core.Text)
viewBillingResponse_nextPageMarker = Lens.lens (\ViewBillingResponse' {nextPageMarker} -> nextPageMarker) (\s@ViewBillingResponse' {} a -> s {nextPageMarker = a} :: ViewBillingResponse)

-- | The response's http status code.
viewBillingResponse_httpStatus :: Lens.Lens' ViewBillingResponse Core.Int
viewBillingResponse_httpStatus = Lens.lens (\ViewBillingResponse' {httpStatus} -> httpStatus) (\s@ViewBillingResponse' {} a -> s {httpStatus = a} :: ViewBillingResponse)

instance Core.NFData ViewBillingResponse
