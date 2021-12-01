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
-- Module      : Amazonka.Route53Domains.ViewBilling
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
module Amazonka.Route53Domains.ViewBilling
  ( -- * Creating a Request
    ViewBilling (..),
    newViewBilling,

    -- * Request Lenses
    viewBilling_start,
    viewBilling_end,
    viewBilling_marker,
    viewBilling_maxItems,

    -- * Destructuring the Response
    ViewBillingResponse (..),
    newViewBillingResponse,

    -- * Response Lenses
    viewBillingResponse_nextPageMarker,
    viewBillingResponse_billingRecords,
    viewBillingResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Domains.Types

-- | The ViewBilling request includes the following elements.
--
-- /See:/ 'newViewBilling' smart constructor.
data ViewBilling = ViewBilling'
  { -- | The beginning date and time for the time period for which you want a
    -- list of billing records. Specify the date and time in Unix time format
    -- and Coordinated Universal time (UTC).
    start :: Prelude.Maybe Core.POSIX,
    -- | The end date and time for the time period for which you want a list of
    -- billing records. Specify the date and time in Unix time format and
    -- Coordinated Universal time (UTC).
    end :: Prelude.Maybe Core.POSIX,
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
    marker :: Prelude.Maybe Prelude.Text,
    -- | The number of billing records to be returned.
    --
    -- Default: 20
    maxItems :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ViewBilling' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'start', 'viewBilling_start' - The beginning date and time for the time period for which you want a
-- list of billing records. Specify the date and time in Unix time format
-- and Coordinated Universal time (UTC).
--
-- 'end', 'viewBilling_end' - The end date and time for the time period for which you want a list of
-- billing records. Specify the date and time in Unix time format and
-- Coordinated Universal time (UTC).
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
--
-- 'maxItems', 'viewBilling_maxItems' - The number of billing records to be returned.
--
-- Default: 20
newViewBilling ::
  ViewBilling
newViewBilling =
  ViewBilling'
    { start = Prelude.Nothing,
      end = Prelude.Nothing,
      marker = Prelude.Nothing,
      maxItems = Prelude.Nothing
    }

-- | The beginning date and time for the time period for which you want a
-- list of billing records. Specify the date and time in Unix time format
-- and Coordinated Universal time (UTC).
viewBilling_start :: Lens.Lens' ViewBilling (Prelude.Maybe Prelude.UTCTime)
viewBilling_start = Lens.lens (\ViewBilling' {start} -> start) (\s@ViewBilling' {} a -> s {start = a} :: ViewBilling) Prelude.. Lens.mapping Core._Time

-- | The end date and time for the time period for which you want a list of
-- billing records. Specify the date and time in Unix time format and
-- Coordinated Universal time (UTC).
viewBilling_end :: Lens.Lens' ViewBilling (Prelude.Maybe Prelude.UTCTime)
viewBilling_end = Lens.lens (\ViewBilling' {end} -> end) (\s@ViewBilling' {} a -> s {end = a} :: ViewBilling) Prelude.. Lens.mapping Core._Time

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
viewBilling_marker :: Lens.Lens' ViewBilling (Prelude.Maybe Prelude.Text)
viewBilling_marker = Lens.lens (\ViewBilling' {marker} -> marker) (\s@ViewBilling' {} a -> s {marker = a} :: ViewBilling)

-- | The number of billing records to be returned.
--
-- Default: 20
viewBilling_maxItems :: Lens.Lens' ViewBilling (Prelude.Maybe Prelude.Int)
viewBilling_maxItems = Lens.lens (\ViewBilling' {maxItems} -> maxItems) (\s@ViewBilling' {} a -> s {maxItems = a} :: ViewBilling)

instance Core.AWSPager ViewBilling where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? viewBillingResponse_nextPageMarker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? viewBillingResponse_billingRecords
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& viewBilling_marker
          Lens..~ rs
          Lens.^? viewBillingResponse_nextPageMarker
            Prelude.. Lens._Just

instance Core.AWSRequest ViewBilling where
  type AWSResponse ViewBilling = ViewBillingResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ViewBillingResponse'
            Prelude.<$> (x Core..?> "NextPageMarker")
            Prelude.<*> (x Core..?> "BillingRecords" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ViewBilling where
  hashWithSalt salt' ViewBilling' {..} =
    salt' `Prelude.hashWithSalt` maxItems
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` end
      `Prelude.hashWithSalt` start

instance Prelude.NFData ViewBilling where
  rnf ViewBilling' {..} =
    Prelude.rnf start
      `Prelude.seq` Prelude.rnf maxItems
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf end

instance Core.ToHeaders ViewBilling where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Route53Domains_v20140515.ViewBilling" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ViewBilling where
  toJSON ViewBilling' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Start" Core..=) Prelude.<$> start,
            ("End" Core..=) Prelude.<$> end,
            ("Marker" Core..=) Prelude.<$> marker,
            ("MaxItems" Core..=) Prelude.<$> maxItems
          ]
      )

instance Core.ToPath ViewBilling where
  toPath = Prelude.const "/"

instance Core.ToQuery ViewBilling where
  toQuery = Prelude.const Prelude.mempty

-- | The ViewBilling response includes the following elements.
--
-- /See:/ 'newViewBillingResponse' smart constructor.
data ViewBillingResponse = ViewBillingResponse'
  { -- | If there are more billing records than you specified for @MaxItems@ in
    -- the request, submit another request and include the value of
    -- @NextPageMarker@ in the value of @Marker@.
    nextPageMarker :: Prelude.Maybe Prelude.Text,
    -- | A summary of billing records.
    billingRecords :: Prelude.Maybe [BillingRecord],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ViewBillingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageMarker', 'viewBillingResponse_nextPageMarker' - If there are more billing records than you specified for @MaxItems@ in
-- the request, submit another request and include the value of
-- @NextPageMarker@ in the value of @Marker@.
--
-- 'billingRecords', 'viewBillingResponse_billingRecords' - A summary of billing records.
--
-- 'httpStatus', 'viewBillingResponse_httpStatus' - The response's http status code.
newViewBillingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ViewBillingResponse
newViewBillingResponse pHttpStatus_ =
  ViewBillingResponse'
    { nextPageMarker =
        Prelude.Nothing,
      billingRecords = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are more billing records than you specified for @MaxItems@ in
-- the request, submit another request and include the value of
-- @NextPageMarker@ in the value of @Marker@.
viewBillingResponse_nextPageMarker :: Lens.Lens' ViewBillingResponse (Prelude.Maybe Prelude.Text)
viewBillingResponse_nextPageMarker = Lens.lens (\ViewBillingResponse' {nextPageMarker} -> nextPageMarker) (\s@ViewBillingResponse' {} a -> s {nextPageMarker = a} :: ViewBillingResponse)

-- | A summary of billing records.
viewBillingResponse_billingRecords :: Lens.Lens' ViewBillingResponse (Prelude.Maybe [BillingRecord])
viewBillingResponse_billingRecords = Lens.lens (\ViewBillingResponse' {billingRecords} -> billingRecords) (\s@ViewBillingResponse' {} a -> s {billingRecords = a} :: ViewBillingResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
viewBillingResponse_httpStatus :: Lens.Lens' ViewBillingResponse Prelude.Int
viewBillingResponse_httpStatus = Lens.lens (\ViewBillingResponse' {httpStatus} -> httpStatus) (\s@ViewBillingResponse' {} a -> s {httpStatus = a} :: ViewBillingResponse)

instance Prelude.NFData ViewBillingResponse where
  rnf ViewBillingResponse' {..} =
    Prelude.rnf nextPageMarker
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf billingRecords
