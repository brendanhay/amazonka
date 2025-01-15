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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all the domain-related billing records for the current Amazon
-- Web Services account for a specified period
--
-- This operation returns paginated results.
module Amazonka.Route53Domains.ViewBilling
  ( -- * Creating a Request
    ViewBilling (..),
    newViewBilling,

    -- * Request Lenses
    viewBilling_end,
    viewBilling_marker,
    viewBilling_maxItems,
    viewBilling_start,

    -- * Destructuring the Response
    ViewBillingResponse (..),
    newViewBillingResponse,

    -- * Response Lenses
    viewBillingResponse_billingRecords,
    viewBillingResponse_nextPageMarker,
    viewBillingResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Domains.Types

-- | The ViewBilling request includes the following elements.
--
-- /See:/ 'newViewBilling' smart constructor.
data ViewBilling = ViewBilling'
  { -- | The end date and time for the time period for which you want a list of
    -- billing records. Specify the date and time in Unix time format and
    -- Coordinated Universal time (UTC).
    end :: Prelude.Maybe Data.POSIX,
    -- | For an initial request for a list of billing records, omit this element.
    -- If the number of billing records that are associated with the current
    -- Amazon Web Services account during the specified period is greater than
    -- the value that you specified for @MaxItems@, you can use @Marker@ to
    -- return additional billing records. Get the value of @NextPageMarker@
    -- from the previous response, and submit another request that includes the
    -- value of @NextPageMarker@ in the @Marker@ element.
    --
    -- Constraints: The marker must match the value of @NextPageMarker@ that
    -- was returned in the previous response.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The number of billing records to be returned.
    --
    -- Default: 20
    maxItems :: Prelude.Maybe Prelude.Int,
    -- | The beginning date and time for the time period for which you want a
    -- list of billing records. Specify the date and time in Unix time format
    -- and Coordinated Universal time (UTC).
    start :: Prelude.Maybe Data.POSIX
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
-- 'end', 'viewBilling_end' - The end date and time for the time period for which you want a list of
-- billing records. Specify the date and time in Unix time format and
-- Coordinated Universal time (UTC).
--
-- 'marker', 'viewBilling_marker' - For an initial request for a list of billing records, omit this element.
-- If the number of billing records that are associated with the current
-- Amazon Web Services account during the specified period is greater than
-- the value that you specified for @MaxItems@, you can use @Marker@ to
-- return additional billing records. Get the value of @NextPageMarker@
-- from the previous response, and submit another request that includes the
-- value of @NextPageMarker@ in the @Marker@ element.
--
-- Constraints: The marker must match the value of @NextPageMarker@ that
-- was returned in the previous response.
--
-- 'maxItems', 'viewBilling_maxItems' - The number of billing records to be returned.
--
-- Default: 20
--
-- 'start', 'viewBilling_start' - The beginning date and time for the time period for which you want a
-- list of billing records. Specify the date and time in Unix time format
-- and Coordinated Universal time (UTC).
newViewBilling ::
  ViewBilling
newViewBilling =
  ViewBilling'
    { end = Prelude.Nothing,
      marker = Prelude.Nothing,
      maxItems = Prelude.Nothing,
      start = Prelude.Nothing
    }

-- | The end date and time for the time period for which you want a list of
-- billing records. Specify the date and time in Unix time format and
-- Coordinated Universal time (UTC).
viewBilling_end :: Lens.Lens' ViewBilling (Prelude.Maybe Prelude.UTCTime)
viewBilling_end = Lens.lens (\ViewBilling' {end} -> end) (\s@ViewBilling' {} a -> s {end = a} :: ViewBilling) Prelude.. Lens.mapping Data._Time

-- | For an initial request for a list of billing records, omit this element.
-- If the number of billing records that are associated with the current
-- Amazon Web Services account during the specified period is greater than
-- the value that you specified for @MaxItems@, you can use @Marker@ to
-- return additional billing records. Get the value of @NextPageMarker@
-- from the previous response, and submit another request that includes the
-- value of @NextPageMarker@ in the @Marker@ element.
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

-- | The beginning date and time for the time period for which you want a
-- list of billing records. Specify the date and time in Unix time format
-- and Coordinated Universal time (UTC).
viewBilling_start :: Lens.Lens' ViewBilling (Prelude.Maybe Prelude.UTCTime)
viewBilling_start = Lens.lens (\ViewBilling' {start} -> start) (\s@ViewBilling' {} a -> s {start = a} :: ViewBilling) Prelude.. Lens.mapping Data._Time

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ViewBillingResponse'
            Prelude.<$> (x Data..?> "BillingRecords" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextPageMarker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ViewBilling where
  hashWithSalt _salt ViewBilling' {..} =
    _salt
      `Prelude.hashWithSalt` end
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxItems
      `Prelude.hashWithSalt` start

instance Prelude.NFData ViewBilling where
  rnf ViewBilling' {..} =
    Prelude.rnf end `Prelude.seq`
      Prelude.rnf marker `Prelude.seq`
        Prelude.rnf maxItems `Prelude.seq`
          Prelude.rnf start

instance Data.ToHeaders ViewBilling where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Domains_v20140515.ViewBilling" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ViewBilling where
  toJSON ViewBilling' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("End" Data..=) Prelude.<$> end,
            ("Marker" Data..=) Prelude.<$> marker,
            ("MaxItems" Data..=) Prelude.<$> maxItems,
            ("Start" Data..=) Prelude.<$> start
          ]
      )

instance Data.ToPath ViewBilling where
  toPath = Prelude.const "/"

instance Data.ToQuery ViewBilling where
  toQuery = Prelude.const Prelude.mempty

-- | The ViewBilling response includes the following elements.
--
-- /See:/ 'newViewBillingResponse' smart constructor.
data ViewBillingResponse = ViewBillingResponse'
  { -- | A summary of billing records.
    billingRecords :: Prelude.Maybe [BillingRecord],
    -- | If there are more billing records than you specified for @MaxItems@ in
    -- the request, submit another request and include the value of
    -- @NextPageMarker@ in the value of @Marker@.
    nextPageMarker :: Prelude.Maybe Prelude.Text,
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
-- 'billingRecords', 'viewBillingResponse_billingRecords' - A summary of billing records.
--
-- 'nextPageMarker', 'viewBillingResponse_nextPageMarker' - If there are more billing records than you specified for @MaxItems@ in
-- the request, submit another request and include the value of
-- @NextPageMarker@ in the value of @Marker@.
--
-- 'httpStatus', 'viewBillingResponse_httpStatus' - The response's http status code.
newViewBillingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ViewBillingResponse
newViewBillingResponse pHttpStatus_ =
  ViewBillingResponse'
    { billingRecords =
        Prelude.Nothing,
      nextPageMarker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A summary of billing records.
viewBillingResponse_billingRecords :: Lens.Lens' ViewBillingResponse (Prelude.Maybe [BillingRecord])
viewBillingResponse_billingRecords = Lens.lens (\ViewBillingResponse' {billingRecords} -> billingRecords) (\s@ViewBillingResponse' {} a -> s {billingRecords = a} :: ViewBillingResponse) Prelude.. Lens.mapping Lens.coerced

-- | If there are more billing records than you specified for @MaxItems@ in
-- the request, submit another request and include the value of
-- @NextPageMarker@ in the value of @Marker@.
viewBillingResponse_nextPageMarker :: Lens.Lens' ViewBillingResponse (Prelude.Maybe Prelude.Text)
viewBillingResponse_nextPageMarker = Lens.lens (\ViewBillingResponse' {nextPageMarker} -> nextPageMarker) (\s@ViewBillingResponse' {} a -> s {nextPageMarker = a} :: ViewBillingResponse)

-- | The response's http status code.
viewBillingResponse_httpStatus :: Lens.Lens' ViewBillingResponse Prelude.Int
viewBillingResponse_httpStatus = Lens.lens (\ViewBillingResponse' {httpStatus} -> httpStatus) (\s@ViewBillingResponse' {} a -> s {httpStatus = a} :: ViewBillingResponse)

instance Prelude.NFData ViewBillingResponse where
  rnf ViewBillingResponse' {..} =
    Prelude.rnf billingRecords `Prelude.seq`
      Prelude.rnf nextPageMarker `Prelude.seq`
        Prelude.rnf httpStatus
