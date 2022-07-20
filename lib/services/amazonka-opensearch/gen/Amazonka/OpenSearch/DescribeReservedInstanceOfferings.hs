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
-- Module      : Amazonka.OpenSearch.DescribeReservedInstanceOfferings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists available reserved OpenSearch instance offerings.
module Amazonka.OpenSearch.DescribeReservedInstanceOfferings
  ( -- * Creating a Request
    DescribeReservedInstanceOfferings (..),
    newDescribeReservedInstanceOfferings,

    -- * Request Lenses
    describeReservedInstanceOfferings_nextToken,
    describeReservedInstanceOfferings_maxResults,
    describeReservedInstanceOfferings_reservedInstanceOfferingId,

    -- * Destructuring the Response
    DescribeReservedInstanceOfferingsResponse (..),
    newDescribeReservedInstanceOfferingsResponse,

    -- * Response Lenses
    describeReservedInstanceOfferingsResponse_nextToken,
    describeReservedInstanceOfferingsResponse_reservedInstanceOfferings,
    describeReservedInstanceOfferingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.OpenSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for parameters to @DescribeReservedInstanceOfferings@
--
-- /See:/ 'newDescribeReservedInstanceOfferings' smart constructor.
data DescribeReservedInstanceOfferings = DescribeReservedInstanceOfferings'
  { -- | Provides an identifier to allow retrieval of paginated results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Set this value to limit the number of results returned. If not
    -- specified, defaults to 100.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The offering identifier filter value. Use this parameter to show only
    -- the available offering that matches the specified reservation
    -- identifier.
    reservedInstanceOfferingId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeReservedInstanceOfferings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeReservedInstanceOfferings_nextToken' - Provides an identifier to allow retrieval of paginated results.
--
-- 'maxResults', 'describeReservedInstanceOfferings_maxResults' - Set this value to limit the number of results returned. If not
-- specified, defaults to 100.
--
-- 'reservedInstanceOfferingId', 'describeReservedInstanceOfferings_reservedInstanceOfferingId' - The offering identifier filter value. Use this parameter to show only
-- the available offering that matches the specified reservation
-- identifier.
newDescribeReservedInstanceOfferings ::
  DescribeReservedInstanceOfferings
newDescribeReservedInstanceOfferings =
  DescribeReservedInstanceOfferings'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      reservedInstanceOfferingId =
        Prelude.Nothing
    }

-- | Provides an identifier to allow retrieval of paginated results.
describeReservedInstanceOfferings_nextToken :: Lens.Lens' DescribeReservedInstanceOfferings (Prelude.Maybe Prelude.Text)
describeReservedInstanceOfferings_nextToken = Lens.lens (\DescribeReservedInstanceOfferings' {nextToken} -> nextToken) (\s@DescribeReservedInstanceOfferings' {} a -> s {nextToken = a} :: DescribeReservedInstanceOfferings)

-- | Set this value to limit the number of results returned. If not
-- specified, defaults to 100.
describeReservedInstanceOfferings_maxResults :: Lens.Lens' DescribeReservedInstanceOfferings (Prelude.Maybe Prelude.Int)
describeReservedInstanceOfferings_maxResults = Lens.lens (\DescribeReservedInstanceOfferings' {maxResults} -> maxResults) (\s@DescribeReservedInstanceOfferings' {} a -> s {maxResults = a} :: DescribeReservedInstanceOfferings)

-- | The offering identifier filter value. Use this parameter to show only
-- the available offering that matches the specified reservation
-- identifier.
describeReservedInstanceOfferings_reservedInstanceOfferingId :: Lens.Lens' DescribeReservedInstanceOfferings (Prelude.Maybe Prelude.Text)
describeReservedInstanceOfferings_reservedInstanceOfferingId = Lens.lens (\DescribeReservedInstanceOfferings' {reservedInstanceOfferingId} -> reservedInstanceOfferingId) (\s@DescribeReservedInstanceOfferings' {} a -> s {reservedInstanceOfferingId = a} :: DescribeReservedInstanceOfferings)

instance
  Core.AWSRequest
    DescribeReservedInstanceOfferings
  where
  type
    AWSResponse DescribeReservedInstanceOfferings =
      DescribeReservedInstanceOfferingsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeReservedInstanceOfferingsResponse'
            Prelude.<$> (x Core..?> "NextToken")
              Prelude.<*> ( x Core..?> "ReservedInstanceOfferings"
                              Core..!@ Prelude.mempty
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeReservedInstanceOfferings
  where
  hashWithSalt
    _salt
    DescribeReservedInstanceOfferings' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` reservedInstanceOfferingId

instance
  Prelude.NFData
    DescribeReservedInstanceOfferings
  where
  rnf DescribeReservedInstanceOfferings' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf reservedInstanceOfferingId

instance
  Core.ToHeaders
    DescribeReservedInstanceOfferings
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    DescribeReservedInstanceOfferings
  where
  toPath =
    Prelude.const
      "/2021-01-01/opensearch/reservedInstanceOfferings"

instance
  Core.ToQuery
    DescribeReservedInstanceOfferings
  where
  toQuery DescribeReservedInstanceOfferings' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults,
        "offeringId" Core.=: reservedInstanceOfferingId
      ]

-- | Container for results from @DescribeReservedInstanceOfferings@
--
-- /See:/ 'newDescribeReservedInstanceOfferingsResponse' smart constructor.
data DescribeReservedInstanceOfferingsResponse = DescribeReservedInstanceOfferingsResponse'
  { -- | Provides an identifier to allow retrieval of paginated results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | List of reserved OpenSearch instance offerings
    reservedInstanceOfferings :: Prelude.Maybe [ReservedInstanceOffering],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeReservedInstanceOfferingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeReservedInstanceOfferingsResponse_nextToken' - Provides an identifier to allow retrieval of paginated results.
--
-- 'reservedInstanceOfferings', 'describeReservedInstanceOfferingsResponse_reservedInstanceOfferings' - List of reserved OpenSearch instance offerings
--
-- 'httpStatus', 'describeReservedInstanceOfferingsResponse_httpStatus' - The response's http status code.
newDescribeReservedInstanceOfferingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeReservedInstanceOfferingsResponse
newDescribeReservedInstanceOfferingsResponse
  pHttpStatus_ =
    DescribeReservedInstanceOfferingsResponse'
      { nextToken =
          Prelude.Nothing,
        reservedInstanceOfferings =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Provides an identifier to allow retrieval of paginated results.
describeReservedInstanceOfferingsResponse_nextToken :: Lens.Lens' DescribeReservedInstanceOfferingsResponse (Prelude.Maybe Prelude.Text)
describeReservedInstanceOfferingsResponse_nextToken = Lens.lens (\DescribeReservedInstanceOfferingsResponse' {nextToken} -> nextToken) (\s@DescribeReservedInstanceOfferingsResponse' {} a -> s {nextToken = a} :: DescribeReservedInstanceOfferingsResponse)

-- | List of reserved OpenSearch instance offerings
describeReservedInstanceOfferingsResponse_reservedInstanceOfferings :: Lens.Lens' DescribeReservedInstanceOfferingsResponse (Prelude.Maybe [ReservedInstanceOffering])
describeReservedInstanceOfferingsResponse_reservedInstanceOfferings = Lens.lens (\DescribeReservedInstanceOfferingsResponse' {reservedInstanceOfferings} -> reservedInstanceOfferings) (\s@DescribeReservedInstanceOfferingsResponse' {} a -> s {reservedInstanceOfferings = a} :: DescribeReservedInstanceOfferingsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeReservedInstanceOfferingsResponse_httpStatus :: Lens.Lens' DescribeReservedInstanceOfferingsResponse Prelude.Int
describeReservedInstanceOfferingsResponse_httpStatus = Lens.lens (\DescribeReservedInstanceOfferingsResponse' {httpStatus} -> httpStatus) (\s@DescribeReservedInstanceOfferingsResponse' {} a -> s {httpStatus = a} :: DescribeReservedInstanceOfferingsResponse)

instance
  Prelude.NFData
    DescribeReservedInstanceOfferingsResponse
  where
  rnf DescribeReservedInstanceOfferingsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf reservedInstanceOfferings
      `Prelude.seq` Prelude.rnf httpStatus
