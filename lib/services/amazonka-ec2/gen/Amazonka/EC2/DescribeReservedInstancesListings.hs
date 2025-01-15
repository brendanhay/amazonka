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
-- Module      : Amazonka.EC2.DescribeReservedInstancesListings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes your account\'s Reserved Instance listings in the Reserved
-- Instance Marketplace.
--
-- The Reserved Instance Marketplace matches sellers who want to resell
-- Reserved Instance capacity that they no longer need with buyers who want
-- to purchase additional capacity. Reserved Instances bought and sold
-- through the Reserved Instance Marketplace work like any other Reserved
-- Instances.
--
-- As a seller, you choose to list some or all of your Reserved Instances,
-- and you specify the upfront price to receive for them. Your Reserved
-- Instances are then listed in the Reserved Instance Marketplace and are
-- available for purchase.
--
-- As a buyer, you specify the configuration of the Reserved Instance to
-- purchase, and the Marketplace matches what you\'re searching for with
-- what\'s available. The Marketplace first sells the lowest priced
-- Reserved Instances to you, and continues to sell available Reserved
-- Instance listings to you until your demand is met. You are charged based
-- on the total price of all of the listings that you purchase.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ri-market-general.html Reserved Instance Marketplace>
-- in the /Amazon EC2 User Guide/.
module Amazonka.EC2.DescribeReservedInstancesListings
  ( -- * Creating a Request
    DescribeReservedInstancesListings (..),
    newDescribeReservedInstancesListings,

    -- * Request Lenses
    describeReservedInstancesListings_filters,
    describeReservedInstancesListings_reservedInstancesId,
    describeReservedInstancesListings_reservedInstancesListingId,

    -- * Destructuring the Response
    DescribeReservedInstancesListingsResponse (..),
    newDescribeReservedInstancesListingsResponse,

    -- * Response Lenses
    describeReservedInstancesListingsResponse_reservedInstancesListings,
    describeReservedInstancesListingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for DescribeReservedInstancesListings.
--
-- /See:/ 'newDescribeReservedInstancesListings' smart constructor.
data DescribeReservedInstancesListings = DescribeReservedInstancesListings'
  { -- | One or more filters.
    --
    -- -   @reserved-instances-id@ - The ID of the Reserved Instances.
    --
    -- -   @reserved-instances-listing-id@ - The ID of the Reserved Instances
    --     listing.
    --
    -- -   @status@ - The status of the Reserved Instance listing (@pending@ |
    --     @active@ | @cancelled@ | @closed@).
    --
    -- -   @status-message@ - The reason for the status.
    filters :: Prelude.Maybe [Filter],
    -- | One or more Reserved Instance IDs.
    reservedInstancesId :: Prelude.Maybe Prelude.Text,
    -- | One or more Reserved Instance listing IDs.
    reservedInstancesListingId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeReservedInstancesListings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'describeReservedInstancesListings_filters' - One or more filters.
--
-- -   @reserved-instances-id@ - The ID of the Reserved Instances.
--
-- -   @reserved-instances-listing-id@ - The ID of the Reserved Instances
--     listing.
--
-- -   @status@ - The status of the Reserved Instance listing (@pending@ |
--     @active@ | @cancelled@ | @closed@).
--
-- -   @status-message@ - The reason for the status.
--
-- 'reservedInstancesId', 'describeReservedInstancesListings_reservedInstancesId' - One or more Reserved Instance IDs.
--
-- 'reservedInstancesListingId', 'describeReservedInstancesListings_reservedInstancesListingId' - One or more Reserved Instance listing IDs.
newDescribeReservedInstancesListings ::
  DescribeReservedInstancesListings
newDescribeReservedInstancesListings =
  DescribeReservedInstancesListings'
    { filters =
        Prelude.Nothing,
      reservedInstancesId = Prelude.Nothing,
      reservedInstancesListingId =
        Prelude.Nothing
    }

-- | One or more filters.
--
-- -   @reserved-instances-id@ - The ID of the Reserved Instances.
--
-- -   @reserved-instances-listing-id@ - The ID of the Reserved Instances
--     listing.
--
-- -   @status@ - The status of the Reserved Instance listing (@pending@ |
--     @active@ | @cancelled@ | @closed@).
--
-- -   @status-message@ - The reason for the status.
describeReservedInstancesListings_filters :: Lens.Lens' DescribeReservedInstancesListings (Prelude.Maybe [Filter])
describeReservedInstancesListings_filters = Lens.lens (\DescribeReservedInstancesListings' {filters} -> filters) (\s@DescribeReservedInstancesListings' {} a -> s {filters = a} :: DescribeReservedInstancesListings) Prelude.. Lens.mapping Lens.coerced

-- | One or more Reserved Instance IDs.
describeReservedInstancesListings_reservedInstancesId :: Lens.Lens' DescribeReservedInstancesListings (Prelude.Maybe Prelude.Text)
describeReservedInstancesListings_reservedInstancesId = Lens.lens (\DescribeReservedInstancesListings' {reservedInstancesId} -> reservedInstancesId) (\s@DescribeReservedInstancesListings' {} a -> s {reservedInstancesId = a} :: DescribeReservedInstancesListings)

-- | One or more Reserved Instance listing IDs.
describeReservedInstancesListings_reservedInstancesListingId :: Lens.Lens' DescribeReservedInstancesListings (Prelude.Maybe Prelude.Text)
describeReservedInstancesListings_reservedInstancesListingId = Lens.lens (\DescribeReservedInstancesListings' {reservedInstancesListingId} -> reservedInstancesListingId) (\s@DescribeReservedInstancesListings' {} a -> s {reservedInstancesListingId = a} :: DescribeReservedInstancesListings)

instance
  Core.AWSRequest
    DescribeReservedInstancesListings
  where
  type
    AWSResponse DescribeReservedInstancesListings =
      DescribeReservedInstancesListingsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeReservedInstancesListingsResponse'
            Prelude.<$> ( x
                            Data..@? "reservedInstancesListingsSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeReservedInstancesListings
  where
  hashWithSalt
    _salt
    DescribeReservedInstancesListings' {..} =
      _salt
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` reservedInstancesId
        `Prelude.hashWithSalt` reservedInstancesListingId

instance
  Prelude.NFData
    DescribeReservedInstancesListings
  where
  rnf DescribeReservedInstancesListings' {..} =
    Prelude.rnf filters `Prelude.seq`
      Prelude.rnf reservedInstancesId `Prelude.seq`
        Prelude.rnf reservedInstancesListingId

instance
  Data.ToHeaders
    DescribeReservedInstancesListings
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DescribeReservedInstancesListings
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeReservedInstancesListings
  where
  toQuery DescribeReservedInstancesListings' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeReservedInstancesListings" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "ReservedInstancesId" Data.=: reservedInstancesId,
        "ReservedInstancesListingId"
          Data.=: reservedInstancesListingId
      ]

-- | Contains the output of DescribeReservedInstancesListings.
--
-- /See:/ 'newDescribeReservedInstancesListingsResponse' smart constructor.
data DescribeReservedInstancesListingsResponse = DescribeReservedInstancesListingsResponse'
  { -- | Information about the Reserved Instance listing.
    reservedInstancesListings :: Prelude.Maybe [ReservedInstancesListing],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeReservedInstancesListingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reservedInstancesListings', 'describeReservedInstancesListingsResponse_reservedInstancesListings' - Information about the Reserved Instance listing.
--
-- 'httpStatus', 'describeReservedInstancesListingsResponse_httpStatus' - The response's http status code.
newDescribeReservedInstancesListingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeReservedInstancesListingsResponse
newDescribeReservedInstancesListingsResponse
  pHttpStatus_ =
    DescribeReservedInstancesListingsResponse'
      { reservedInstancesListings =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the Reserved Instance listing.
describeReservedInstancesListingsResponse_reservedInstancesListings :: Lens.Lens' DescribeReservedInstancesListingsResponse (Prelude.Maybe [ReservedInstancesListing])
describeReservedInstancesListingsResponse_reservedInstancesListings = Lens.lens (\DescribeReservedInstancesListingsResponse' {reservedInstancesListings} -> reservedInstancesListings) (\s@DescribeReservedInstancesListingsResponse' {} a -> s {reservedInstancesListings = a} :: DescribeReservedInstancesListingsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeReservedInstancesListingsResponse_httpStatus :: Lens.Lens' DescribeReservedInstancesListingsResponse Prelude.Int
describeReservedInstancesListingsResponse_httpStatus = Lens.lens (\DescribeReservedInstancesListingsResponse' {httpStatus} -> httpStatus) (\s@DescribeReservedInstancesListingsResponse' {} a -> s {httpStatus = a} :: DescribeReservedInstancesListingsResponse)

instance
  Prelude.NFData
    DescribeReservedInstancesListingsResponse
  where
  rnf DescribeReservedInstancesListingsResponse' {..} =
    Prelude.rnf reservedInstancesListings `Prelude.seq`
      Prelude.rnf httpStatus
