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
-- Module      : Network.AWS.EC2.CancelReservedInstancesListing
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the specified Reserved Instance listing in the Reserved Instance
-- Marketplace.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ri-market-general.html Reserved Instance Marketplace>
-- in the /Amazon EC2 User Guide/.
module Network.AWS.EC2.CancelReservedInstancesListing
  ( -- * Creating a Request
    CancelReservedInstancesListing (..),
    newCancelReservedInstancesListing,

    -- * Request Lenses
    cancelReservedInstancesListing_reservedInstancesListingId,

    -- * Destructuring the Response
    CancelReservedInstancesListingResponse (..),
    newCancelReservedInstancesListingResponse,

    -- * Response Lenses
    cancelReservedInstancesListingResponse_reservedInstancesListings,
    cancelReservedInstancesListingResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for CancelReservedInstancesListing.
--
-- /See:/ 'newCancelReservedInstancesListing' smart constructor.
data CancelReservedInstancesListing = CancelReservedInstancesListing'
  { -- | The ID of the Reserved Instance listing.
    reservedInstancesListingId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelReservedInstancesListing' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reservedInstancesListingId', 'cancelReservedInstancesListing_reservedInstancesListingId' - The ID of the Reserved Instance listing.
newCancelReservedInstancesListing ::
  -- | 'reservedInstancesListingId'
  Prelude.Text ->
  CancelReservedInstancesListing
newCancelReservedInstancesListing
  pReservedInstancesListingId_ =
    CancelReservedInstancesListing'
      { reservedInstancesListingId =
          pReservedInstancesListingId_
      }

-- | The ID of the Reserved Instance listing.
cancelReservedInstancesListing_reservedInstancesListingId :: Lens.Lens' CancelReservedInstancesListing Prelude.Text
cancelReservedInstancesListing_reservedInstancesListingId = Lens.lens (\CancelReservedInstancesListing' {reservedInstancesListingId} -> reservedInstancesListingId) (\s@CancelReservedInstancesListing' {} a -> s {reservedInstancesListingId = a} :: CancelReservedInstancesListing)

instance
  Core.AWSRequest
    CancelReservedInstancesListing
  where
  type
    AWSResponse CancelReservedInstancesListing =
      CancelReservedInstancesListingResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CancelReservedInstancesListingResponse'
            Prelude.<$> ( x Core..@? "reservedInstancesListingsSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CancelReservedInstancesListing

instance
  Prelude.NFData
    CancelReservedInstancesListing

instance
  Core.ToHeaders
    CancelReservedInstancesListing
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CancelReservedInstancesListing where
  toPath = Prelude.const "/"

instance Core.ToQuery CancelReservedInstancesListing where
  toQuery CancelReservedInstancesListing' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "CancelReservedInstancesListing" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "ReservedInstancesListingId"
          Core.=: reservedInstancesListingId
      ]

-- | Contains the output of CancelReservedInstancesListing.
--
-- /See:/ 'newCancelReservedInstancesListingResponse' smart constructor.
data CancelReservedInstancesListingResponse = CancelReservedInstancesListingResponse'
  { -- | The Reserved Instance listing.
    reservedInstancesListings :: Prelude.Maybe [ReservedInstancesListing],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelReservedInstancesListingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reservedInstancesListings', 'cancelReservedInstancesListingResponse_reservedInstancesListings' - The Reserved Instance listing.
--
-- 'httpStatus', 'cancelReservedInstancesListingResponse_httpStatus' - The response's http status code.
newCancelReservedInstancesListingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelReservedInstancesListingResponse
newCancelReservedInstancesListingResponse
  pHttpStatus_ =
    CancelReservedInstancesListingResponse'
      { reservedInstancesListings =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The Reserved Instance listing.
cancelReservedInstancesListingResponse_reservedInstancesListings :: Lens.Lens' CancelReservedInstancesListingResponse (Prelude.Maybe [ReservedInstancesListing])
cancelReservedInstancesListingResponse_reservedInstancesListings = Lens.lens (\CancelReservedInstancesListingResponse' {reservedInstancesListings} -> reservedInstancesListings) (\s@CancelReservedInstancesListingResponse' {} a -> s {reservedInstancesListings = a} :: CancelReservedInstancesListingResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
cancelReservedInstancesListingResponse_httpStatus :: Lens.Lens' CancelReservedInstancesListingResponse Prelude.Int
cancelReservedInstancesListingResponse_httpStatus = Lens.lens (\CancelReservedInstancesListingResponse' {httpStatus} -> httpStatus) (\s@CancelReservedInstancesListingResponse' {} a -> s {httpStatus = a} :: CancelReservedInstancesListingResponse)

instance
  Prelude.NFData
    CancelReservedInstancesListingResponse
