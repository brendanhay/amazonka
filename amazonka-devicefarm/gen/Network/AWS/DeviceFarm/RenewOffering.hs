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
-- Module      : Network.AWS.DeviceFarm.RenewOffering
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Explicitly sets the quantity of devices to renew for an offering,
-- starting from the @effectiveDate@ of the next period. The API returns a
-- @NotEligible@ error if the user is not permitted to invoke the
-- operation. If you must be able to invoke this operation, contact
-- <mailto:aws-devicefarm-support@amazon.com aws-devicefarm-support\@amazon.com>.
module Network.AWS.DeviceFarm.RenewOffering
  ( -- * Creating a Request
    RenewOffering (..),
    newRenewOffering,

    -- * Request Lenses
    renewOffering_quantity,
    renewOffering_offeringId,

    -- * Destructuring the Response
    RenewOfferingResponse (..),
    newRenewOfferingResponse,

    -- * Response Lenses
    renewOfferingResponse_offeringTransaction,
    renewOfferingResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request that represents an offering renewal.
--
-- /See:/ 'newRenewOffering' smart constructor.
data RenewOffering = RenewOffering'
  { -- | The quantity requested in an offering renewal.
    quantity :: Core.Maybe Core.Int,
    -- | The ID of a request to renew an offering.
    offeringId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RenewOffering' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'quantity', 'renewOffering_quantity' - The quantity requested in an offering renewal.
--
-- 'offeringId', 'renewOffering_offeringId' - The ID of a request to renew an offering.
newRenewOffering ::
  RenewOffering
newRenewOffering =
  RenewOffering'
    { quantity = Core.Nothing,
      offeringId = Core.Nothing
    }

-- | The quantity requested in an offering renewal.
renewOffering_quantity :: Lens.Lens' RenewOffering (Core.Maybe Core.Int)
renewOffering_quantity = Lens.lens (\RenewOffering' {quantity} -> quantity) (\s@RenewOffering' {} a -> s {quantity = a} :: RenewOffering)

-- | The ID of a request to renew an offering.
renewOffering_offeringId :: Lens.Lens' RenewOffering (Core.Maybe Core.Text)
renewOffering_offeringId = Lens.lens (\RenewOffering' {offeringId} -> offeringId) (\s@RenewOffering' {} a -> s {offeringId = a} :: RenewOffering)

instance Core.AWSRequest RenewOffering where
  type
    AWSResponse RenewOffering =
      RenewOfferingResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RenewOfferingResponse'
            Core.<$> (x Core..?> "offeringTransaction")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable RenewOffering

instance Core.NFData RenewOffering

instance Core.ToHeaders RenewOffering where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.RenewOffering" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON RenewOffering where
  toJSON RenewOffering' {..} =
    Core.object
      ( Core.catMaybes
          [ ("quantity" Core..=) Core.<$> quantity,
            ("offeringId" Core..=) Core.<$> offeringId
          ]
      )

instance Core.ToPath RenewOffering where
  toPath = Core.const "/"

instance Core.ToQuery RenewOffering where
  toQuery = Core.const Core.mempty

-- | The result of a renewal offering.
--
-- /See:/ 'newRenewOfferingResponse' smart constructor.
data RenewOfferingResponse = RenewOfferingResponse'
  { -- | Represents the status of the offering transaction for the renewal.
    offeringTransaction :: Core.Maybe OfferingTransaction,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RenewOfferingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'offeringTransaction', 'renewOfferingResponse_offeringTransaction' - Represents the status of the offering transaction for the renewal.
--
-- 'httpStatus', 'renewOfferingResponse_httpStatus' - The response's http status code.
newRenewOfferingResponse ::
  -- | 'httpStatus'
  Core.Int ->
  RenewOfferingResponse
newRenewOfferingResponse pHttpStatus_ =
  RenewOfferingResponse'
    { offeringTransaction =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Represents the status of the offering transaction for the renewal.
renewOfferingResponse_offeringTransaction :: Lens.Lens' RenewOfferingResponse (Core.Maybe OfferingTransaction)
renewOfferingResponse_offeringTransaction = Lens.lens (\RenewOfferingResponse' {offeringTransaction} -> offeringTransaction) (\s@RenewOfferingResponse' {} a -> s {offeringTransaction = a} :: RenewOfferingResponse)

-- | The response's http status code.
renewOfferingResponse_httpStatus :: Lens.Lens' RenewOfferingResponse Core.Int
renewOfferingResponse_httpStatus = Lens.lens (\RenewOfferingResponse' {httpStatus} -> httpStatus) (\s@RenewOfferingResponse' {} a -> s {httpStatus = a} :: RenewOfferingResponse)

instance Core.NFData RenewOfferingResponse
