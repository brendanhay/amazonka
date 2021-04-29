{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.DeviceFarm.PurchaseOffering
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Immediately purchases offerings for an AWS account. Offerings renew with
-- the latest total purchased quantity for an offering, unless the renewal
-- was overridden. The API returns a @NotEligible@ error if the user is not
-- permitted to invoke the operation. If you must be able to invoke this
-- operation, contact
-- <mailto:aws-devicefarm-support@amazon.com aws-devicefarm-support\@amazon.com>.
module Network.AWS.DeviceFarm.PurchaseOffering
  ( -- * Creating a Request
    PurchaseOffering (..),
    newPurchaseOffering,

    -- * Request Lenses
    purchaseOffering_quantity,
    purchaseOffering_offeringId,
    purchaseOffering_offeringPromotionId,

    -- * Destructuring the Response
    PurchaseOfferingResponse (..),
    newPurchaseOfferingResponse,

    -- * Response Lenses
    purchaseOfferingResponse_offeringTransaction,
    purchaseOfferingResponse_httpStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request for a purchase offering.
--
-- /See:/ 'newPurchaseOffering' smart constructor.
data PurchaseOffering = PurchaseOffering'
  { -- | The number of device slots to purchase in an offering request.
    quantity :: Prelude.Maybe Prelude.Int,
    -- | The ID of the offering.
    offeringId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the offering promotion to be applied to the purchase.
    offeringPromotionId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PurchaseOffering' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'quantity', 'purchaseOffering_quantity' - The number of device slots to purchase in an offering request.
--
-- 'offeringId', 'purchaseOffering_offeringId' - The ID of the offering.
--
-- 'offeringPromotionId', 'purchaseOffering_offeringPromotionId' - The ID of the offering promotion to be applied to the purchase.
newPurchaseOffering ::
  PurchaseOffering
newPurchaseOffering =
  PurchaseOffering'
    { quantity = Prelude.Nothing,
      offeringId = Prelude.Nothing,
      offeringPromotionId = Prelude.Nothing
    }

-- | The number of device slots to purchase in an offering request.
purchaseOffering_quantity :: Lens.Lens' PurchaseOffering (Prelude.Maybe Prelude.Int)
purchaseOffering_quantity = Lens.lens (\PurchaseOffering' {quantity} -> quantity) (\s@PurchaseOffering' {} a -> s {quantity = a} :: PurchaseOffering)

-- | The ID of the offering.
purchaseOffering_offeringId :: Lens.Lens' PurchaseOffering (Prelude.Maybe Prelude.Text)
purchaseOffering_offeringId = Lens.lens (\PurchaseOffering' {offeringId} -> offeringId) (\s@PurchaseOffering' {} a -> s {offeringId = a} :: PurchaseOffering)

-- | The ID of the offering promotion to be applied to the purchase.
purchaseOffering_offeringPromotionId :: Lens.Lens' PurchaseOffering (Prelude.Maybe Prelude.Text)
purchaseOffering_offeringPromotionId = Lens.lens (\PurchaseOffering' {offeringPromotionId} -> offeringPromotionId) (\s@PurchaseOffering' {} a -> s {offeringPromotionId = a} :: PurchaseOffering)

instance Prelude.AWSRequest PurchaseOffering where
  type Rs PurchaseOffering = PurchaseOfferingResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PurchaseOfferingResponse'
            Prelude.<$> (x Prelude..?> "offeringTransaction")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PurchaseOffering

instance Prelude.NFData PurchaseOffering

instance Prelude.ToHeaders PurchaseOffering where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DeviceFarm_20150623.PurchaseOffering" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON PurchaseOffering where
  toJSON PurchaseOffering' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("quantity" Prelude..=) Prelude.<$> quantity,
            ("offeringId" Prelude..=) Prelude.<$> offeringId,
            ("offeringPromotionId" Prelude..=)
              Prelude.<$> offeringPromotionId
          ]
      )

instance Prelude.ToPath PurchaseOffering where
  toPath = Prelude.const "/"

instance Prelude.ToQuery PurchaseOffering where
  toQuery = Prelude.const Prelude.mempty

-- | The result of the purchase offering (for example, success or failure).
--
-- /See:/ 'newPurchaseOfferingResponse' smart constructor.
data PurchaseOfferingResponse = PurchaseOfferingResponse'
  { -- | Represents the offering transaction for the purchase result.
    offeringTransaction :: Prelude.Maybe OfferingTransaction,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PurchaseOfferingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'offeringTransaction', 'purchaseOfferingResponse_offeringTransaction' - Represents the offering transaction for the purchase result.
--
-- 'httpStatus', 'purchaseOfferingResponse_httpStatus' - The response's http status code.
newPurchaseOfferingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PurchaseOfferingResponse
newPurchaseOfferingResponse pHttpStatus_ =
  PurchaseOfferingResponse'
    { offeringTransaction =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Represents the offering transaction for the purchase result.
purchaseOfferingResponse_offeringTransaction :: Lens.Lens' PurchaseOfferingResponse (Prelude.Maybe OfferingTransaction)
purchaseOfferingResponse_offeringTransaction = Lens.lens (\PurchaseOfferingResponse' {offeringTransaction} -> offeringTransaction) (\s@PurchaseOfferingResponse' {} a -> s {offeringTransaction = a} :: PurchaseOfferingResponse)

-- | The response's http status code.
purchaseOfferingResponse_httpStatus :: Lens.Lens' PurchaseOfferingResponse Prelude.Int
purchaseOfferingResponse_httpStatus = Lens.lens (\PurchaseOfferingResponse' {httpStatus} -> httpStatus) (\s@PurchaseOfferingResponse' {} a -> s {httpStatus = a} :: PurchaseOfferingResponse)

instance Prelude.NFData PurchaseOfferingResponse
