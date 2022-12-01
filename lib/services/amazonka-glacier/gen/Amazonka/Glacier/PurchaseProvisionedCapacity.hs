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
-- Module      : Amazonka.Glacier.PurchaseProvisionedCapacity
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation purchases a provisioned capacity unit for an AWS account.
module Amazonka.Glacier.PurchaseProvisionedCapacity
  ( -- * Creating a Request
    PurchaseProvisionedCapacity (..),
    newPurchaseProvisionedCapacity,

    -- * Request Lenses
    purchaseProvisionedCapacity_accountId,

    -- * Destructuring the Response
    PurchaseProvisionedCapacityResponse (..),
    newPurchaseProvisionedCapacityResponse,

    -- * Response Lenses
    purchaseProvisionedCapacityResponse_capacityId,
    purchaseProvisionedCapacityResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Glacier.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPurchaseProvisionedCapacity' smart constructor.
data PurchaseProvisionedCapacity = PurchaseProvisionedCapacity'
  { -- | The AWS account ID of the account that owns the vault. You can either
    -- specify an AWS account ID or optionally a single \'-\' (hyphen), in
    -- which case Amazon S3 Glacier uses the AWS account ID associated with the
    -- credentials used to sign the request. If you use an account ID, don\'t
    -- include any hyphens (\'-\') in the ID.
    accountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PurchaseProvisionedCapacity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'purchaseProvisionedCapacity_accountId' - The AWS account ID of the account that owns the vault. You can either
-- specify an AWS account ID or optionally a single \'-\' (hyphen), in
-- which case Amazon S3 Glacier uses the AWS account ID associated with the
-- credentials used to sign the request. If you use an account ID, don\'t
-- include any hyphens (\'-\') in the ID.
newPurchaseProvisionedCapacity ::
  -- | 'accountId'
  Prelude.Text ->
  PurchaseProvisionedCapacity
newPurchaseProvisionedCapacity pAccountId_ =
  PurchaseProvisionedCapacity'
    { accountId =
        pAccountId_
    }

-- | The AWS account ID of the account that owns the vault. You can either
-- specify an AWS account ID or optionally a single \'-\' (hyphen), in
-- which case Amazon S3 Glacier uses the AWS account ID associated with the
-- credentials used to sign the request. If you use an account ID, don\'t
-- include any hyphens (\'-\') in the ID.
purchaseProvisionedCapacity_accountId :: Lens.Lens' PurchaseProvisionedCapacity Prelude.Text
purchaseProvisionedCapacity_accountId = Lens.lens (\PurchaseProvisionedCapacity' {accountId} -> accountId) (\s@PurchaseProvisionedCapacity' {} a -> s {accountId = a} :: PurchaseProvisionedCapacity)

instance Core.AWSRequest PurchaseProvisionedCapacity where
  type
    AWSResponse PurchaseProvisionedCapacity =
      PurchaseProvisionedCapacityResponse
  request overrides =
    Request.glacierVersionHeader (Core.version defaultService)
      Prelude.. Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PurchaseProvisionedCapacityResponse'
            Prelude.<$> (h Core..#? "x-amz-capacity-id")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PurchaseProvisionedCapacity where
  hashWithSalt _salt PurchaseProvisionedCapacity' {..} =
    _salt `Prelude.hashWithSalt` accountId

instance Prelude.NFData PurchaseProvisionedCapacity where
  rnf PurchaseProvisionedCapacity' {..} =
    Prelude.rnf accountId

instance Core.ToHeaders PurchaseProvisionedCapacity where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON PurchaseProvisionedCapacity where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath PurchaseProvisionedCapacity where
  toPath PurchaseProvisionedCapacity' {..} =
    Prelude.mconcat
      ["/", Core.toBS accountId, "/provisioned-capacity"]

instance Core.ToQuery PurchaseProvisionedCapacity where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPurchaseProvisionedCapacityResponse' smart constructor.
data PurchaseProvisionedCapacityResponse = PurchaseProvisionedCapacityResponse'
  { -- | The ID that identifies the provisioned capacity unit.
    capacityId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PurchaseProvisionedCapacityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capacityId', 'purchaseProvisionedCapacityResponse_capacityId' - The ID that identifies the provisioned capacity unit.
--
-- 'httpStatus', 'purchaseProvisionedCapacityResponse_httpStatus' - The response's http status code.
newPurchaseProvisionedCapacityResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PurchaseProvisionedCapacityResponse
newPurchaseProvisionedCapacityResponse pHttpStatus_ =
  PurchaseProvisionedCapacityResponse'
    { capacityId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID that identifies the provisioned capacity unit.
purchaseProvisionedCapacityResponse_capacityId :: Lens.Lens' PurchaseProvisionedCapacityResponse (Prelude.Maybe Prelude.Text)
purchaseProvisionedCapacityResponse_capacityId = Lens.lens (\PurchaseProvisionedCapacityResponse' {capacityId} -> capacityId) (\s@PurchaseProvisionedCapacityResponse' {} a -> s {capacityId = a} :: PurchaseProvisionedCapacityResponse)

-- | The response's http status code.
purchaseProvisionedCapacityResponse_httpStatus :: Lens.Lens' PurchaseProvisionedCapacityResponse Prelude.Int
purchaseProvisionedCapacityResponse_httpStatus = Lens.lens (\PurchaseProvisionedCapacityResponse' {httpStatus} -> httpStatus) (\s@PurchaseProvisionedCapacityResponse' {} a -> s {httpStatus = a} :: PurchaseProvisionedCapacityResponse)

instance
  Prelude.NFData
    PurchaseProvisionedCapacityResponse
  where
  rnf PurchaseProvisionedCapacityResponse' {..} =
    Prelude.rnf capacityId
      `Prelude.seq` Prelude.rnf httpStatus
