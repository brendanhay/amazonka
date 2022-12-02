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
-- Module      : Amazonka.EC2.ConfirmProductInstance
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Determines whether a product code is associated with an instance. This
-- action can only be used by the owner of the product code. It is useful
-- when a product code owner must verify whether another user\'s instance
-- is eligible for support.
module Amazonka.EC2.ConfirmProductInstance
  ( -- * Creating a Request
    ConfirmProductInstance (..),
    newConfirmProductInstance,

    -- * Request Lenses
    confirmProductInstance_dryRun,
    confirmProductInstance_instanceId,
    confirmProductInstance_productCode,

    -- * Destructuring the Response
    ConfirmProductInstanceResponse (..),
    newConfirmProductInstanceResponse,

    -- * Response Lenses
    confirmProductInstanceResponse_ownerId,
    confirmProductInstanceResponse_return,
    confirmProductInstanceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newConfirmProductInstance' smart constructor.
data ConfirmProductInstance = ConfirmProductInstance'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the instance.
    instanceId :: Prelude.Text,
    -- | The product code. This must be a product code that you own.
    productCode :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfirmProductInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'confirmProductInstance_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'instanceId', 'confirmProductInstance_instanceId' - The ID of the instance.
--
-- 'productCode', 'confirmProductInstance_productCode' - The product code. This must be a product code that you own.
newConfirmProductInstance ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'productCode'
  Prelude.Text ->
  ConfirmProductInstance
newConfirmProductInstance pInstanceId_ pProductCode_ =
  ConfirmProductInstance'
    { dryRun = Prelude.Nothing,
      instanceId = pInstanceId_,
      productCode = pProductCode_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
confirmProductInstance_dryRun :: Lens.Lens' ConfirmProductInstance (Prelude.Maybe Prelude.Bool)
confirmProductInstance_dryRun = Lens.lens (\ConfirmProductInstance' {dryRun} -> dryRun) (\s@ConfirmProductInstance' {} a -> s {dryRun = a} :: ConfirmProductInstance)

-- | The ID of the instance.
confirmProductInstance_instanceId :: Lens.Lens' ConfirmProductInstance Prelude.Text
confirmProductInstance_instanceId = Lens.lens (\ConfirmProductInstance' {instanceId} -> instanceId) (\s@ConfirmProductInstance' {} a -> s {instanceId = a} :: ConfirmProductInstance)

-- | The product code. This must be a product code that you own.
confirmProductInstance_productCode :: Lens.Lens' ConfirmProductInstance Prelude.Text
confirmProductInstance_productCode = Lens.lens (\ConfirmProductInstance' {productCode} -> productCode) (\s@ConfirmProductInstance' {} a -> s {productCode = a} :: ConfirmProductInstance)

instance Core.AWSRequest ConfirmProductInstance where
  type
    AWSResponse ConfirmProductInstance =
      ConfirmProductInstanceResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ConfirmProductInstanceResponse'
            Prelude.<$> (x Data..@? "ownerId")
            Prelude.<*> (x Data..@? "return")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ConfirmProductInstance where
  hashWithSalt _salt ConfirmProductInstance' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` productCode

instance Prelude.NFData ConfirmProductInstance where
  rnf ConfirmProductInstance' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf productCode

instance Data.ToHeaders ConfirmProductInstance where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ConfirmProductInstance where
  toPath = Prelude.const "/"

instance Data.ToQuery ConfirmProductInstance where
  toQuery ConfirmProductInstance' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ConfirmProductInstance" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "InstanceId" Data.=: instanceId,
        "ProductCode" Data.=: productCode
      ]

-- | /See:/ 'newConfirmProductInstanceResponse' smart constructor.
data ConfirmProductInstanceResponse = ConfirmProductInstanceResponse'
  { -- | The Amazon Web Services account ID of the instance owner. This is only
    -- present if the product code is attached to the instance.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The return value of the request. Returns @true@ if the specified product
    -- code is owned by the requester and associated with the specified
    -- instance.
    return' :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfirmProductInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ownerId', 'confirmProductInstanceResponse_ownerId' - The Amazon Web Services account ID of the instance owner. This is only
-- present if the product code is attached to the instance.
--
-- 'return'', 'confirmProductInstanceResponse_return' - The return value of the request. Returns @true@ if the specified product
-- code is owned by the requester and associated with the specified
-- instance.
--
-- 'httpStatus', 'confirmProductInstanceResponse_httpStatus' - The response's http status code.
newConfirmProductInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ConfirmProductInstanceResponse
newConfirmProductInstanceResponse pHttpStatus_ =
  ConfirmProductInstanceResponse'
    { ownerId =
        Prelude.Nothing,
      return' = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Web Services account ID of the instance owner. This is only
-- present if the product code is attached to the instance.
confirmProductInstanceResponse_ownerId :: Lens.Lens' ConfirmProductInstanceResponse (Prelude.Maybe Prelude.Text)
confirmProductInstanceResponse_ownerId = Lens.lens (\ConfirmProductInstanceResponse' {ownerId} -> ownerId) (\s@ConfirmProductInstanceResponse' {} a -> s {ownerId = a} :: ConfirmProductInstanceResponse)

-- | The return value of the request. Returns @true@ if the specified product
-- code is owned by the requester and associated with the specified
-- instance.
confirmProductInstanceResponse_return :: Lens.Lens' ConfirmProductInstanceResponse (Prelude.Maybe Prelude.Bool)
confirmProductInstanceResponse_return = Lens.lens (\ConfirmProductInstanceResponse' {return'} -> return') (\s@ConfirmProductInstanceResponse' {} a -> s {return' = a} :: ConfirmProductInstanceResponse)

-- | The response's http status code.
confirmProductInstanceResponse_httpStatus :: Lens.Lens' ConfirmProductInstanceResponse Prelude.Int
confirmProductInstanceResponse_httpStatus = Lens.lens (\ConfirmProductInstanceResponse' {httpStatus} -> httpStatus) (\s@ConfirmProductInstanceResponse' {} a -> s {httpStatus = a} :: ConfirmProductInstanceResponse)

instance
  Prelude.NFData
    ConfirmProductInstanceResponse
  where
  rnf ConfirmProductInstanceResponse' {..} =
    Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf return'
      `Prelude.seq` Prelude.rnf httpStatus
