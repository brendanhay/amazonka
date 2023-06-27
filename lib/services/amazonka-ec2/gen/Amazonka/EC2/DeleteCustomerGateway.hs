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
-- Module      : Amazonka.EC2.DeleteCustomerGateway
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified customer gateway. You must delete the VPN
-- connection before you can delete the customer gateway.
module Amazonka.EC2.DeleteCustomerGateway
  ( -- * Creating a Request
    DeleteCustomerGateway (..),
    newDeleteCustomerGateway,

    -- * Request Lenses
    deleteCustomerGateway_dryRun,
    deleteCustomerGateway_customerGatewayId,

    -- * Destructuring the Response
    DeleteCustomerGatewayResponse (..),
    newDeleteCustomerGatewayResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for DeleteCustomerGateway.
--
-- /See:/ 'newDeleteCustomerGateway' smart constructor.
data DeleteCustomerGateway = DeleteCustomerGateway'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the customer gateway.
    customerGatewayId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCustomerGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteCustomerGateway_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'customerGatewayId', 'deleteCustomerGateway_customerGatewayId' - The ID of the customer gateway.
newDeleteCustomerGateway ::
  -- | 'customerGatewayId'
  Prelude.Text ->
  DeleteCustomerGateway
newDeleteCustomerGateway pCustomerGatewayId_ =
  DeleteCustomerGateway'
    { dryRun = Prelude.Nothing,
      customerGatewayId = pCustomerGatewayId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteCustomerGateway_dryRun :: Lens.Lens' DeleteCustomerGateway (Prelude.Maybe Prelude.Bool)
deleteCustomerGateway_dryRun = Lens.lens (\DeleteCustomerGateway' {dryRun} -> dryRun) (\s@DeleteCustomerGateway' {} a -> s {dryRun = a} :: DeleteCustomerGateway)

-- | The ID of the customer gateway.
deleteCustomerGateway_customerGatewayId :: Lens.Lens' DeleteCustomerGateway Prelude.Text
deleteCustomerGateway_customerGatewayId = Lens.lens (\DeleteCustomerGateway' {customerGatewayId} -> customerGatewayId) (\s@DeleteCustomerGateway' {} a -> s {customerGatewayId = a} :: DeleteCustomerGateway)

instance Core.AWSRequest DeleteCustomerGateway where
  type
    AWSResponse DeleteCustomerGateway =
      DeleteCustomerGatewayResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull DeleteCustomerGatewayResponse'

instance Prelude.Hashable DeleteCustomerGateway where
  hashWithSalt _salt DeleteCustomerGateway' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` customerGatewayId

instance Prelude.NFData DeleteCustomerGateway where
  rnf DeleteCustomerGateway' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf customerGatewayId

instance Data.ToHeaders DeleteCustomerGateway where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteCustomerGateway where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteCustomerGateway where
  toQuery DeleteCustomerGateway' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteCustomerGateway" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "CustomerGatewayId" Data.=: customerGatewayId
      ]

-- | /See:/ 'newDeleteCustomerGatewayResponse' smart constructor.
data DeleteCustomerGatewayResponse = DeleteCustomerGatewayResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCustomerGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteCustomerGatewayResponse ::
  DeleteCustomerGatewayResponse
newDeleteCustomerGatewayResponse =
  DeleteCustomerGatewayResponse'

instance Prelude.NFData DeleteCustomerGatewayResponse where
  rnf _ = ()
