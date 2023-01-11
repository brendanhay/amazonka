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
-- Module      : Amazonka.EC2.DeleteNetworkInterface
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified network interface. You must detach the network
-- interface before you can delete it.
module Amazonka.EC2.DeleteNetworkInterface
  ( -- * Creating a Request
    DeleteNetworkInterface (..),
    newDeleteNetworkInterface,

    -- * Request Lenses
    deleteNetworkInterface_dryRun,
    deleteNetworkInterface_networkInterfaceId,

    -- * Destructuring the Response
    DeleteNetworkInterfaceResponse (..),
    newDeleteNetworkInterfaceResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for DeleteNetworkInterface.
--
-- /See:/ 'newDeleteNetworkInterface' smart constructor.
data DeleteNetworkInterface = DeleteNetworkInterface'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the network interface.
    networkInterfaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteNetworkInterface' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteNetworkInterface_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'networkInterfaceId', 'deleteNetworkInterface_networkInterfaceId' - The ID of the network interface.
newDeleteNetworkInterface ::
  -- | 'networkInterfaceId'
  Prelude.Text ->
  DeleteNetworkInterface
newDeleteNetworkInterface pNetworkInterfaceId_ =
  DeleteNetworkInterface'
    { dryRun = Prelude.Nothing,
      networkInterfaceId = pNetworkInterfaceId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteNetworkInterface_dryRun :: Lens.Lens' DeleteNetworkInterface (Prelude.Maybe Prelude.Bool)
deleteNetworkInterface_dryRun = Lens.lens (\DeleteNetworkInterface' {dryRun} -> dryRun) (\s@DeleteNetworkInterface' {} a -> s {dryRun = a} :: DeleteNetworkInterface)

-- | The ID of the network interface.
deleteNetworkInterface_networkInterfaceId :: Lens.Lens' DeleteNetworkInterface Prelude.Text
deleteNetworkInterface_networkInterfaceId = Lens.lens (\DeleteNetworkInterface' {networkInterfaceId} -> networkInterfaceId) (\s@DeleteNetworkInterface' {} a -> s {networkInterfaceId = a} :: DeleteNetworkInterface)

instance Core.AWSRequest DeleteNetworkInterface where
  type
    AWSResponse DeleteNetworkInterface =
      DeleteNetworkInterfaceResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      DeleteNetworkInterfaceResponse'

instance Prelude.Hashable DeleteNetworkInterface where
  hashWithSalt _salt DeleteNetworkInterface' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` networkInterfaceId

instance Prelude.NFData DeleteNetworkInterface where
  rnf DeleteNetworkInterface' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf networkInterfaceId

instance Data.ToHeaders DeleteNetworkInterface where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteNetworkInterface where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteNetworkInterface where
  toQuery DeleteNetworkInterface' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteNetworkInterface" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "NetworkInterfaceId" Data.=: networkInterfaceId
      ]

-- | /See:/ 'newDeleteNetworkInterfaceResponse' smart constructor.
data DeleteNetworkInterfaceResponse = DeleteNetworkInterfaceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteNetworkInterfaceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteNetworkInterfaceResponse ::
  DeleteNetworkInterfaceResponse
newDeleteNetworkInterfaceResponse =
  DeleteNetworkInterfaceResponse'

instance
  Prelude.NFData
    DeleteNetworkInterfaceResponse
  where
  rnf _ = ()
