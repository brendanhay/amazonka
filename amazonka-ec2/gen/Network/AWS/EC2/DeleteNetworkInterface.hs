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
-- Module      : Network.AWS.EC2.DeleteNetworkInterface
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified network interface. You must detach the network
-- interface before you can delete it.
module Network.AWS.EC2.DeleteNetworkInterface
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

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DeleteNetworkInterface.
--
-- /See:/ 'newDeleteNetworkInterface' smart constructor.
data DeleteNetworkInterface = DeleteNetworkInterface'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the network interface.
    networkInterfaceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DeleteNetworkInterface
newDeleteNetworkInterface pNetworkInterfaceId_ =
  DeleteNetworkInterface'
    { dryRun = Core.Nothing,
      networkInterfaceId = pNetworkInterfaceId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteNetworkInterface_dryRun :: Lens.Lens' DeleteNetworkInterface (Core.Maybe Core.Bool)
deleteNetworkInterface_dryRun = Lens.lens (\DeleteNetworkInterface' {dryRun} -> dryRun) (\s@DeleteNetworkInterface' {} a -> s {dryRun = a} :: DeleteNetworkInterface)

-- | The ID of the network interface.
deleteNetworkInterface_networkInterfaceId :: Lens.Lens' DeleteNetworkInterface Core.Text
deleteNetworkInterface_networkInterfaceId = Lens.lens (\DeleteNetworkInterface' {networkInterfaceId} -> networkInterfaceId) (\s@DeleteNetworkInterface' {} a -> s {networkInterfaceId = a} :: DeleteNetworkInterface)

instance Core.AWSRequest DeleteNetworkInterface where
  type
    AWSResponse DeleteNetworkInterface =
      DeleteNetworkInterfaceResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      DeleteNetworkInterfaceResponse'

instance Core.Hashable DeleteNetworkInterface

instance Core.NFData DeleteNetworkInterface

instance Core.ToHeaders DeleteNetworkInterface where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteNetworkInterface where
  toPath = Core.const "/"

instance Core.ToQuery DeleteNetworkInterface where
  toQuery DeleteNetworkInterface' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeleteNetworkInterface" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "NetworkInterfaceId" Core.=: networkInterfaceId
      ]

-- | /See:/ 'newDeleteNetworkInterfaceResponse' smart constructor.
data DeleteNetworkInterfaceResponse = DeleteNetworkInterfaceResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteNetworkInterfaceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteNetworkInterfaceResponse ::
  DeleteNetworkInterfaceResponse
newDeleteNetworkInterfaceResponse =
  DeleteNetworkInterfaceResponse'

instance Core.NFData DeleteNetworkInterfaceResponse
