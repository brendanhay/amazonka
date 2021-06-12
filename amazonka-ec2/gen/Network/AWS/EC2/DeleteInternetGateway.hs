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
-- Module      : Network.AWS.EC2.DeleteInternetGateway
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified internet gateway. You must detach the internet
-- gateway from the VPC before you can delete it.
module Network.AWS.EC2.DeleteInternetGateway
  ( -- * Creating a Request
    DeleteInternetGateway (..),
    newDeleteInternetGateway,

    -- * Request Lenses
    deleteInternetGateway_dryRun,
    deleteInternetGateway_internetGatewayId,

    -- * Destructuring the Response
    DeleteInternetGatewayResponse (..),
    newDeleteInternetGatewayResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteInternetGateway' smart constructor.
data DeleteInternetGateway = DeleteInternetGateway'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the internet gateway.
    internetGatewayId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteInternetGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteInternetGateway_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'internetGatewayId', 'deleteInternetGateway_internetGatewayId' - The ID of the internet gateway.
newDeleteInternetGateway ::
  -- | 'internetGatewayId'
  Core.Text ->
  DeleteInternetGateway
newDeleteInternetGateway pInternetGatewayId_ =
  DeleteInternetGateway'
    { dryRun = Core.Nothing,
      internetGatewayId = pInternetGatewayId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteInternetGateway_dryRun :: Lens.Lens' DeleteInternetGateway (Core.Maybe Core.Bool)
deleteInternetGateway_dryRun = Lens.lens (\DeleteInternetGateway' {dryRun} -> dryRun) (\s@DeleteInternetGateway' {} a -> s {dryRun = a} :: DeleteInternetGateway)

-- | The ID of the internet gateway.
deleteInternetGateway_internetGatewayId :: Lens.Lens' DeleteInternetGateway Core.Text
deleteInternetGateway_internetGatewayId = Lens.lens (\DeleteInternetGateway' {internetGatewayId} -> internetGatewayId) (\s@DeleteInternetGateway' {} a -> s {internetGatewayId = a} :: DeleteInternetGateway)

instance Core.AWSRequest DeleteInternetGateway where
  type
    AWSResponse DeleteInternetGateway =
      DeleteInternetGatewayResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull DeleteInternetGatewayResponse'

instance Core.Hashable DeleteInternetGateway

instance Core.NFData DeleteInternetGateway

instance Core.ToHeaders DeleteInternetGateway where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteInternetGateway where
  toPath = Core.const "/"

instance Core.ToQuery DeleteInternetGateway where
  toQuery DeleteInternetGateway' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeleteInternetGateway" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "InternetGatewayId" Core.=: internetGatewayId
      ]

-- | /See:/ 'newDeleteInternetGatewayResponse' smart constructor.
data DeleteInternetGatewayResponse = DeleteInternetGatewayResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteInternetGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteInternetGatewayResponse ::
  DeleteInternetGatewayResponse
newDeleteInternetGatewayResponse =
  DeleteInternetGatewayResponse'

instance Core.NFData DeleteInternetGatewayResponse
