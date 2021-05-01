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

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteInternetGateway' smart constructor.
data DeleteInternetGateway = DeleteInternetGateway'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the internet gateway.
    internetGatewayId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeleteInternetGateway
newDeleteInternetGateway pInternetGatewayId_ =
  DeleteInternetGateway'
    { dryRun = Prelude.Nothing,
      internetGatewayId = pInternetGatewayId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteInternetGateway_dryRun :: Lens.Lens' DeleteInternetGateway (Prelude.Maybe Prelude.Bool)
deleteInternetGateway_dryRun = Lens.lens (\DeleteInternetGateway' {dryRun} -> dryRun) (\s@DeleteInternetGateway' {} a -> s {dryRun = a} :: DeleteInternetGateway)

-- | The ID of the internet gateway.
deleteInternetGateway_internetGatewayId :: Lens.Lens' DeleteInternetGateway Prelude.Text
deleteInternetGateway_internetGatewayId = Lens.lens (\DeleteInternetGateway' {internetGatewayId} -> internetGatewayId) (\s@DeleteInternetGateway' {} a -> s {internetGatewayId = a} :: DeleteInternetGateway)

instance Prelude.AWSRequest DeleteInternetGateway where
  type
    Rs DeleteInternetGateway =
      DeleteInternetGatewayResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull DeleteInternetGatewayResponse'

instance Prelude.Hashable DeleteInternetGateway

instance Prelude.NFData DeleteInternetGateway

instance Prelude.ToHeaders DeleteInternetGateway where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteInternetGateway where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteInternetGateway where
  toQuery DeleteInternetGateway' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeleteInternetGateway" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        "InternetGatewayId" Prelude.=: internetGatewayId
      ]

-- | /See:/ 'newDeleteInternetGatewayResponse' smart constructor.
data DeleteInternetGatewayResponse = DeleteInternetGatewayResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteInternetGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteInternetGatewayResponse ::
  DeleteInternetGatewayResponse
newDeleteInternetGatewayResponse =
  DeleteInternetGatewayResponse'

instance Prelude.NFData DeleteInternetGatewayResponse
