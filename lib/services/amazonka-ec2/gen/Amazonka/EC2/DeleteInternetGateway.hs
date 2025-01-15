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
-- Module      : Amazonka.EC2.DeleteInternetGateway
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified internet gateway. You must detach the internet
-- gateway from the VPC before you can delete it.
module Amazonka.EC2.DeleteInternetGateway
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DeleteInternetGateway where
  type
    AWSResponse DeleteInternetGateway =
      DeleteInternetGatewayResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull DeleteInternetGatewayResponse'

instance Prelude.Hashable DeleteInternetGateway where
  hashWithSalt _salt DeleteInternetGateway' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` internetGatewayId

instance Prelude.NFData DeleteInternetGateway where
  rnf DeleteInternetGateway' {..} =
    Prelude.rnf dryRun `Prelude.seq`
      Prelude.rnf internetGatewayId

instance Data.ToHeaders DeleteInternetGateway where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteInternetGateway where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteInternetGateway where
  toQuery DeleteInternetGateway' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteInternetGateway" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "InternetGatewayId" Data.=: internetGatewayId
      ]

-- | /See:/ 'newDeleteInternetGatewayResponse' smart constructor.
data DeleteInternetGatewayResponse = DeleteInternetGatewayResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteInternetGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteInternetGatewayResponse ::
  DeleteInternetGatewayResponse
newDeleteInternetGatewayResponse =
  DeleteInternetGatewayResponse'

instance Prelude.NFData DeleteInternetGatewayResponse where
  rnf _ = ()
