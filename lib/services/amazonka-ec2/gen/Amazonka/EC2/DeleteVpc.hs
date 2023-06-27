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
-- Module      : Amazonka.EC2.DeleteVpc
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified VPC. You must detach or delete all gateways and
-- resources that are associated with the VPC before you can delete it. For
-- example, you must terminate all instances running in the VPC, delete all
-- security groups associated with the VPC (except the default one), delete
-- all route tables associated with the VPC (except the default one), and
-- so on.
module Amazonka.EC2.DeleteVpc
  ( -- * Creating a Request
    DeleteVpc (..),
    newDeleteVpc,

    -- * Request Lenses
    deleteVpc_dryRun,
    deleteVpc_vpcId,

    -- * Destructuring the Response
    DeleteVpcResponse (..),
    newDeleteVpcResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteVpc' smart constructor.
data DeleteVpc = DeleteVpc'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the VPC.
    vpcId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVpc' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteVpc_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'vpcId', 'deleteVpc_vpcId' - The ID of the VPC.
newDeleteVpc ::
  -- | 'vpcId'
  Prelude.Text ->
  DeleteVpc
newDeleteVpc pVpcId_ =
  DeleteVpc'
    { dryRun = Prelude.Nothing,
      vpcId = pVpcId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteVpc_dryRun :: Lens.Lens' DeleteVpc (Prelude.Maybe Prelude.Bool)
deleteVpc_dryRun = Lens.lens (\DeleteVpc' {dryRun} -> dryRun) (\s@DeleteVpc' {} a -> s {dryRun = a} :: DeleteVpc)

-- | The ID of the VPC.
deleteVpc_vpcId :: Lens.Lens' DeleteVpc Prelude.Text
deleteVpc_vpcId = Lens.lens (\DeleteVpc' {vpcId} -> vpcId) (\s@DeleteVpc' {} a -> s {vpcId = a} :: DeleteVpc)

instance Core.AWSRequest DeleteVpc where
  type AWSResponse DeleteVpc = DeleteVpcResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response = Response.receiveNull DeleteVpcResponse'

instance Prelude.Hashable DeleteVpc where
  hashWithSalt _salt DeleteVpc' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData DeleteVpc where
  rnf DeleteVpc' {..} =
    Prelude.rnf dryRun `Prelude.seq` Prelude.rnf vpcId

instance Data.ToHeaders DeleteVpc where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteVpc where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteVpc where
  toQuery DeleteVpc' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteVpc" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "VpcId" Data.=: vpcId
      ]

-- | /See:/ 'newDeleteVpcResponse' smart constructor.
data DeleteVpcResponse = DeleteVpcResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVpcResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteVpcResponse ::
  DeleteVpcResponse
newDeleteVpcResponse = DeleteVpcResponse'

instance Prelude.NFData DeleteVpcResponse where
  rnf _ = ()
