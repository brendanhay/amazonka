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
-- Module      : Amazonka.EC2.DetachClassicLinkVpc
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- We are retiring EC2-Classic. We recommend that you migrate from
-- EC2-Classic to a VPC. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-migrate.html Migrate from EC2-Classic to a VPC>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- Unlinks (detaches) a linked EC2-Classic instance from a VPC. After the
-- instance has been unlinked, the VPC security groups are no longer
-- associated with it. An instance is automatically unlinked from a VPC
-- when it\'s stopped.
module Amazonka.EC2.DetachClassicLinkVpc
  ( -- * Creating a Request
    DetachClassicLinkVpc (..),
    newDetachClassicLinkVpc,

    -- * Request Lenses
    detachClassicLinkVpc_dryRun,
    detachClassicLinkVpc_instanceId,
    detachClassicLinkVpc_vpcId,

    -- * Destructuring the Response
    DetachClassicLinkVpcResponse (..),
    newDetachClassicLinkVpcResponse,

    -- * Response Lenses
    detachClassicLinkVpcResponse_return,
    detachClassicLinkVpcResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDetachClassicLinkVpc' smart constructor.
data DetachClassicLinkVpc = DetachClassicLinkVpc'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the instance to unlink from the VPC.
    instanceId :: Prelude.Text,
    -- | The ID of the VPC to which the instance is linked.
    vpcId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetachClassicLinkVpc' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'detachClassicLinkVpc_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'instanceId', 'detachClassicLinkVpc_instanceId' - The ID of the instance to unlink from the VPC.
--
-- 'vpcId', 'detachClassicLinkVpc_vpcId' - The ID of the VPC to which the instance is linked.
newDetachClassicLinkVpc ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'vpcId'
  Prelude.Text ->
  DetachClassicLinkVpc
newDetachClassicLinkVpc pInstanceId_ pVpcId_ =
  DetachClassicLinkVpc'
    { dryRun = Prelude.Nothing,
      instanceId = pInstanceId_,
      vpcId = pVpcId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
detachClassicLinkVpc_dryRun :: Lens.Lens' DetachClassicLinkVpc (Prelude.Maybe Prelude.Bool)
detachClassicLinkVpc_dryRun = Lens.lens (\DetachClassicLinkVpc' {dryRun} -> dryRun) (\s@DetachClassicLinkVpc' {} a -> s {dryRun = a} :: DetachClassicLinkVpc)

-- | The ID of the instance to unlink from the VPC.
detachClassicLinkVpc_instanceId :: Lens.Lens' DetachClassicLinkVpc Prelude.Text
detachClassicLinkVpc_instanceId = Lens.lens (\DetachClassicLinkVpc' {instanceId} -> instanceId) (\s@DetachClassicLinkVpc' {} a -> s {instanceId = a} :: DetachClassicLinkVpc)

-- | The ID of the VPC to which the instance is linked.
detachClassicLinkVpc_vpcId :: Lens.Lens' DetachClassicLinkVpc Prelude.Text
detachClassicLinkVpc_vpcId = Lens.lens (\DetachClassicLinkVpc' {vpcId} -> vpcId) (\s@DetachClassicLinkVpc' {} a -> s {vpcId = a} :: DetachClassicLinkVpc)

instance Core.AWSRequest DetachClassicLinkVpc where
  type
    AWSResponse DetachClassicLinkVpc =
      DetachClassicLinkVpcResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DetachClassicLinkVpcResponse'
            Prelude.<$> (x Data..@? "return")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DetachClassicLinkVpc where
  hashWithSalt _salt DetachClassicLinkVpc' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData DetachClassicLinkVpc where
  rnf DetachClassicLinkVpc' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf vpcId

instance Data.ToHeaders DetachClassicLinkVpc where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DetachClassicLinkVpc where
  toPath = Prelude.const "/"

instance Data.ToQuery DetachClassicLinkVpc where
  toQuery DetachClassicLinkVpc' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DetachClassicLinkVpc" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "InstanceId" Data.=: instanceId,
        "VpcId" Data.=: vpcId
      ]

-- | /See:/ 'newDetachClassicLinkVpcResponse' smart constructor.
data DetachClassicLinkVpcResponse = DetachClassicLinkVpcResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, it returns an error.
    return' :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetachClassicLinkVpcResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'return'', 'detachClassicLinkVpcResponse_return' - Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- 'httpStatus', 'detachClassicLinkVpcResponse_httpStatus' - The response's http status code.
newDetachClassicLinkVpcResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DetachClassicLinkVpcResponse
newDetachClassicLinkVpcResponse pHttpStatus_ =
  DetachClassicLinkVpcResponse'
    { return' =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
detachClassicLinkVpcResponse_return :: Lens.Lens' DetachClassicLinkVpcResponse (Prelude.Maybe Prelude.Bool)
detachClassicLinkVpcResponse_return = Lens.lens (\DetachClassicLinkVpcResponse' {return'} -> return') (\s@DetachClassicLinkVpcResponse' {} a -> s {return' = a} :: DetachClassicLinkVpcResponse)

-- | The response's http status code.
detachClassicLinkVpcResponse_httpStatus :: Lens.Lens' DetachClassicLinkVpcResponse Prelude.Int
detachClassicLinkVpcResponse_httpStatus = Lens.lens (\DetachClassicLinkVpcResponse' {httpStatus} -> httpStatus) (\s@DetachClassicLinkVpcResponse' {} a -> s {httpStatus = a} :: DetachClassicLinkVpcResponse)

instance Prelude.NFData DetachClassicLinkVpcResponse where
  rnf DetachClassicLinkVpcResponse' {..} =
    Prelude.rnf return'
      `Prelude.seq` Prelude.rnf httpStatus
