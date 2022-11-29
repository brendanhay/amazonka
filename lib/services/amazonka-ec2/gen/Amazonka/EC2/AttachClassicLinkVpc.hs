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
-- Module      : Amazonka.EC2.AttachClassicLinkVpc
-- Copyright   : (c) 2013-2022 Brendan Hay
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
-- Links an EC2-Classic instance to a ClassicLink-enabled VPC through one
-- or more of the VPC\'s security groups. You cannot link an EC2-Classic
-- instance to more than one VPC at a time. You can only link an instance
-- that\'s in the @running@ state. An instance is automatically unlinked
-- from a VPC when it\'s stopped - you can link it to the VPC again when
-- you restart it.
--
-- After you\'ve linked an instance, you cannot change the VPC security
-- groups that are associated with it. To change the security groups, you
-- must first unlink the instance, and then link it again.
--
-- Linking your instance to a VPC is sometimes referred to as /attaching/
-- your instance.
module Amazonka.EC2.AttachClassicLinkVpc
  ( -- * Creating a Request
    AttachClassicLinkVpc (..),
    newAttachClassicLinkVpc,

    -- * Request Lenses
    attachClassicLinkVpc_dryRun,
    attachClassicLinkVpc_groups,
    attachClassicLinkVpc_instanceId,
    attachClassicLinkVpc_vpcId,

    -- * Destructuring the Response
    AttachClassicLinkVpcResponse (..),
    newAttachClassicLinkVpcResponse,

    -- * Response Lenses
    attachClassicLinkVpcResponse_return,
    attachClassicLinkVpcResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAttachClassicLinkVpc' smart constructor.
data AttachClassicLinkVpc = AttachClassicLinkVpc'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of one or more of the VPC\'s security groups. You cannot specify
    -- security groups from a different VPC.
    groups :: [Prelude.Text],
    -- | The ID of an EC2-Classic instance to link to the ClassicLink-enabled
    -- VPC.
    instanceId :: Prelude.Text,
    -- | The ID of a ClassicLink-enabled VPC.
    vpcId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttachClassicLinkVpc' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'attachClassicLinkVpc_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'groups', 'attachClassicLinkVpc_groups' - The ID of one or more of the VPC\'s security groups. You cannot specify
-- security groups from a different VPC.
--
-- 'instanceId', 'attachClassicLinkVpc_instanceId' - The ID of an EC2-Classic instance to link to the ClassicLink-enabled
-- VPC.
--
-- 'vpcId', 'attachClassicLinkVpc_vpcId' - The ID of a ClassicLink-enabled VPC.
newAttachClassicLinkVpc ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'vpcId'
  Prelude.Text ->
  AttachClassicLinkVpc
newAttachClassicLinkVpc pInstanceId_ pVpcId_ =
  AttachClassicLinkVpc'
    { dryRun = Prelude.Nothing,
      groups = Prelude.mempty,
      instanceId = pInstanceId_,
      vpcId = pVpcId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
attachClassicLinkVpc_dryRun :: Lens.Lens' AttachClassicLinkVpc (Prelude.Maybe Prelude.Bool)
attachClassicLinkVpc_dryRun = Lens.lens (\AttachClassicLinkVpc' {dryRun} -> dryRun) (\s@AttachClassicLinkVpc' {} a -> s {dryRun = a} :: AttachClassicLinkVpc)

-- | The ID of one or more of the VPC\'s security groups. You cannot specify
-- security groups from a different VPC.
attachClassicLinkVpc_groups :: Lens.Lens' AttachClassicLinkVpc [Prelude.Text]
attachClassicLinkVpc_groups = Lens.lens (\AttachClassicLinkVpc' {groups} -> groups) (\s@AttachClassicLinkVpc' {} a -> s {groups = a} :: AttachClassicLinkVpc) Prelude.. Lens.coerced

-- | The ID of an EC2-Classic instance to link to the ClassicLink-enabled
-- VPC.
attachClassicLinkVpc_instanceId :: Lens.Lens' AttachClassicLinkVpc Prelude.Text
attachClassicLinkVpc_instanceId = Lens.lens (\AttachClassicLinkVpc' {instanceId} -> instanceId) (\s@AttachClassicLinkVpc' {} a -> s {instanceId = a} :: AttachClassicLinkVpc)

-- | The ID of a ClassicLink-enabled VPC.
attachClassicLinkVpc_vpcId :: Lens.Lens' AttachClassicLinkVpc Prelude.Text
attachClassicLinkVpc_vpcId = Lens.lens (\AttachClassicLinkVpc' {vpcId} -> vpcId) (\s@AttachClassicLinkVpc' {} a -> s {vpcId = a} :: AttachClassicLinkVpc)

instance Core.AWSRequest AttachClassicLinkVpc where
  type
    AWSResponse AttachClassicLinkVpc =
      AttachClassicLinkVpcResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          AttachClassicLinkVpcResponse'
            Prelude.<$> (x Core..@? "return")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AttachClassicLinkVpc where
  hashWithSalt _salt AttachClassicLinkVpc' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` groups
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData AttachClassicLinkVpc where
  rnf AttachClassicLinkVpc' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf groups
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf vpcId

instance Core.ToHeaders AttachClassicLinkVpc where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath AttachClassicLinkVpc where
  toPath = Prelude.const "/"

instance Core.ToQuery AttachClassicLinkVpc where
  toQuery AttachClassicLinkVpc' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("AttachClassicLinkVpc" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun,
        Core.toQueryList "SecurityGroupId" groups,
        "InstanceId" Core.=: instanceId,
        "VpcId" Core.=: vpcId
      ]

-- | /See:/ 'newAttachClassicLinkVpcResponse' smart constructor.
data AttachClassicLinkVpcResponse = AttachClassicLinkVpcResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, it returns an error.
    return' :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttachClassicLinkVpcResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'return'', 'attachClassicLinkVpcResponse_return' - Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- 'httpStatus', 'attachClassicLinkVpcResponse_httpStatus' - The response's http status code.
newAttachClassicLinkVpcResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AttachClassicLinkVpcResponse
newAttachClassicLinkVpcResponse pHttpStatus_ =
  AttachClassicLinkVpcResponse'
    { return' =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
attachClassicLinkVpcResponse_return :: Lens.Lens' AttachClassicLinkVpcResponse (Prelude.Maybe Prelude.Bool)
attachClassicLinkVpcResponse_return = Lens.lens (\AttachClassicLinkVpcResponse' {return'} -> return') (\s@AttachClassicLinkVpcResponse' {} a -> s {return' = a} :: AttachClassicLinkVpcResponse)

-- | The response's http status code.
attachClassicLinkVpcResponse_httpStatus :: Lens.Lens' AttachClassicLinkVpcResponse Prelude.Int
attachClassicLinkVpcResponse_httpStatus = Lens.lens (\AttachClassicLinkVpcResponse' {httpStatus} -> httpStatus) (\s@AttachClassicLinkVpcResponse' {} a -> s {httpStatus = a} :: AttachClassicLinkVpcResponse)

instance Prelude.NFData AttachClassicLinkVpcResponse where
  rnf AttachClassicLinkVpcResponse' {..} =
    Prelude.rnf return'
      `Prelude.seq` Prelude.rnf httpStatus
