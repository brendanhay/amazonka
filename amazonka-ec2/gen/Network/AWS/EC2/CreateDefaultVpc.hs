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
-- Module      : Network.AWS.EC2.CreateDefaultVpc
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a default VPC with a size @\/16@ IPv4 CIDR block and a default
-- subnet in each Availability Zone. For more information about the
-- components of a default VPC, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/default-vpc.html Default VPC and Default Subnets>
-- in the /Amazon Virtual Private Cloud User Guide/. You cannot specify the
-- components of the default VPC yourself.
--
-- If you deleted your previous default VPC, you can create a default VPC.
-- You cannot have more than one default VPC per Region.
--
-- If your account supports EC2-Classic, you cannot use this action to
-- create a default VPC in a Region that supports EC2-Classic. If you want
-- a default VPC in a Region that supports EC2-Classic, see \"I really want
-- a default VPC for my existing EC2 account. Is that possible?\" in the
-- <http://aws.amazon.com/vpc/faqs/#Default_VPCs Default VPCs FAQ>.
module Network.AWS.EC2.CreateDefaultVpc
  ( -- * Creating a Request
    CreateDefaultVpc (..),
    newCreateDefaultVpc,

    -- * Request Lenses
    createDefaultVpc_dryRun,

    -- * Destructuring the Response
    CreateDefaultVpcResponse (..),
    newCreateDefaultVpcResponse,

    -- * Response Lenses
    createDefaultVpcResponse_vpc,
    createDefaultVpcResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateDefaultVpc' smart constructor.
data CreateDefaultVpc = CreateDefaultVpc'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateDefaultVpc' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'createDefaultVpc_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
newCreateDefaultVpc ::
  CreateDefaultVpc
newCreateDefaultVpc =
  CreateDefaultVpc' {dryRun = Prelude.Nothing}

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createDefaultVpc_dryRun :: Lens.Lens' CreateDefaultVpc (Prelude.Maybe Prelude.Bool)
createDefaultVpc_dryRun = Lens.lens (\CreateDefaultVpc' {dryRun} -> dryRun) (\s@CreateDefaultVpc' {} a -> s {dryRun = a} :: CreateDefaultVpc)

instance Prelude.AWSRequest CreateDefaultVpc where
  type Rs CreateDefaultVpc = CreateDefaultVpcResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateDefaultVpcResponse'
            Prelude.<$> (x Prelude..@? "vpc")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDefaultVpc

instance Prelude.NFData CreateDefaultVpc

instance Prelude.ToHeaders CreateDefaultVpc where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath CreateDefaultVpc where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateDefaultVpc where
  toQuery CreateDefaultVpc' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("CreateDefaultVpc" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun
      ]

-- | /See:/ 'newCreateDefaultVpcResponse' smart constructor.
data CreateDefaultVpcResponse = CreateDefaultVpcResponse'
  { -- | Information about the VPC.
    vpc :: Prelude.Maybe Vpc,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateDefaultVpcResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpc', 'createDefaultVpcResponse_vpc' - Information about the VPC.
--
-- 'httpStatus', 'createDefaultVpcResponse_httpStatus' - The response's http status code.
newCreateDefaultVpcResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDefaultVpcResponse
newCreateDefaultVpcResponse pHttpStatus_ =
  CreateDefaultVpcResponse'
    { vpc = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the VPC.
createDefaultVpcResponse_vpc :: Lens.Lens' CreateDefaultVpcResponse (Prelude.Maybe Vpc)
createDefaultVpcResponse_vpc = Lens.lens (\CreateDefaultVpcResponse' {vpc} -> vpc) (\s@CreateDefaultVpcResponse' {} a -> s {vpc = a} :: CreateDefaultVpcResponse)

-- | The response's http status code.
createDefaultVpcResponse_httpStatus :: Lens.Lens' CreateDefaultVpcResponse Prelude.Int
createDefaultVpcResponse_httpStatus = Lens.lens (\CreateDefaultVpcResponse' {httpStatus} -> httpStatus) (\s@CreateDefaultVpcResponse' {} a -> s {httpStatus = a} :: CreateDefaultVpcResponse)

instance Prelude.NFData CreateDefaultVpcResponse
