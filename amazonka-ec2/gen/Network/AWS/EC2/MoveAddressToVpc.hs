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
-- Module      : Network.AWS.EC2.MoveAddressToVpc
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Moves an Elastic IP address from the EC2-Classic platform to the EC2-VPC
-- platform. The Elastic IP address must be allocated to your account for
-- more than 24 hours, and it must not be associated with an instance.
-- After the Elastic IP address is moved, it is no longer available for use
-- in the EC2-Classic platform, unless you move it back using the
-- RestoreAddressToClassic request. You cannot move an Elastic IP address
-- that was originally allocated for use in the EC2-VPC platform to the
-- EC2-Classic platform.
module Network.AWS.EC2.MoveAddressToVpc
  ( -- * Creating a Request
    MoveAddressToVpc (..),
    newMoveAddressToVpc,

    -- * Request Lenses
    moveAddressToVpc_dryRun,
    moveAddressToVpc_publicIp,

    -- * Destructuring the Response
    MoveAddressToVpcResponse (..),
    newMoveAddressToVpcResponse,

    -- * Response Lenses
    moveAddressToVpcResponse_status,
    moveAddressToVpcResponse_allocationId,
    moveAddressToVpcResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newMoveAddressToVpc' smart constructor.
data MoveAddressToVpc = MoveAddressToVpc'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The Elastic IP address.
    publicIp :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MoveAddressToVpc' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'moveAddressToVpc_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'publicIp', 'moveAddressToVpc_publicIp' - The Elastic IP address.
newMoveAddressToVpc ::
  -- | 'publicIp'
  Prelude.Text ->
  MoveAddressToVpc
newMoveAddressToVpc pPublicIp_ =
  MoveAddressToVpc'
    { dryRun = Prelude.Nothing,
      publicIp = pPublicIp_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
moveAddressToVpc_dryRun :: Lens.Lens' MoveAddressToVpc (Prelude.Maybe Prelude.Bool)
moveAddressToVpc_dryRun = Lens.lens (\MoveAddressToVpc' {dryRun} -> dryRun) (\s@MoveAddressToVpc' {} a -> s {dryRun = a} :: MoveAddressToVpc)

-- | The Elastic IP address.
moveAddressToVpc_publicIp :: Lens.Lens' MoveAddressToVpc Prelude.Text
moveAddressToVpc_publicIp = Lens.lens (\MoveAddressToVpc' {publicIp} -> publicIp) (\s@MoveAddressToVpc' {} a -> s {publicIp = a} :: MoveAddressToVpc)

instance Core.AWSRequest MoveAddressToVpc where
  type
    AWSResponse MoveAddressToVpc =
      MoveAddressToVpcResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          MoveAddressToVpcResponse'
            Prelude.<$> (x Core..@? "status")
            Prelude.<*> (x Core..@? "allocationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable MoveAddressToVpc

instance Prelude.NFData MoveAddressToVpc

instance Core.ToHeaders MoveAddressToVpc where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath MoveAddressToVpc where
  toPath = Prelude.const "/"

instance Core.ToQuery MoveAddressToVpc where
  toQuery MoveAddressToVpc' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("MoveAddressToVpc" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun,
        "PublicIp" Core.=: publicIp
      ]

-- | /See:/ 'newMoveAddressToVpcResponse' smart constructor.
data MoveAddressToVpcResponse = MoveAddressToVpcResponse'
  { -- | The status of the move of the IP address.
    status :: Prelude.Maybe AddressStatus,
    -- | The allocation ID for the Elastic IP address.
    allocationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MoveAddressToVpcResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'moveAddressToVpcResponse_status' - The status of the move of the IP address.
--
-- 'allocationId', 'moveAddressToVpcResponse_allocationId' - The allocation ID for the Elastic IP address.
--
-- 'httpStatus', 'moveAddressToVpcResponse_httpStatus' - The response's http status code.
newMoveAddressToVpcResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  MoveAddressToVpcResponse
newMoveAddressToVpcResponse pHttpStatus_ =
  MoveAddressToVpcResponse'
    { status = Prelude.Nothing,
      allocationId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the move of the IP address.
moveAddressToVpcResponse_status :: Lens.Lens' MoveAddressToVpcResponse (Prelude.Maybe AddressStatus)
moveAddressToVpcResponse_status = Lens.lens (\MoveAddressToVpcResponse' {status} -> status) (\s@MoveAddressToVpcResponse' {} a -> s {status = a} :: MoveAddressToVpcResponse)

-- | The allocation ID for the Elastic IP address.
moveAddressToVpcResponse_allocationId :: Lens.Lens' MoveAddressToVpcResponse (Prelude.Maybe Prelude.Text)
moveAddressToVpcResponse_allocationId = Lens.lens (\MoveAddressToVpcResponse' {allocationId} -> allocationId) (\s@MoveAddressToVpcResponse' {} a -> s {allocationId = a} :: MoveAddressToVpcResponse)

-- | The response's http status code.
moveAddressToVpcResponse_httpStatus :: Lens.Lens' MoveAddressToVpcResponse Prelude.Int
moveAddressToVpcResponse_httpStatus = Lens.lens (\MoveAddressToVpcResponse' {httpStatus} -> httpStatus) (\s@MoveAddressToVpcResponse' {} a -> s {httpStatus = a} :: MoveAddressToVpcResponse)

instance Prelude.NFData MoveAddressToVpcResponse
