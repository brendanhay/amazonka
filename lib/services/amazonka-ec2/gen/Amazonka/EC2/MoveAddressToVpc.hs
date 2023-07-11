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
-- Module      : Amazonka.EC2.MoveAddressToVpc
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
--
-- We are retiring EC2-Classic. We recommend that you migrate from
-- EC2-Classic to a VPC. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-migrate.html Migrate from EC2-Classic to a VPC>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Amazonka.EC2.MoveAddressToVpc
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
    moveAddressToVpcResponse_allocationId,
    moveAddressToVpcResponse_status,
    moveAddressToVpcResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          MoveAddressToVpcResponse'
            Prelude.<$> (x Data..@? "allocationId")
            Prelude.<*> (x Data..@? "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable MoveAddressToVpc where
  hashWithSalt _salt MoveAddressToVpc' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` publicIp

instance Prelude.NFData MoveAddressToVpc where
  rnf MoveAddressToVpc' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf publicIp

instance Data.ToHeaders MoveAddressToVpc where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath MoveAddressToVpc where
  toPath = Prelude.const "/"

instance Data.ToQuery MoveAddressToVpc where
  toQuery MoveAddressToVpc' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("MoveAddressToVpc" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "PublicIp" Data.=: publicIp
      ]

-- | /See:/ 'newMoveAddressToVpcResponse' smart constructor.
data MoveAddressToVpcResponse = MoveAddressToVpcResponse'
  { -- | The allocation ID for the Elastic IP address.
    allocationId :: Prelude.Maybe Prelude.Text,
    -- | The status of the move of the IP address.
    status :: Prelude.Maybe AddressStatus,
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
-- 'allocationId', 'moveAddressToVpcResponse_allocationId' - The allocation ID for the Elastic IP address.
--
-- 'status', 'moveAddressToVpcResponse_status' - The status of the move of the IP address.
--
-- 'httpStatus', 'moveAddressToVpcResponse_httpStatus' - The response's http status code.
newMoveAddressToVpcResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  MoveAddressToVpcResponse
newMoveAddressToVpcResponse pHttpStatus_ =
  MoveAddressToVpcResponse'
    { allocationId =
        Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The allocation ID for the Elastic IP address.
moveAddressToVpcResponse_allocationId :: Lens.Lens' MoveAddressToVpcResponse (Prelude.Maybe Prelude.Text)
moveAddressToVpcResponse_allocationId = Lens.lens (\MoveAddressToVpcResponse' {allocationId} -> allocationId) (\s@MoveAddressToVpcResponse' {} a -> s {allocationId = a} :: MoveAddressToVpcResponse)

-- | The status of the move of the IP address.
moveAddressToVpcResponse_status :: Lens.Lens' MoveAddressToVpcResponse (Prelude.Maybe AddressStatus)
moveAddressToVpcResponse_status = Lens.lens (\MoveAddressToVpcResponse' {status} -> status) (\s@MoveAddressToVpcResponse' {} a -> s {status = a} :: MoveAddressToVpcResponse)

-- | The response's http status code.
moveAddressToVpcResponse_httpStatus :: Lens.Lens' MoveAddressToVpcResponse Prelude.Int
moveAddressToVpcResponse_httpStatus = Lens.lens (\MoveAddressToVpcResponse' {httpStatus} -> httpStatus) (\s@MoveAddressToVpcResponse' {} a -> s {httpStatus = a} :: MoveAddressToVpcResponse)

instance Prelude.NFData MoveAddressToVpcResponse where
  rnf MoveAddressToVpcResponse' {..} =
    Prelude.rnf allocationId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
