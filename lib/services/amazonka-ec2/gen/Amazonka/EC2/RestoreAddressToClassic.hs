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
-- Module      : Amazonka.EC2.RestoreAddressToClassic
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores an Elastic IP address that was previously moved to the EC2-VPC
-- platform back to the EC2-Classic platform. You cannot move an Elastic IP
-- address that was originally allocated for use in EC2-VPC. The Elastic IP
-- address must not be associated with an instance or network interface.
--
-- We are retiring EC2-Classic. We recommend that you migrate from
-- EC2-Classic to a VPC. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-migrate.html Migrate from EC2-Classic to a VPC>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Amazonka.EC2.RestoreAddressToClassic
  ( -- * Creating a Request
    RestoreAddressToClassic (..),
    newRestoreAddressToClassic,

    -- * Request Lenses
    restoreAddressToClassic_dryRun,
    restoreAddressToClassic_publicIp,

    -- * Destructuring the Response
    RestoreAddressToClassicResponse (..),
    newRestoreAddressToClassicResponse,

    -- * Response Lenses
    restoreAddressToClassicResponse_status,
    restoreAddressToClassicResponse_publicIp,
    restoreAddressToClassicResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRestoreAddressToClassic' smart constructor.
data RestoreAddressToClassic = RestoreAddressToClassic'
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
-- Create a value of 'RestoreAddressToClassic' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'restoreAddressToClassic_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'publicIp', 'restoreAddressToClassic_publicIp' - The Elastic IP address.
newRestoreAddressToClassic ::
  -- | 'publicIp'
  Prelude.Text ->
  RestoreAddressToClassic
newRestoreAddressToClassic pPublicIp_ =
  RestoreAddressToClassic'
    { dryRun = Prelude.Nothing,
      publicIp = pPublicIp_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
restoreAddressToClassic_dryRun :: Lens.Lens' RestoreAddressToClassic (Prelude.Maybe Prelude.Bool)
restoreAddressToClassic_dryRun = Lens.lens (\RestoreAddressToClassic' {dryRun} -> dryRun) (\s@RestoreAddressToClassic' {} a -> s {dryRun = a} :: RestoreAddressToClassic)

-- | The Elastic IP address.
restoreAddressToClassic_publicIp :: Lens.Lens' RestoreAddressToClassic Prelude.Text
restoreAddressToClassic_publicIp = Lens.lens (\RestoreAddressToClassic' {publicIp} -> publicIp) (\s@RestoreAddressToClassic' {} a -> s {publicIp = a} :: RestoreAddressToClassic)

instance Core.AWSRequest RestoreAddressToClassic where
  type
    AWSResponse RestoreAddressToClassic =
      RestoreAddressToClassicResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          RestoreAddressToClassicResponse'
            Prelude.<$> (x Data..@? "status")
            Prelude.<*> (x Data..@? "publicIp")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RestoreAddressToClassic where
  hashWithSalt _salt RestoreAddressToClassic' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` publicIp

instance Prelude.NFData RestoreAddressToClassic where
  rnf RestoreAddressToClassic' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf publicIp

instance Data.ToHeaders RestoreAddressToClassic where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath RestoreAddressToClassic where
  toPath = Prelude.const "/"

instance Data.ToQuery RestoreAddressToClassic where
  toQuery RestoreAddressToClassic' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("RestoreAddressToClassic" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "PublicIp" Data.=: publicIp
      ]

-- | /See:/ 'newRestoreAddressToClassicResponse' smart constructor.
data RestoreAddressToClassicResponse = RestoreAddressToClassicResponse'
  { -- | The move status for the IP address.
    status :: Prelude.Maybe AddressStatus,
    -- | The Elastic IP address.
    publicIp :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreAddressToClassicResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'restoreAddressToClassicResponse_status' - The move status for the IP address.
--
-- 'publicIp', 'restoreAddressToClassicResponse_publicIp' - The Elastic IP address.
--
-- 'httpStatus', 'restoreAddressToClassicResponse_httpStatus' - The response's http status code.
newRestoreAddressToClassicResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RestoreAddressToClassicResponse
newRestoreAddressToClassicResponse pHttpStatus_ =
  RestoreAddressToClassicResponse'
    { status =
        Prelude.Nothing,
      publicIp = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The move status for the IP address.
restoreAddressToClassicResponse_status :: Lens.Lens' RestoreAddressToClassicResponse (Prelude.Maybe AddressStatus)
restoreAddressToClassicResponse_status = Lens.lens (\RestoreAddressToClassicResponse' {status} -> status) (\s@RestoreAddressToClassicResponse' {} a -> s {status = a} :: RestoreAddressToClassicResponse)

-- | The Elastic IP address.
restoreAddressToClassicResponse_publicIp :: Lens.Lens' RestoreAddressToClassicResponse (Prelude.Maybe Prelude.Text)
restoreAddressToClassicResponse_publicIp = Lens.lens (\RestoreAddressToClassicResponse' {publicIp} -> publicIp) (\s@RestoreAddressToClassicResponse' {} a -> s {publicIp = a} :: RestoreAddressToClassicResponse)

-- | The response's http status code.
restoreAddressToClassicResponse_httpStatus :: Lens.Lens' RestoreAddressToClassicResponse Prelude.Int
restoreAddressToClassicResponse_httpStatus = Lens.lens (\RestoreAddressToClassicResponse' {httpStatus} -> httpStatus) (\s@RestoreAddressToClassicResponse' {} a -> s {httpStatus = a} :: RestoreAddressToClassicResponse)

instance
  Prelude.NFData
    RestoreAddressToClassicResponse
  where
  rnf RestoreAddressToClassicResponse' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf publicIp
      `Prelude.seq` Prelude.rnf httpStatus
