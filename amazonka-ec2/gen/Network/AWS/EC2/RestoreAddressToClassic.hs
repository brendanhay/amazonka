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
-- Module      : Network.AWS.EC2.RestoreAddressToClassic
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores an Elastic IP address that was previously moved to the EC2-VPC
-- platform back to the EC2-Classic platform. You cannot move an Elastic IP
-- address that was originally allocated for use in EC2-VPC. The Elastic IP
-- address must not be associated with an instance or network interface.
module Network.AWS.EC2.RestoreAddressToClassic
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

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRestoreAddressToClassic' smart constructor.
data RestoreAddressToClassic = RestoreAddressToClassic'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The Elastic IP address.
    publicIp :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  RestoreAddressToClassic
newRestoreAddressToClassic pPublicIp_ =
  RestoreAddressToClassic'
    { dryRun = Core.Nothing,
      publicIp = pPublicIp_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
restoreAddressToClassic_dryRun :: Lens.Lens' RestoreAddressToClassic (Core.Maybe Core.Bool)
restoreAddressToClassic_dryRun = Lens.lens (\RestoreAddressToClassic' {dryRun} -> dryRun) (\s@RestoreAddressToClassic' {} a -> s {dryRun = a} :: RestoreAddressToClassic)

-- | The Elastic IP address.
restoreAddressToClassic_publicIp :: Lens.Lens' RestoreAddressToClassic Core.Text
restoreAddressToClassic_publicIp = Lens.lens (\RestoreAddressToClassic' {publicIp} -> publicIp) (\s@RestoreAddressToClassic' {} a -> s {publicIp = a} :: RestoreAddressToClassic)

instance Core.AWSRequest RestoreAddressToClassic where
  type
    AWSResponse RestoreAddressToClassic =
      RestoreAddressToClassicResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          RestoreAddressToClassicResponse'
            Core.<$> (x Core..@? "status")
            Core.<*> (x Core..@? "publicIp")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable RestoreAddressToClassic

instance Core.NFData RestoreAddressToClassic

instance Core.ToHeaders RestoreAddressToClassic where
  toHeaders = Core.const Core.mempty

instance Core.ToPath RestoreAddressToClassic where
  toPath = Core.const "/"

instance Core.ToQuery RestoreAddressToClassic where
  toQuery RestoreAddressToClassic' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("RestoreAddressToClassic" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "PublicIp" Core.=: publicIp
      ]

-- | /See:/ 'newRestoreAddressToClassicResponse' smart constructor.
data RestoreAddressToClassicResponse = RestoreAddressToClassicResponse'
  { -- | The move status for the IP address.
    status :: Core.Maybe AddressStatus,
    -- | The Elastic IP address.
    publicIp :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  RestoreAddressToClassicResponse
newRestoreAddressToClassicResponse pHttpStatus_ =
  RestoreAddressToClassicResponse'
    { status =
        Core.Nothing,
      publicIp = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The move status for the IP address.
restoreAddressToClassicResponse_status :: Lens.Lens' RestoreAddressToClassicResponse (Core.Maybe AddressStatus)
restoreAddressToClassicResponse_status = Lens.lens (\RestoreAddressToClassicResponse' {status} -> status) (\s@RestoreAddressToClassicResponse' {} a -> s {status = a} :: RestoreAddressToClassicResponse)

-- | The Elastic IP address.
restoreAddressToClassicResponse_publicIp :: Lens.Lens' RestoreAddressToClassicResponse (Core.Maybe Core.Text)
restoreAddressToClassicResponse_publicIp = Lens.lens (\RestoreAddressToClassicResponse' {publicIp} -> publicIp) (\s@RestoreAddressToClassicResponse' {} a -> s {publicIp = a} :: RestoreAddressToClassicResponse)

-- | The response's http status code.
restoreAddressToClassicResponse_httpStatus :: Lens.Lens' RestoreAddressToClassicResponse Core.Int
restoreAddressToClassicResponse_httpStatus = Lens.lens (\RestoreAddressToClassicResponse' {httpStatus} -> httpStatus) (\s@RestoreAddressToClassicResponse' {} a -> s {httpStatus = a} :: RestoreAddressToClassicResponse)

instance Core.NFData RestoreAddressToClassicResponse
