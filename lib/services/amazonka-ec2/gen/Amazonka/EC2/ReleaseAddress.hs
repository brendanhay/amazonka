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
-- Module      : Amazonka.EC2.ReleaseAddress
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Releases the specified Elastic IP address.
--
-- [Default VPC] Releasing an Elastic IP address automatically
-- disassociates it from any instance that it\'s associated with. To
-- disassociate an Elastic IP address without releasing it, use
-- DisassociateAddress.
--
-- [Nondefault VPC] You must use DisassociateAddress to disassociate the
-- Elastic IP address before you can release it. Otherwise, Amazon EC2
-- returns an error (@InvalidIPAddress.InUse@).
--
-- After releasing an Elastic IP address, it is released to the IP address
-- pool. Be sure to update your DNS records and any servers or devices that
-- communicate with the address. If you attempt to release an Elastic IP
-- address that you already released, you\'ll get an @AuthFailure@ error if
-- the address is already allocated to another Amazon Web Services account.
--
-- After you release an Elastic IP address, you might be able to recover
-- it. For more information, see AllocateAddress.
module Amazonka.EC2.ReleaseAddress
  ( -- * Creating a Request
    ReleaseAddress (..),
    newReleaseAddress,

    -- * Request Lenses
    releaseAddress_allocationId,
    releaseAddress_dryRun,
    releaseAddress_networkBorderGroup,
    releaseAddress_publicIp,

    -- * Destructuring the Response
    ReleaseAddressResponse (..),
    newReleaseAddressResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newReleaseAddress' smart constructor.
data ReleaseAddress = ReleaseAddress'
  { -- | The allocation ID. This parameter is required.
    allocationId :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The set of Availability Zones, Local Zones, or Wavelength Zones from
    -- which Amazon Web Services advertises IP addresses.
    --
    -- If you provide an incorrect network border group, you receive an
    -- @InvalidAddress.NotFound@ error.
    --
    -- You cannot use a network border group with EC2 Classic. If you attempt
    -- this operation on EC2 classic, you receive an
    -- @InvalidParameterCombination@ error.
    networkBorderGroup :: Prelude.Maybe Prelude.Text,
    -- | Deprecated.
    publicIp :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReleaseAddress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allocationId', 'releaseAddress_allocationId' - The allocation ID. This parameter is required.
--
-- 'dryRun', 'releaseAddress_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'networkBorderGroup', 'releaseAddress_networkBorderGroup' - The set of Availability Zones, Local Zones, or Wavelength Zones from
-- which Amazon Web Services advertises IP addresses.
--
-- If you provide an incorrect network border group, you receive an
-- @InvalidAddress.NotFound@ error.
--
-- You cannot use a network border group with EC2 Classic. If you attempt
-- this operation on EC2 classic, you receive an
-- @InvalidParameterCombination@ error.
--
-- 'publicIp', 'releaseAddress_publicIp' - Deprecated.
newReleaseAddress ::
  ReleaseAddress
newReleaseAddress =
  ReleaseAddress'
    { allocationId = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      networkBorderGroup = Prelude.Nothing,
      publicIp = Prelude.Nothing
    }

-- | The allocation ID. This parameter is required.
releaseAddress_allocationId :: Lens.Lens' ReleaseAddress (Prelude.Maybe Prelude.Text)
releaseAddress_allocationId = Lens.lens (\ReleaseAddress' {allocationId} -> allocationId) (\s@ReleaseAddress' {} a -> s {allocationId = a} :: ReleaseAddress)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
releaseAddress_dryRun :: Lens.Lens' ReleaseAddress (Prelude.Maybe Prelude.Bool)
releaseAddress_dryRun = Lens.lens (\ReleaseAddress' {dryRun} -> dryRun) (\s@ReleaseAddress' {} a -> s {dryRun = a} :: ReleaseAddress)

-- | The set of Availability Zones, Local Zones, or Wavelength Zones from
-- which Amazon Web Services advertises IP addresses.
--
-- If you provide an incorrect network border group, you receive an
-- @InvalidAddress.NotFound@ error.
--
-- You cannot use a network border group with EC2 Classic. If you attempt
-- this operation on EC2 classic, you receive an
-- @InvalidParameterCombination@ error.
releaseAddress_networkBorderGroup :: Lens.Lens' ReleaseAddress (Prelude.Maybe Prelude.Text)
releaseAddress_networkBorderGroup = Lens.lens (\ReleaseAddress' {networkBorderGroup} -> networkBorderGroup) (\s@ReleaseAddress' {} a -> s {networkBorderGroup = a} :: ReleaseAddress)

-- | Deprecated.
releaseAddress_publicIp :: Lens.Lens' ReleaseAddress (Prelude.Maybe Prelude.Text)
releaseAddress_publicIp = Lens.lens (\ReleaseAddress' {publicIp} -> publicIp) (\s@ReleaseAddress' {} a -> s {publicIp = a} :: ReleaseAddress)

instance Core.AWSRequest ReleaseAddress where
  type
    AWSResponse ReleaseAddress =
      ReleaseAddressResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull ReleaseAddressResponse'

instance Prelude.Hashable ReleaseAddress where
  hashWithSalt _salt ReleaseAddress' {..} =
    _salt
      `Prelude.hashWithSalt` allocationId
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` networkBorderGroup
      `Prelude.hashWithSalt` publicIp

instance Prelude.NFData ReleaseAddress where
  rnf ReleaseAddress' {..} =
    Prelude.rnf allocationId
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf networkBorderGroup
      `Prelude.seq` Prelude.rnf publicIp

instance Data.ToHeaders ReleaseAddress where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ReleaseAddress where
  toPath = Prelude.const "/"

instance Data.ToQuery ReleaseAddress where
  toQuery ReleaseAddress' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ReleaseAddress" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "AllocationId" Data.=: allocationId,
        "DryRun" Data.=: dryRun,
        "NetworkBorderGroup" Data.=: networkBorderGroup,
        "PublicIp" Data.=: publicIp
      ]

-- | /See:/ 'newReleaseAddressResponse' smart constructor.
data ReleaseAddressResponse = ReleaseAddressResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReleaseAddressResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newReleaseAddressResponse ::
  ReleaseAddressResponse
newReleaseAddressResponse = ReleaseAddressResponse'

instance Prelude.NFData ReleaseAddressResponse where
  rnf _ = ()
