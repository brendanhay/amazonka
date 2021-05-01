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
-- Module      : Network.AWS.EC2.ReleaseAddress
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Releases the specified Elastic IP address.
--
-- [EC2-Classic, default VPC] Releasing an Elastic IP address automatically
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
-- the address is already allocated to another AWS account.
--
-- [EC2-VPC] After you release an Elastic IP address for use in a VPC, you
-- might be able to recover it. For more information, see AllocateAddress.
module Network.AWS.EC2.ReleaseAddress
  ( -- * Creating a Request
    ReleaseAddress (..),
    newReleaseAddress,

    -- * Request Lenses
    releaseAddress_dryRun,
    releaseAddress_publicIp,
    releaseAddress_allocationId,
    releaseAddress_networkBorderGroup,

    -- * Destructuring the Response
    ReleaseAddressResponse (..),
    newReleaseAddressResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newReleaseAddress' smart constructor.
data ReleaseAddress = ReleaseAddress'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | [EC2-Classic] The Elastic IP address. Required for EC2-Classic.
    publicIp :: Prelude.Maybe Prelude.Text,
    -- | [EC2-VPC] The allocation ID. Required for EC2-VPC.
    allocationId :: Prelude.Maybe Prelude.Text,
    -- | The set of Availability Zones, Local Zones, or Wavelength Zones from
    -- which AWS advertises IP addresses.
    --
    -- If you provide an incorrect network border group, you will receive an
    -- @InvalidAddress.NotFound@ error. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html Error Codes>.
    --
    -- You cannot use a network border group with EC2 Classic. If you attempt
    -- this operation on EC2 classic, you will receive an
    -- @InvalidParameterCombination@ error. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html Error Codes>.
    networkBorderGroup :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ReleaseAddress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'releaseAddress_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'publicIp', 'releaseAddress_publicIp' - [EC2-Classic] The Elastic IP address. Required for EC2-Classic.
--
-- 'allocationId', 'releaseAddress_allocationId' - [EC2-VPC] The allocation ID. Required for EC2-VPC.
--
-- 'networkBorderGroup', 'releaseAddress_networkBorderGroup' - The set of Availability Zones, Local Zones, or Wavelength Zones from
-- which AWS advertises IP addresses.
--
-- If you provide an incorrect network border group, you will receive an
-- @InvalidAddress.NotFound@ error. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html Error Codes>.
--
-- You cannot use a network border group with EC2 Classic. If you attempt
-- this operation on EC2 classic, you will receive an
-- @InvalidParameterCombination@ error. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html Error Codes>.
newReleaseAddress ::
  ReleaseAddress
newReleaseAddress =
  ReleaseAddress'
    { dryRun = Prelude.Nothing,
      publicIp = Prelude.Nothing,
      allocationId = Prelude.Nothing,
      networkBorderGroup = Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
releaseAddress_dryRun :: Lens.Lens' ReleaseAddress (Prelude.Maybe Prelude.Bool)
releaseAddress_dryRun = Lens.lens (\ReleaseAddress' {dryRun} -> dryRun) (\s@ReleaseAddress' {} a -> s {dryRun = a} :: ReleaseAddress)

-- | [EC2-Classic] The Elastic IP address. Required for EC2-Classic.
releaseAddress_publicIp :: Lens.Lens' ReleaseAddress (Prelude.Maybe Prelude.Text)
releaseAddress_publicIp = Lens.lens (\ReleaseAddress' {publicIp} -> publicIp) (\s@ReleaseAddress' {} a -> s {publicIp = a} :: ReleaseAddress)

-- | [EC2-VPC] The allocation ID. Required for EC2-VPC.
releaseAddress_allocationId :: Lens.Lens' ReleaseAddress (Prelude.Maybe Prelude.Text)
releaseAddress_allocationId = Lens.lens (\ReleaseAddress' {allocationId} -> allocationId) (\s@ReleaseAddress' {} a -> s {allocationId = a} :: ReleaseAddress)

-- | The set of Availability Zones, Local Zones, or Wavelength Zones from
-- which AWS advertises IP addresses.
--
-- If you provide an incorrect network border group, you will receive an
-- @InvalidAddress.NotFound@ error. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html Error Codes>.
--
-- You cannot use a network border group with EC2 Classic. If you attempt
-- this operation on EC2 classic, you will receive an
-- @InvalidParameterCombination@ error. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html Error Codes>.
releaseAddress_networkBorderGroup :: Lens.Lens' ReleaseAddress (Prelude.Maybe Prelude.Text)
releaseAddress_networkBorderGroup = Lens.lens (\ReleaseAddress' {networkBorderGroup} -> networkBorderGroup) (\s@ReleaseAddress' {} a -> s {networkBorderGroup = a} :: ReleaseAddress)

instance Prelude.AWSRequest ReleaseAddress where
  type Rs ReleaseAddress = ReleaseAddressResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull ReleaseAddressResponse'

instance Prelude.Hashable ReleaseAddress

instance Prelude.NFData ReleaseAddress

instance Prelude.ToHeaders ReleaseAddress where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ReleaseAddress where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ReleaseAddress where
  toQuery ReleaseAddress' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("ReleaseAddress" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        "PublicIp" Prelude.=: publicIp,
        "AllocationId" Prelude.=: allocationId,
        "NetworkBorderGroup" Prelude.=: networkBorderGroup
      ]

-- | /See:/ 'newReleaseAddressResponse' smart constructor.
data ReleaseAddressResponse = ReleaseAddressResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ReleaseAddressResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newReleaseAddressResponse ::
  ReleaseAddressResponse
newReleaseAddressResponse = ReleaseAddressResponse'

instance Prelude.NFData ReleaseAddressResponse
