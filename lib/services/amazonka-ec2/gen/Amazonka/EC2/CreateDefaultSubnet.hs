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
-- Module      : Amazonka.EC2.CreateDefaultSubnet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a default subnet with a size @\/20@ IPv4 CIDR block in the
-- specified Availability Zone in your default VPC. You can have only one
-- default subnet per Availability Zone. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/default-vpc.html#create-default-subnet Creating a default subnet>
-- in the /Amazon Virtual Private Cloud User Guide/.
module Amazonka.EC2.CreateDefaultSubnet
  ( -- * Creating a Request
    CreateDefaultSubnet (..),
    newCreateDefaultSubnet,

    -- * Request Lenses
    createDefaultSubnet_dryRun,
    createDefaultSubnet_ipv6Native,
    createDefaultSubnet_availabilityZone,

    -- * Destructuring the Response
    CreateDefaultSubnetResponse (..),
    newCreateDefaultSubnetResponse,

    -- * Response Lenses
    createDefaultSubnetResponse_subnet,
    createDefaultSubnetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDefaultSubnet' smart constructor.
data CreateDefaultSubnet = CreateDefaultSubnet'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether to create an IPv6 only subnet. If you already have a
    -- default subnet for this Availability Zone, you must delete it before you
    -- can create an IPv6 only subnet.
    ipv6Native :: Prelude.Maybe Prelude.Bool,
    -- | The Availability Zone in which to create the default subnet.
    availabilityZone :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDefaultSubnet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'createDefaultSubnet_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'ipv6Native', 'createDefaultSubnet_ipv6Native' - Indicates whether to create an IPv6 only subnet. If you already have a
-- default subnet for this Availability Zone, you must delete it before you
-- can create an IPv6 only subnet.
--
-- 'availabilityZone', 'createDefaultSubnet_availabilityZone' - The Availability Zone in which to create the default subnet.
newCreateDefaultSubnet ::
  -- | 'availabilityZone'
  Prelude.Text ->
  CreateDefaultSubnet
newCreateDefaultSubnet pAvailabilityZone_ =
  CreateDefaultSubnet'
    { dryRun = Prelude.Nothing,
      ipv6Native = Prelude.Nothing,
      availabilityZone = pAvailabilityZone_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createDefaultSubnet_dryRun :: Lens.Lens' CreateDefaultSubnet (Prelude.Maybe Prelude.Bool)
createDefaultSubnet_dryRun = Lens.lens (\CreateDefaultSubnet' {dryRun} -> dryRun) (\s@CreateDefaultSubnet' {} a -> s {dryRun = a} :: CreateDefaultSubnet)

-- | Indicates whether to create an IPv6 only subnet. If you already have a
-- default subnet for this Availability Zone, you must delete it before you
-- can create an IPv6 only subnet.
createDefaultSubnet_ipv6Native :: Lens.Lens' CreateDefaultSubnet (Prelude.Maybe Prelude.Bool)
createDefaultSubnet_ipv6Native = Lens.lens (\CreateDefaultSubnet' {ipv6Native} -> ipv6Native) (\s@CreateDefaultSubnet' {} a -> s {ipv6Native = a} :: CreateDefaultSubnet)

-- | The Availability Zone in which to create the default subnet.
createDefaultSubnet_availabilityZone :: Lens.Lens' CreateDefaultSubnet Prelude.Text
createDefaultSubnet_availabilityZone = Lens.lens (\CreateDefaultSubnet' {availabilityZone} -> availabilityZone) (\s@CreateDefaultSubnet' {} a -> s {availabilityZone = a} :: CreateDefaultSubnet)

instance Core.AWSRequest CreateDefaultSubnet where
  type
    AWSResponse CreateDefaultSubnet =
      CreateDefaultSubnetResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateDefaultSubnetResponse'
            Prelude.<$> (x Data..@? "subnet")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDefaultSubnet where
  hashWithSalt _salt CreateDefaultSubnet' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` ipv6Native
      `Prelude.hashWithSalt` availabilityZone

instance Prelude.NFData CreateDefaultSubnet where
  rnf CreateDefaultSubnet' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf ipv6Native
      `Prelude.seq` Prelude.rnf availabilityZone

instance Data.ToHeaders CreateDefaultSubnet where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateDefaultSubnet where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateDefaultSubnet where
  toQuery CreateDefaultSubnet' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateDefaultSubnet" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "Ipv6Native" Data.=: ipv6Native,
        "AvailabilityZone" Data.=: availabilityZone
      ]

-- | /See:/ 'newCreateDefaultSubnetResponse' smart constructor.
data CreateDefaultSubnetResponse = CreateDefaultSubnetResponse'
  { -- | Information about the subnet.
    subnet :: Prelude.Maybe Subnet,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDefaultSubnetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subnet', 'createDefaultSubnetResponse_subnet' - Information about the subnet.
--
-- 'httpStatus', 'createDefaultSubnetResponse_httpStatus' - The response's http status code.
newCreateDefaultSubnetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDefaultSubnetResponse
newCreateDefaultSubnetResponse pHttpStatus_ =
  CreateDefaultSubnetResponse'
    { subnet =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the subnet.
createDefaultSubnetResponse_subnet :: Lens.Lens' CreateDefaultSubnetResponse (Prelude.Maybe Subnet)
createDefaultSubnetResponse_subnet = Lens.lens (\CreateDefaultSubnetResponse' {subnet} -> subnet) (\s@CreateDefaultSubnetResponse' {} a -> s {subnet = a} :: CreateDefaultSubnetResponse)

-- | The response's http status code.
createDefaultSubnetResponse_httpStatus :: Lens.Lens' CreateDefaultSubnetResponse Prelude.Int
createDefaultSubnetResponse_httpStatus = Lens.lens (\CreateDefaultSubnetResponse' {httpStatus} -> httpStatus) (\s@CreateDefaultSubnetResponse' {} a -> s {httpStatus = a} :: CreateDefaultSubnetResponse)

instance Prelude.NFData CreateDefaultSubnetResponse where
  rnf CreateDefaultSubnetResponse' {..} =
    Prelude.rnf subnet
      `Prelude.seq` Prelude.rnf httpStatus
