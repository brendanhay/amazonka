{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.S3Outposts.Types.Endpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3Outposts.Types.Endpoint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3Outposts.Types.EndpointAccessType
import Amazonka.S3Outposts.Types.EndpointStatus
import Amazonka.S3Outposts.Types.NetworkInterface

-- | Amazon S3 on Outposts Access Points simplify managing data access at
-- scale for shared datasets in S3 on Outposts. S3 on Outposts uses
-- endpoints to connect to Outposts buckets so that you can perform actions
-- within your virtual private cloud (VPC). For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/AccessingS3Outposts.html Accessing S3 on Outposts using VPC only access points>.
--
-- /See:/ 'newEndpoint' smart constructor.
data Endpoint = Endpoint'
  { -- | The time the endpoint was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The status of the endpoint.
    status :: Prelude.Maybe EndpointStatus,
    -- | The ID of the VPC used for the endpoint.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The network interface of the endpoint.
    networkInterfaces :: Prelude.Maybe [NetworkInterface],
    -- | The ID of the AWS Outposts.
    outpostsId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the subnet used for the endpoint.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the security group used for the endpoint.
    securityGroupId :: Prelude.Maybe Prelude.Text,
    accessType :: Prelude.Maybe EndpointAccessType,
    -- | The ID of the customer-owned IPv4 pool used for the endpoint.
    customerOwnedIpv4Pool :: Prelude.Maybe Prelude.Text,
    -- | The VPC CIDR committed by this endpoint.
    cidrBlock :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the endpoint.
    endpointArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Endpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'endpoint_creationTime' - The time the endpoint was created.
--
-- 'status', 'endpoint_status' - The status of the endpoint.
--
-- 'vpcId', 'endpoint_vpcId' - The ID of the VPC used for the endpoint.
--
-- 'networkInterfaces', 'endpoint_networkInterfaces' - The network interface of the endpoint.
--
-- 'outpostsId', 'endpoint_outpostsId' - The ID of the AWS Outposts.
--
-- 'subnetId', 'endpoint_subnetId' - The ID of the subnet used for the endpoint.
--
-- 'securityGroupId', 'endpoint_securityGroupId' - The ID of the security group used for the endpoint.
--
-- 'accessType', 'endpoint_accessType' -
--
-- 'customerOwnedIpv4Pool', 'endpoint_customerOwnedIpv4Pool' - The ID of the customer-owned IPv4 pool used for the endpoint.
--
-- 'cidrBlock', 'endpoint_cidrBlock' - The VPC CIDR committed by this endpoint.
--
-- 'endpointArn', 'endpoint_endpointArn' - The Amazon Resource Name (ARN) of the endpoint.
newEndpoint ::
  Endpoint
newEndpoint =
  Endpoint'
    { creationTime = Prelude.Nothing,
      status = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      networkInterfaces = Prelude.Nothing,
      outpostsId = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      securityGroupId = Prelude.Nothing,
      accessType = Prelude.Nothing,
      customerOwnedIpv4Pool = Prelude.Nothing,
      cidrBlock = Prelude.Nothing,
      endpointArn = Prelude.Nothing
    }

-- | The time the endpoint was created.
endpoint_creationTime :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.UTCTime)
endpoint_creationTime = Lens.lens (\Endpoint' {creationTime} -> creationTime) (\s@Endpoint' {} a -> s {creationTime = a} :: Endpoint) Prelude.. Lens.mapping Core._Time

-- | The status of the endpoint.
endpoint_status :: Lens.Lens' Endpoint (Prelude.Maybe EndpointStatus)
endpoint_status = Lens.lens (\Endpoint' {status} -> status) (\s@Endpoint' {} a -> s {status = a} :: Endpoint)

-- | The ID of the VPC used for the endpoint.
endpoint_vpcId :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_vpcId = Lens.lens (\Endpoint' {vpcId} -> vpcId) (\s@Endpoint' {} a -> s {vpcId = a} :: Endpoint)

-- | The network interface of the endpoint.
endpoint_networkInterfaces :: Lens.Lens' Endpoint (Prelude.Maybe [NetworkInterface])
endpoint_networkInterfaces = Lens.lens (\Endpoint' {networkInterfaces} -> networkInterfaces) (\s@Endpoint' {} a -> s {networkInterfaces = a} :: Endpoint) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the AWS Outposts.
endpoint_outpostsId :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_outpostsId = Lens.lens (\Endpoint' {outpostsId} -> outpostsId) (\s@Endpoint' {} a -> s {outpostsId = a} :: Endpoint)

-- | The ID of the subnet used for the endpoint.
endpoint_subnetId :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_subnetId = Lens.lens (\Endpoint' {subnetId} -> subnetId) (\s@Endpoint' {} a -> s {subnetId = a} :: Endpoint)

-- | The ID of the security group used for the endpoint.
endpoint_securityGroupId :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_securityGroupId = Lens.lens (\Endpoint' {securityGroupId} -> securityGroupId) (\s@Endpoint' {} a -> s {securityGroupId = a} :: Endpoint)

-- |
endpoint_accessType :: Lens.Lens' Endpoint (Prelude.Maybe EndpointAccessType)
endpoint_accessType = Lens.lens (\Endpoint' {accessType} -> accessType) (\s@Endpoint' {} a -> s {accessType = a} :: Endpoint)

-- | The ID of the customer-owned IPv4 pool used for the endpoint.
endpoint_customerOwnedIpv4Pool :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_customerOwnedIpv4Pool = Lens.lens (\Endpoint' {customerOwnedIpv4Pool} -> customerOwnedIpv4Pool) (\s@Endpoint' {} a -> s {customerOwnedIpv4Pool = a} :: Endpoint)

-- | The VPC CIDR committed by this endpoint.
endpoint_cidrBlock :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_cidrBlock = Lens.lens (\Endpoint' {cidrBlock} -> cidrBlock) (\s@Endpoint' {} a -> s {cidrBlock = a} :: Endpoint)

-- | The Amazon Resource Name (ARN) of the endpoint.
endpoint_endpointArn :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_endpointArn = Lens.lens (\Endpoint' {endpointArn} -> endpointArn) (\s@Endpoint' {} a -> s {endpointArn = a} :: Endpoint)

instance Core.FromJSON Endpoint where
  parseJSON =
    Core.withObject
      "Endpoint"
      ( \x ->
          Endpoint'
            Prelude.<$> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "VpcId")
            Prelude.<*> ( x Core..:? "NetworkInterfaces"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "OutpostsId")
            Prelude.<*> (x Core..:? "SubnetId")
            Prelude.<*> (x Core..:? "SecurityGroupId")
            Prelude.<*> (x Core..:? "AccessType")
            Prelude.<*> (x Core..:? "CustomerOwnedIpv4Pool")
            Prelude.<*> (x Core..:? "CidrBlock")
            Prelude.<*> (x Core..:? "EndpointArn")
      )

instance Prelude.Hashable Endpoint where
  hashWithSalt _salt Endpoint' {..} =
    _salt `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` networkInterfaces
      `Prelude.hashWithSalt` outpostsId
      `Prelude.hashWithSalt` subnetId
      `Prelude.hashWithSalt` securityGroupId
      `Prelude.hashWithSalt` accessType
      `Prelude.hashWithSalt` customerOwnedIpv4Pool
      `Prelude.hashWithSalt` cidrBlock
      `Prelude.hashWithSalt` endpointArn

instance Prelude.NFData Endpoint where
  rnf Endpoint' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf networkInterfaces
      `Prelude.seq` Prelude.rnf outpostsId
      `Prelude.seq` Prelude.rnf subnetId
      `Prelude.seq` Prelude.rnf securityGroupId
      `Prelude.seq` Prelude.rnf accessType
      `Prelude.seq` Prelude.rnf customerOwnedIpv4Pool
      `Prelude.seq` Prelude.rnf cidrBlock
      `Prelude.seq` Prelude.rnf endpointArn
