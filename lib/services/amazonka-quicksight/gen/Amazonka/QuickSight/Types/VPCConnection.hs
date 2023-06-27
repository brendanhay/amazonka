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
-- Module      : Amazonka.QuickSight.Types.VPCConnection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.VPCConnection where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.NetworkInterface
import Amazonka.QuickSight.Types.VPCConnectionAvailabilityStatus
import Amazonka.QuickSight.Types.VPCConnectionResourceStatus

-- | The structure of a VPC connection.
--
-- /See:/ 'newVPCConnection' smart constructor.
data VPCConnection = VPCConnection'
  { -- | The Amazon Resource Name (ARN) of the VPC connection.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The availability status of the VPC connection.
    availabilityStatus :: Prelude.Maybe VPCConnectionAvailabilityStatus,
    -- | The time that the VPC connection was created.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | A list of IP addresses of DNS resolver endpoints for the VPC connection.
    dnsResolvers :: Prelude.Maybe [Prelude.Text],
    -- | The time that the VPC connection was last updated.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    -- | The display name for the VPC connection.
    name :: Prelude.Maybe Prelude.Text,
    -- | A list of network interfaces.
    networkInterfaces :: Prelude.Maybe [NetworkInterface],
    -- | The ARN of the IAM role associated with the VPC connection.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon EC2 security group IDs associated with the VPC connection.
    securityGroupIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The status of the VPC connection.
    status :: Prelude.Maybe VPCConnectionResourceStatus,
    -- | The ID of the VPC connection that you\'re creating. This ID is a unique
    -- identifier for each Amazon Web Services Region in an Amazon Web Services
    -- account.
    vPCConnectionId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon EC2 VPC ID associated with the VPC connection.
    vPCId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VPCConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'vPCConnection_arn' - The Amazon Resource Name (ARN) of the VPC connection.
--
-- 'availabilityStatus', 'vPCConnection_availabilityStatus' - The availability status of the VPC connection.
--
-- 'createdTime', 'vPCConnection_createdTime' - The time that the VPC connection was created.
--
-- 'dnsResolvers', 'vPCConnection_dnsResolvers' - A list of IP addresses of DNS resolver endpoints for the VPC connection.
--
-- 'lastUpdatedTime', 'vPCConnection_lastUpdatedTime' - The time that the VPC connection was last updated.
--
-- 'name', 'vPCConnection_name' - The display name for the VPC connection.
--
-- 'networkInterfaces', 'vPCConnection_networkInterfaces' - A list of network interfaces.
--
-- 'roleArn', 'vPCConnection_roleArn' - The ARN of the IAM role associated with the VPC connection.
--
-- 'securityGroupIds', 'vPCConnection_securityGroupIds' - The Amazon EC2 security group IDs associated with the VPC connection.
--
-- 'status', 'vPCConnection_status' - The status of the VPC connection.
--
-- 'vPCConnectionId', 'vPCConnection_vPCConnectionId' - The ID of the VPC connection that you\'re creating. This ID is a unique
-- identifier for each Amazon Web Services Region in an Amazon Web Services
-- account.
--
-- 'vPCId', 'vPCConnection_vPCId' - The Amazon EC2 VPC ID associated with the VPC connection.
newVPCConnection ::
  VPCConnection
newVPCConnection =
  VPCConnection'
    { arn = Prelude.Nothing,
      availabilityStatus = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      dnsResolvers = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      name = Prelude.Nothing,
      networkInterfaces = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      securityGroupIds = Prelude.Nothing,
      status = Prelude.Nothing,
      vPCConnectionId = Prelude.Nothing,
      vPCId = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the VPC connection.
vPCConnection_arn :: Lens.Lens' VPCConnection (Prelude.Maybe Prelude.Text)
vPCConnection_arn = Lens.lens (\VPCConnection' {arn} -> arn) (\s@VPCConnection' {} a -> s {arn = a} :: VPCConnection)

-- | The availability status of the VPC connection.
vPCConnection_availabilityStatus :: Lens.Lens' VPCConnection (Prelude.Maybe VPCConnectionAvailabilityStatus)
vPCConnection_availabilityStatus = Lens.lens (\VPCConnection' {availabilityStatus} -> availabilityStatus) (\s@VPCConnection' {} a -> s {availabilityStatus = a} :: VPCConnection)

-- | The time that the VPC connection was created.
vPCConnection_createdTime :: Lens.Lens' VPCConnection (Prelude.Maybe Prelude.UTCTime)
vPCConnection_createdTime = Lens.lens (\VPCConnection' {createdTime} -> createdTime) (\s@VPCConnection' {} a -> s {createdTime = a} :: VPCConnection) Prelude.. Lens.mapping Data._Time

-- | A list of IP addresses of DNS resolver endpoints for the VPC connection.
vPCConnection_dnsResolvers :: Lens.Lens' VPCConnection (Prelude.Maybe [Prelude.Text])
vPCConnection_dnsResolvers = Lens.lens (\VPCConnection' {dnsResolvers} -> dnsResolvers) (\s@VPCConnection' {} a -> s {dnsResolvers = a} :: VPCConnection) Prelude.. Lens.mapping Lens.coerced

-- | The time that the VPC connection was last updated.
vPCConnection_lastUpdatedTime :: Lens.Lens' VPCConnection (Prelude.Maybe Prelude.UTCTime)
vPCConnection_lastUpdatedTime = Lens.lens (\VPCConnection' {lastUpdatedTime} -> lastUpdatedTime) (\s@VPCConnection' {} a -> s {lastUpdatedTime = a} :: VPCConnection) Prelude.. Lens.mapping Data._Time

-- | The display name for the VPC connection.
vPCConnection_name :: Lens.Lens' VPCConnection (Prelude.Maybe Prelude.Text)
vPCConnection_name = Lens.lens (\VPCConnection' {name} -> name) (\s@VPCConnection' {} a -> s {name = a} :: VPCConnection)

-- | A list of network interfaces.
vPCConnection_networkInterfaces :: Lens.Lens' VPCConnection (Prelude.Maybe [NetworkInterface])
vPCConnection_networkInterfaces = Lens.lens (\VPCConnection' {networkInterfaces} -> networkInterfaces) (\s@VPCConnection' {} a -> s {networkInterfaces = a} :: VPCConnection) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the IAM role associated with the VPC connection.
vPCConnection_roleArn :: Lens.Lens' VPCConnection (Prelude.Maybe Prelude.Text)
vPCConnection_roleArn = Lens.lens (\VPCConnection' {roleArn} -> roleArn) (\s@VPCConnection' {} a -> s {roleArn = a} :: VPCConnection)

-- | The Amazon EC2 security group IDs associated with the VPC connection.
vPCConnection_securityGroupIds :: Lens.Lens' VPCConnection (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
vPCConnection_securityGroupIds = Lens.lens (\VPCConnection' {securityGroupIds} -> securityGroupIds) (\s@VPCConnection' {} a -> s {securityGroupIds = a} :: VPCConnection) Prelude.. Lens.mapping Lens.coerced

-- | The status of the VPC connection.
vPCConnection_status :: Lens.Lens' VPCConnection (Prelude.Maybe VPCConnectionResourceStatus)
vPCConnection_status = Lens.lens (\VPCConnection' {status} -> status) (\s@VPCConnection' {} a -> s {status = a} :: VPCConnection)

-- | The ID of the VPC connection that you\'re creating. This ID is a unique
-- identifier for each Amazon Web Services Region in an Amazon Web Services
-- account.
vPCConnection_vPCConnectionId :: Lens.Lens' VPCConnection (Prelude.Maybe Prelude.Text)
vPCConnection_vPCConnectionId = Lens.lens (\VPCConnection' {vPCConnectionId} -> vPCConnectionId) (\s@VPCConnection' {} a -> s {vPCConnectionId = a} :: VPCConnection)

-- | The Amazon EC2 VPC ID associated with the VPC connection.
vPCConnection_vPCId :: Lens.Lens' VPCConnection (Prelude.Maybe Prelude.Text)
vPCConnection_vPCId = Lens.lens (\VPCConnection' {vPCId} -> vPCId) (\s@VPCConnection' {} a -> s {vPCId = a} :: VPCConnection)

instance Data.FromJSON VPCConnection where
  parseJSON =
    Data.withObject
      "VPCConnection"
      ( \x ->
          VPCConnection'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "AvailabilityStatus")
            Prelude.<*> (x Data..:? "CreatedTime")
            Prelude.<*> (x Data..:? "DnsResolvers" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "LastUpdatedTime")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> ( x
                            Data..:? "NetworkInterfaces"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "RoleArn")
            Prelude.<*> (x Data..:? "SecurityGroupIds")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "VPCConnectionId")
            Prelude.<*> (x Data..:? "VPCId")
      )

instance Prelude.Hashable VPCConnection where
  hashWithSalt _salt VPCConnection' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` availabilityStatus
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` dnsResolvers
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` networkInterfaces
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` vPCConnectionId
      `Prelude.hashWithSalt` vPCId

instance Prelude.NFData VPCConnection where
  rnf VPCConnection' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf availabilityStatus
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf dnsResolvers
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf networkInterfaces
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf vPCConnectionId
      `Prelude.seq` Prelude.rnf vPCId
