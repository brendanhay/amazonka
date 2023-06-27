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
-- Module      : Amazonka.QuickSight.Types.VPCConnectionSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.VPCConnectionSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.NetworkInterface
import Amazonka.QuickSight.Types.VPCConnectionAvailabilityStatus
import Amazonka.QuickSight.Types.VPCConnectionResourceStatus

-- | The summary metadata that describes a VPC connection.
--
-- /See:/ 'newVPCConnectionSummary' smart constructor.
data VPCConnectionSummary = VPCConnectionSummary'
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
-- Create a value of 'VPCConnectionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'vPCConnectionSummary_arn' - The Amazon Resource Name (ARN) of the VPC connection.
--
-- 'availabilityStatus', 'vPCConnectionSummary_availabilityStatus' - The availability status of the VPC connection.
--
-- 'createdTime', 'vPCConnectionSummary_createdTime' - The time that the VPC connection was created.
--
-- 'dnsResolvers', 'vPCConnectionSummary_dnsResolvers' - A list of IP addresses of DNS resolver endpoints for the VPC connection.
--
-- 'lastUpdatedTime', 'vPCConnectionSummary_lastUpdatedTime' - The time that the VPC connection was last updated.
--
-- 'name', 'vPCConnectionSummary_name' - The display name for the VPC connection.
--
-- 'networkInterfaces', 'vPCConnectionSummary_networkInterfaces' - A list of network interfaces.
--
-- 'roleArn', 'vPCConnectionSummary_roleArn' - The ARN of the IAM role associated with the VPC connection.
--
-- 'securityGroupIds', 'vPCConnectionSummary_securityGroupIds' - The Amazon EC2 security group IDs associated with the VPC connection.
--
-- 'status', 'vPCConnectionSummary_status' - The status of the VPC connection.
--
-- 'vPCConnectionId', 'vPCConnectionSummary_vPCConnectionId' - The ID of the VPC connection that you\'re creating. This ID is a unique
-- identifier for each Amazon Web Services Region in an Amazon Web Services
-- account.
--
-- 'vPCId', 'vPCConnectionSummary_vPCId' - The Amazon EC2 VPC ID associated with the VPC connection.
newVPCConnectionSummary ::
  VPCConnectionSummary
newVPCConnectionSummary =
  VPCConnectionSummary'
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
vPCConnectionSummary_arn :: Lens.Lens' VPCConnectionSummary (Prelude.Maybe Prelude.Text)
vPCConnectionSummary_arn = Lens.lens (\VPCConnectionSummary' {arn} -> arn) (\s@VPCConnectionSummary' {} a -> s {arn = a} :: VPCConnectionSummary)

-- | The availability status of the VPC connection.
vPCConnectionSummary_availabilityStatus :: Lens.Lens' VPCConnectionSummary (Prelude.Maybe VPCConnectionAvailabilityStatus)
vPCConnectionSummary_availabilityStatus = Lens.lens (\VPCConnectionSummary' {availabilityStatus} -> availabilityStatus) (\s@VPCConnectionSummary' {} a -> s {availabilityStatus = a} :: VPCConnectionSummary)

-- | The time that the VPC connection was created.
vPCConnectionSummary_createdTime :: Lens.Lens' VPCConnectionSummary (Prelude.Maybe Prelude.UTCTime)
vPCConnectionSummary_createdTime = Lens.lens (\VPCConnectionSummary' {createdTime} -> createdTime) (\s@VPCConnectionSummary' {} a -> s {createdTime = a} :: VPCConnectionSummary) Prelude.. Lens.mapping Data._Time

-- | A list of IP addresses of DNS resolver endpoints for the VPC connection.
vPCConnectionSummary_dnsResolvers :: Lens.Lens' VPCConnectionSummary (Prelude.Maybe [Prelude.Text])
vPCConnectionSummary_dnsResolvers = Lens.lens (\VPCConnectionSummary' {dnsResolvers} -> dnsResolvers) (\s@VPCConnectionSummary' {} a -> s {dnsResolvers = a} :: VPCConnectionSummary) Prelude.. Lens.mapping Lens.coerced

-- | The time that the VPC connection was last updated.
vPCConnectionSummary_lastUpdatedTime :: Lens.Lens' VPCConnectionSummary (Prelude.Maybe Prelude.UTCTime)
vPCConnectionSummary_lastUpdatedTime = Lens.lens (\VPCConnectionSummary' {lastUpdatedTime} -> lastUpdatedTime) (\s@VPCConnectionSummary' {} a -> s {lastUpdatedTime = a} :: VPCConnectionSummary) Prelude.. Lens.mapping Data._Time

-- | The display name for the VPC connection.
vPCConnectionSummary_name :: Lens.Lens' VPCConnectionSummary (Prelude.Maybe Prelude.Text)
vPCConnectionSummary_name = Lens.lens (\VPCConnectionSummary' {name} -> name) (\s@VPCConnectionSummary' {} a -> s {name = a} :: VPCConnectionSummary)

-- | A list of network interfaces.
vPCConnectionSummary_networkInterfaces :: Lens.Lens' VPCConnectionSummary (Prelude.Maybe [NetworkInterface])
vPCConnectionSummary_networkInterfaces = Lens.lens (\VPCConnectionSummary' {networkInterfaces} -> networkInterfaces) (\s@VPCConnectionSummary' {} a -> s {networkInterfaces = a} :: VPCConnectionSummary) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the IAM role associated with the VPC connection.
vPCConnectionSummary_roleArn :: Lens.Lens' VPCConnectionSummary (Prelude.Maybe Prelude.Text)
vPCConnectionSummary_roleArn = Lens.lens (\VPCConnectionSummary' {roleArn} -> roleArn) (\s@VPCConnectionSummary' {} a -> s {roleArn = a} :: VPCConnectionSummary)

-- | The Amazon EC2 security group IDs associated with the VPC connection.
vPCConnectionSummary_securityGroupIds :: Lens.Lens' VPCConnectionSummary (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
vPCConnectionSummary_securityGroupIds = Lens.lens (\VPCConnectionSummary' {securityGroupIds} -> securityGroupIds) (\s@VPCConnectionSummary' {} a -> s {securityGroupIds = a} :: VPCConnectionSummary) Prelude.. Lens.mapping Lens.coerced

-- | The status of the VPC connection.
vPCConnectionSummary_status :: Lens.Lens' VPCConnectionSummary (Prelude.Maybe VPCConnectionResourceStatus)
vPCConnectionSummary_status = Lens.lens (\VPCConnectionSummary' {status} -> status) (\s@VPCConnectionSummary' {} a -> s {status = a} :: VPCConnectionSummary)

-- | The ID of the VPC connection that you\'re creating. This ID is a unique
-- identifier for each Amazon Web Services Region in an Amazon Web Services
-- account.
vPCConnectionSummary_vPCConnectionId :: Lens.Lens' VPCConnectionSummary (Prelude.Maybe Prelude.Text)
vPCConnectionSummary_vPCConnectionId = Lens.lens (\VPCConnectionSummary' {vPCConnectionId} -> vPCConnectionId) (\s@VPCConnectionSummary' {} a -> s {vPCConnectionId = a} :: VPCConnectionSummary)

-- | The Amazon EC2 VPC ID associated with the VPC connection.
vPCConnectionSummary_vPCId :: Lens.Lens' VPCConnectionSummary (Prelude.Maybe Prelude.Text)
vPCConnectionSummary_vPCId = Lens.lens (\VPCConnectionSummary' {vPCId} -> vPCId) (\s@VPCConnectionSummary' {} a -> s {vPCId = a} :: VPCConnectionSummary)

instance Data.FromJSON VPCConnectionSummary where
  parseJSON =
    Data.withObject
      "VPCConnectionSummary"
      ( \x ->
          VPCConnectionSummary'
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

instance Prelude.Hashable VPCConnectionSummary where
  hashWithSalt _salt VPCConnectionSummary' {..} =
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

instance Prelude.NFData VPCConnectionSummary where
  rnf VPCConnectionSummary' {..} =
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
