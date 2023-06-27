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
-- Module      : Amazonka.VPCLattice.Types.TargetGroupSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VPCLattice.Types.TargetGroupSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.VPCLattice.Types.IpAddressType
import Amazonka.VPCLattice.Types.TargetGroupProtocol
import Amazonka.VPCLattice.Types.TargetGroupStatus
import Amazonka.VPCLattice.Types.TargetGroupType

-- | Summary information about a target group.
--
-- /See:/ 'newTargetGroupSummary' smart constructor.
data TargetGroupSummary = TargetGroupSummary'
  { -- | The ARN (Amazon Resource Name) of the target group.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the target group was created, specified in
    -- ISO-8601 format.
    createdAt :: Prelude.Maybe Data.ISO8601,
    -- | The ID of the target group.
    id :: Prelude.Maybe Prelude.Text,
    -- | The type of IP address used for the target group. The possible values
    -- are @ipv4@ and @ipv6@. This is an optional parameter. If not specified,
    -- the IP address type defaults to @ipv4@.
    ipAddressType :: Prelude.Maybe IpAddressType,
    -- | The date and time that the target group was last updated, specified in
    -- ISO-8601 format.
    lastUpdatedAt :: Prelude.Maybe Data.ISO8601,
    -- | The name of the target group.
    name :: Prelude.Maybe Prelude.Text,
    -- | The port of the target group.
    port :: Prelude.Maybe Prelude.Natural,
    -- | The protocol of the target group.
    protocol :: Prelude.Maybe TargetGroupProtocol,
    -- | The list of Amazon Resource Names (ARNs) of the service.
    serviceArns :: Prelude.Maybe [Prelude.Text],
    -- | The status.
    status :: Prelude.Maybe TargetGroupStatus,
    -- | The target group type.
    type' :: Prelude.Maybe TargetGroupType,
    -- | The ID of the VPC of the target group.
    vpcIdentifier :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TargetGroupSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'targetGroupSummary_arn' - The ARN (Amazon Resource Name) of the target group.
--
-- 'createdAt', 'targetGroupSummary_createdAt' - The date and time that the target group was created, specified in
-- ISO-8601 format.
--
-- 'id', 'targetGroupSummary_id' - The ID of the target group.
--
-- 'ipAddressType', 'targetGroupSummary_ipAddressType' - The type of IP address used for the target group. The possible values
-- are @ipv4@ and @ipv6@. This is an optional parameter. If not specified,
-- the IP address type defaults to @ipv4@.
--
-- 'lastUpdatedAt', 'targetGroupSummary_lastUpdatedAt' - The date and time that the target group was last updated, specified in
-- ISO-8601 format.
--
-- 'name', 'targetGroupSummary_name' - The name of the target group.
--
-- 'port', 'targetGroupSummary_port' - The port of the target group.
--
-- 'protocol', 'targetGroupSummary_protocol' - The protocol of the target group.
--
-- 'serviceArns', 'targetGroupSummary_serviceArns' - The list of Amazon Resource Names (ARNs) of the service.
--
-- 'status', 'targetGroupSummary_status' - The status.
--
-- 'type'', 'targetGroupSummary_type' - The target group type.
--
-- 'vpcIdentifier', 'targetGroupSummary_vpcIdentifier' - The ID of the VPC of the target group.
newTargetGroupSummary ::
  TargetGroupSummary
newTargetGroupSummary =
  TargetGroupSummary'
    { arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      id = Prelude.Nothing,
      ipAddressType = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      name = Prelude.Nothing,
      port = Prelude.Nothing,
      protocol = Prelude.Nothing,
      serviceArns = Prelude.Nothing,
      status = Prelude.Nothing,
      type' = Prelude.Nothing,
      vpcIdentifier = Prelude.Nothing
    }

-- | The ARN (Amazon Resource Name) of the target group.
targetGroupSummary_arn :: Lens.Lens' TargetGroupSummary (Prelude.Maybe Prelude.Text)
targetGroupSummary_arn = Lens.lens (\TargetGroupSummary' {arn} -> arn) (\s@TargetGroupSummary' {} a -> s {arn = a} :: TargetGroupSummary)

-- | The date and time that the target group was created, specified in
-- ISO-8601 format.
targetGroupSummary_createdAt :: Lens.Lens' TargetGroupSummary (Prelude.Maybe Prelude.UTCTime)
targetGroupSummary_createdAt = Lens.lens (\TargetGroupSummary' {createdAt} -> createdAt) (\s@TargetGroupSummary' {} a -> s {createdAt = a} :: TargetGroupSummary) Prelude.. Lens.mapping Data._Time

-- | The ID of the target group.
targetGroupSummary_id :: Lens.Lens' TargetGroupSummary (Prelude.Maybe Prelude.Text)
targetGroupSummary_id = Lens.lens (\TargetGroupSummary' {id} -> id) (\s@TargetGroupSummary' {} a -> s {id = a} :: TargetGroupSummary)

-- | The type of IP address used for the target group. The possible values
-- are @ipv4@ and @ipv6@. This is an optional parameter. If not specified,
-- the IP address type defaults to @ipv4@.
targetGroupSummary_ipAddressType :: Lens.Lens' TargetGroupSummary (Prelude.Maybe IpAddressType)
targetGroupSummary_ipAddressType = Lens.lens (\TargetGroupSummary' {ipAddressType} -> ipAddressType) (\s@TargetGroupSummary' {} a -> s {ipAddressType = a} :: TargetGroupSummary)

-- | The date and time that the target group was last updated, specified in
-- ISO-8601 format.
targetGroupSummary_lastUpdatedAt :: Lens.Lens' TargetGroupSummary (Prelude.Maybe Prelude.UTCTime)
targetGroupSummary_lastUpdatedAt = Lens.lens (\TargetGroupSummary' {lastUpdatedAt} -> lastUpdatedAt) (\s@TargetGroupSummary' {} a -> s {lastUpdatedAt = a} :: TargetGroupSummary) Prelude.. Lens.mapping Data._Time

-- | The name of the target group.
targetGroupSummary_name :: Lens.Lens' TargetGroupSummary (Prelude.Maybe Prelude.Text)
targetGroupSummary_name = Lens.lens (\TargetGroupSummary' {name} -> name) (\s@TargetGroupSummary' {} a -> s {name = a} :: TargetGroupSummary)

-- | The port of the target group.
targetGroupSummary_port :: Lens.Lens' TargetGroupSummary (Prelude.Maybe Prelude.Natural)
targetGroupSummary_port = Lens.lens (\TargetGroupSummary' {port} -> port) (\s@TargetGroupSummary' {} a -> s {port = a} :: TargetGroupSummary)

-- | The protocol of the target group.
targetGroupSummary_protocol :: Lens.Lens' TargetGroupSummary (Prelude.Maybe TargetGroupProtocol)
targetGroupSummary_protocol = Lens.lens (\TargetGroupSummary' {protocol} -> protocol) (\s@TargetGroupSummary' {} a -> s {protocol = a} :: TargetGroupSummary)

-- | The list of Amazon Resource Names (ARNs) of the service.
targetGroupSummary_serviceArns :: Lens.Lens' TargetGroupSummary (Prelude.Maybe [Prelude.Text])
targetGroupSummary_serviceArns = Lens.lens (\TargetGroupSummary' {serviceArns} -> serviceArns) (\s@TargetGroupSummary' {} a -> s {serviceArns = a} :: TargetGroupSummary) Prelude.. Lens.mapping Lens.coerced

-- | The status.
targetGroupSummary_status :: Lens.Lens' TargetGroupSummary (Prelude.Maybe TargetGroupStatus)
targetGroupSummary_status = Lens.lens (\TargetGroupSummary' {status} -> status) (\s@TargetGroupSummary' {} a -> s {status = a} :: TargetGroupSummary)

-- | The target group type.
targetGroupSummary_type :: Lens.Lens' TargetGroupSummary (Prelude.Maybe TargetGroupType)
targetGroupSummary_type = Lens.lens (\TargetGroupSummary' {type'} -> type') (\s@TargetGroupSummary' {} a -> s {type' = a} :: TargetGroupSummary)

-- | The ID of the VPC of the target group.
targetGroupSummary_vpcIdentifier :: Lens.Lens' TargetGroupSummary (Prelude.Maybe Prelude.Text)
targetGroupSummary_vpcIdentifier = Lens.lens (\TargetGroupSummary' {vpcIdentifier} -> vpcIdentifier) (\s@TargetGroupSummary' {} a -> s {vpcIdentifier = a} :: TargetGroupSummary)

instance Data.FromJSON TargetGroupSummary where
  parseJSON =
    Data.withObject
      "TargetGroupSummary"
      ( \x ->
          TargetGroupSummary'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "ipAddressType")
            Prelude.<*> (x Data..:? "lastUpdatedAt")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "port")
            Prelude.<*> (x Data..:? "protocol")
            Prelude.<*> (x Data..:? "serviceArns" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "type")
            Prelude.<*> (x Data..:? "vpcIdentifier")
      )

instance Prelude.Hashable TargetGroupSummary where
  hashWithSalt _salt TargetGroupSummary' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` ipAddressType
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` protocol
      `Prelude.hashWithSalt` serviceArns
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` vpcIdentifier

instance Prelude.NFData TargetGroupSummary where
  rnf TargetGroupSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf ipAddressType
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf serviceArns
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf vpcIdentifier
