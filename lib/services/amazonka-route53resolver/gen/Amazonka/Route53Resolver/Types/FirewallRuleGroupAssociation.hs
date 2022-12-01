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
-- Module      : Amazonka.Route53Resolver.Types.FirewallRuleGroupAssociation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Resolver.Types.FirewallRuleGroupAssociation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53Resolver.Types.FirewallRuleGroupAssociationStatus
import Amazonka.Route53Resolver.Types.MutationProtectionStatus

-- | An association between a firewall rule group and a VPC, which enables
-- DNS filtering for the VPC.
--
-- /See:/ 'newFirewallRuleGroupAssociation' smart constructor.
data FirewallRuleGroupAssociation = FirewallRuleGroupAssociation'
  { -- | The name of the association.
    name :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the association was last modified, in Unix time
    -- format and Coordinated Universal Time (UTC).
    modificationTime :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the firewall rule group.
    firewallRuleGroupId :: Prelude.Maybe Prelude.Text,
    -- | The owner of the association, used only for associations that are not
    -- managed by you. If you use Firewall Manager to manage your DNS
    -- Firewalls, then this reports Firewall Manager as the managed owner.
    managedOwnerName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the firewall rule group association.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The current status of the association.
    status :: Prelude.Maybe FirewallRuleGroupAssociationStatus,
    -- | The identifier for the association.
    id :: Prelude.Maybe Prelude.Text,
    -- | If enabled, this setting disallows modification or removal of the
    -- association, to help prevent against accidentally altering DNS firewall
    -- protections.
    mutationProtection :: Prelude.Maybe MutationProtectionStatus,
    -- | A unique string defined by you to identify the request. This allows you
    -- to retry failed requests without the risk of running the operation
    -- twice. This can be any unique string, for example, a timestamp.
    creatorRequestId :: Prelude.Maybe Prelude.Text,
    -- | The setting that determines the processing order of the rule group among
    -- the rule groups that are associated with a single VPC. DNS Firewall
    -- filters VPC traffic starting from rule group with the lowest numeric
    -- priority setting.
    priority :: Prelude.Maybe Prelude.Int,
    -- | The date and time that the association was created, in Unix time format
    -- and Coordinated Universal Time (UTC).
    creationTime :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the VPC that is associated with the rule group.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | Additional information about the status of the response, if available.
    statusMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FirewallRuleGroupAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'firewallRuleGroupAssociation_name' - The name of the association.
--
-- 'modificationTime', 'firewallRuleGroupAssociation_modificationTime' - The date and time that the association was last modified, in Unix time
-- format and Coordinated Universal Time (UTC).
--
-- 'firewallRuleGroupId', 'firewallRuleGroupAssociation_firewallRuleGroupId' - The unique identifier of the firewall rule group.
--
-- 'managedOwnerName', 'firewallRuleGroupAssociation_managedOwnerName' - The owner of the association, used only for associations that are not
-- managed by you. If you use Firewall Manager to manage your DNS
-- Firewalls, then this reports Firewall Manager as the managed owner.
--
-- 'arn', 'firewallRuleGroupAssociation_arn' - The Amazon Resource Name (ARN) of the firewall rule group association.
--
-- 'status', 'firewallRuleGroupAssociation_status' - The current status of the association.
--
-- 'id', 'firewallRuleGroupAssociation_id' - The identifier for the association.
--
-- 'mutationProtection', 'firewallRuleGroupAssociation_mutationProtection' - If enabled, this setting disallows modification or removal of the
-- association, to help prevent against accidentally altering DNS firewall
-- protections.
--
-- 'creatorRequestId', 'firewallRuleGroupAssociation_creatorRequestId' - A unique string defined by you to identify the request. This allows you
-- to retry failed requests without the risk of running the operation
-- twice. This can be any unique string, for example, a timestamp.
--
-- 'priority', 'firewallRuleGroupAssociation_priority' - The setting that determines the processing order of the rule group among
-- the rule groups that are associated with a single VPC. DNS Firewall
-- filters VPC traffic starting from rule group with the lowest numeric
-- priority setting.
--
-- 'creationTime', 'firewallRuleGroupAssociation_creationTime' - The date and time that the association was created, in Unix time format
-- and Coordinated Universal Time (UTC).
--
-- 'vpcId', 'firewallRuleGroupAssociation_vpcId' - The unique identifier of the VPC that is associated with the rule group.
--
-- 'statusMessage', 'firewallRuleGroupAssociation_statusMessage' - Additional information about the status of the response, if available.
newFirewallRuleGroupAssociation ::
  FirewallRuleGroupAssociation
newFirewallRuleGroupAssociation =
  FirewallRuleGroupAssociation'
    { name =
        Prelude.Nothing,
      modificationTime = Prelude.Nothing,
      firewallRuleGroupId = Prelude.Nothing,
      managedOwnerName = Prelude.Nothing,
      arn = Prelude.Nothing,
      status = Prelude.Nothing,
      id = Prelude.Nothing,
      mutationProtection = Prelude.Nothing,
      creatorRequestId = Prelude.Nothing,
      priority = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      statusMessage = Prelude.Nothing
    }

-- | The name of the association.
firewallRuleGroupAssociation_name :: Lens.Lens' FirewallRuleGroupAssociation (Prelude.Maybe Prelude.Text)
firewallRuleGroupAssociation_name = Lens.lens (\FirewallRuleGroupAssociation' {name} -> name) (\s@FirewallRuleGroupAssociation' {} a -> s {name = a} :: FirewallRuleGroupAssociation)

-- | The date and time that the association was last modified, in Unix time
-- format and Coordinated Universal Time (UTC).
firewallRuleGroupAssociation_modificationTime :: Lens.Lens' FirewallRuleGroupAssociation (Prelude.Maybe Prelude.Text)
firewallRuleGroupAssociation_modificationTime = Lens.lens (\FirewallRuleGroupAssociation' {modificationTime} -> modificationTime) (\s@FirewallRuleGroupAssociation' {} a -> s {modificationTime = a} :: FirewallRuleGroupAssociation)

-- | The unique identifier of the firewall rule group.
firewallRuleGroupAssociation_firewallRuleGroupId :: Lens.Lens' FirewallRuleGroupAssociation (Prelude.Maybe Prelude.Text)
firewallRuleGroupAssociation_firewallRuleGroupId = Lens.lens (\FirewallRuleGroupAssociation' {firewallRuleGroupId} -> firewallRuleGroupId) (\s@FirewallRuleGroupAssociation' {} a -> s {firewallRuleGroupId = a} :: FirewallRuleGroupAssociation)

-- | The owner of the association, used only for associations that are not
-- managed by you. If you use Firewall Manager to manage your DNS
-- Firewalls, then this reports Firewall Manager as the managed owner.
firewallRuleGroupAssociation_managedOwnerName :: Lens.Lens' FirewallRuleGroupAssociation (Prelude.Maybe Prelude.Text)
firewallRuleGroupAssociation_managedOwnerName = Lens.lens (\FirewallRuleGroupAssociation' {managedOwnerName} -> managedOwnerName) (\s@FirewallRuleGroupAssociation' {} a -> s {managedOwnerName = a} :: FirewallRuleGroupAssociation)

-- | The Amazon Resource Name (ARN) of the firewall rule group association.
firewallRuleGroupAssociation_arn :: Lens.Lens' FirewallRuleGroupAssociation (Prelude.Maybe Prelude.Text)
firewallRuleGroupAssociation_arn = Lens.lens (\FirewallRuleGroupAssociation' {arn} -> arn) (\s@FirewallRuleGroupAssociation' {} a -> s {arn = a} :: FirewallRuleGroupAssociation)

-- | The current status of the association.
firewallRuleGroupAssociation_status :: Lens.Lens' FirewallRuleGroupAssociation (Prelude.Maybe FirewallRuleGroupAssociationStatus)
firewallRuleGroupAssociation_status = Lens.lens (\FirewallRuleGroupAssociation' {status} -> status) (\s@FirewallRuleGroupAssociation' {} a -> s {status = a} :: FirewallRuleGroupAssociation)

-- | The identifier for the association.
firewallRuleGroupAssociation_id :: Lens.Lens' FirewallRuleGroupAssociation (Prelude.Maybe Prelude.Text)
firewallRuleGroupAssociation_id = Lens.lens (\FirewallRuleGroupAssociation' {id} -> id) (\s@FirewallRuleGroupAssociation' {} a -> s {id = a} :: FirewallRuleGroupAssociation)

-- | If enabled, this setting disallows modification or removal of the
-- association, to help prevent against accidentally altering DNS firewall
-- protections.
firewallRuleGroupAssociation_mutationProtection :: Lens.Lens' FirewallRuleGroupAssociation (Prelude.Maybe MutationProtectionStatus)
firewallRuleGroupAssociation_mutationProtection = Lens.lens (\FirewallRuleGroupAssociation' {mutationProtection} -> mutationProtection) (\s@FirewallRuleGroupAssociation' {} a -> s {mutationProtection = a} :: FirewallRuleGroupAssociation)

-- | A unique string defined by you to identify the request. This allows you
-- to retry failed requests without the risk of running the operation
-- twice. This can be any unique string, for example, a timestamp.
firewallRuleGroupAssociation_creatorRequestId :: Lens.Lens' FirewallRuleGroupAssociation (Prelude.Maybe Prelude.Text)
firewallRuleGroupAssociation_creatorRequestId = Lens.lens (\FirewallRuleGroupAssociation' {creatorRequestId} -> creatorRequestId) (\s@FirewallRuleGroupAssociation' {} a -> s {creatorRequestId = a} :: FirewallRuleGroupAssociation)

-- | The setting that determines the processing order of the rule group among
-- the rule groups that are associated with a single VPC. DNS Firewall
-- filters VPC traffic starting from rule group with the lowest numeric
-- priority setting.
firewallRuleGroupAssociation_priority :: Lens.Lens' FirewallRuleGroupAssociation (Prelude.Maybe Prelude.Int)
firewallRuleGroupAssociation_priority = Lens.lens (\FirewallRuleGroupAssociation' {priority} -> priority) (\s@FirewallRuleGroupAssociation' {} a -> s {priority = a} :: FirewallRuleGroupAssociation)

-- | The date and time that the association was created, in Unix time format
-- and Coordinated Universal Time (UTC).
firewallRuleGroupAssociation_creationTime :: Lens.Lens' FirewallRuleGroupAssociation (Prelude.Maybe Prelude.Text)
firewallRuleGroupAssociation_creationTime = Lens.lens (\FirewallRuleGroupAssociation' {creationTime} -> creationTime) (\s@FirewallRuleGroupAssociation' {} a -> s {creationTime = a} :: FirewallRuleGroupAssociation)

-- | The unique identifier of the VPC that is associated with the rule group.
firewallRuleGroupAssociation_vpcId :: Lens.Lens' FirewallRuleGroupAssociation (Prelude.Maybe Prelude.Text)
firewallRuleGroupAssociation_vpcId = Lens.lens (\FirewallRuleGroupAssociation' {vpcId} -> vpcId) (\s@FirewallRuleGroupAssociation' {} a -> s {vpcId = a} :: FirewallRuleGroupAssociation)

-- | Additional information about the status of the response, if available.
firewallRuleGroupAssociation_statusMessage :: Lens.Lens' FirewallRuleGroupAssociation (Prelude.Maybe Prelude.Text)
firewallRuleGroupAssociation_statusMessage = Lens.lens (\FirewallRuleGroupAssociation' {statusMessage} -> statusMessage) (\s@FirewallRuleGroupAssociation' {} a -> s {statusMessage = a} :: FirewallRuleGroupAssociation)

instance Core.FromJSON FirewallRuleGroupAssociation where
  parseJSON =
    Core.withObject
      "FirewallRuleGroupAssociation"
      ( \x ->
          FirewallRuleGroupAssociation'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "ModificationTime")
            Prelude.<*> (x Core..:? "FirewallRuleGroupId")
            Prelude.<*> (x Core..:? "ManagedOwnerName")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "MutationProtection")
            Prelude.<*> (x Core..:? "CreatorRequestId")
            Prelude.<*> (x Core..:? "Priority")
            Prelude.<*> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "VpcId")
            Prelude.<*> (x Core..:? "StatusMessage")
      )

instance
  Prelude.Hashable
    FirewallRuleGroupAssociation
  where
  hashWithSalt _salt FirewallRuleGroupAssociation' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` modificationTime
      `Prelude.hashWithSalt` firewallRuleGroupId
      `Prelude.hashWithSalt` managedOwnerName
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` mutationProtection
      `Prelude.hashWithSalt` creatorRequestId
      `Prelude.hashWithSalt` priority
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` statusMessage

instance Prelude.NFData FirewallRuleGroupAssociation where
  rnf FirewallRuleGroupAssociation' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf modificationTime
      `Prelude.seq` Prelude.rnf firewallRuleGroupId
      `Prelude.seq` Prelude.rnf managedOwnerName
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf mutationProtection
      `Prelude.seq` Prelude.rnf creatorRequestId
      `Prelude.seq` Prelude.rnf priority
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf statusMessage
