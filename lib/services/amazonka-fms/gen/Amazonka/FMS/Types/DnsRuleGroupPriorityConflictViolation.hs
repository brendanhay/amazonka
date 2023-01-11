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
-- Module      : Amazonka.FMS.Types.DnsRuleGroupPriorityConflictViolation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.DnsRuleGroupPriorityConflictViolation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A rule group that Firewall Manager tried to associate with a VPC has the
-- same priority as a rule group that\'s already associated.
--
-- /See:/ 'newDnsRuleGroupPriorityConflictViolation' smart constructor.
data DnsRuleGroupPriorityConflictViolation = DnsRuleGroupPriorityConflictViolation'
  { -- | The ID of the Firewall Manager DNS Firewall policy that was already
    -- applied to the VPC. This policy contains the rule group that\'s already
    -- associated with the VPC.
    conflictingPolicyId :: Prelude.Maybe Prelude.Text,
    -- | The priority setting of the two conflicting rule groups.
    conflictingPriority :: Prelude.Maybe Prelude.Natural,
    -- | The priorities of rule groups that are already associated with the VPC.
    -- To retry your operation, choose priority settings that aren\'t in this
    -- list for the rule groups in your new DNS Firewall policy.
    unavailablePriorities :: Prelude.Maybe [Prelude.Natural],
    -- | Information about the VPC ID.
    violationTarget :: Prelude.Maybe Prelude.Text,
    -- | A description of the violation that specifies the VPC and the rule group
    -- that\'s already associated with it.
    violationTargetDescription :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DnsRuleGroupPriorityConflictViolation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conflictingPolicyId', 'dnsRuleGroupPriorityConflictViolation_conflictingPolicyId' - The ID of the Firewall Manager DNS Firewall policy that was already
-- applied to the VPC. This policy contains the rule group that\'s already
-- associated with the VPC.
--
-- 'conflictingPriority', 'dnsRuleGroupPriorityConflictViolation_conflictingPriority' - The priority setting of the two conflicting rule groups.
--
-- 'unavailablePriorities', 'dnsRuleGroupPriorityConflictViolation_unavailablePriorities' - The priorities of rule groups that are already associated with the VPC.
-- To retry your operation, choose priority settings that aren\'t in this
-- list for the rule groups in your new DNS Firewall policy.
--
-- 'violationTarget', 'dnsRuleGroupPriorityConflictViolation_violationTarget' - Information about the VPC ID.
--
-- 'violationTargetDescription', 'dnsRuleGroupPriorityConflictViolation_violationTargetDescription' - A description of the violation that specifies the VPC and the rule group
-- that\'s already associated with it.
newDnsRuleGroupPriorityConflictViolation ::
  DnsRuleGroupPriorityConflictViolation
newDnsRuleGroupPriorityConflictViolation =
  DnsRuleGroupPriorityConflictViolation'
    { conflictingPolicyId =
        Prelude.Nothing,
      conflictingPriority =
        Prelude.Nothing,
      unavailablePriorities =
        Prelude.Nothing,
      violationTarget = Prelude.Nothing,
      violationTargetDescription =
        Prelude.Nothing
    }

-- | The ID of the Firewall Manager DNS Firewall policy that was already
-- applied to the VPC. This policy contains the rule group that\'s already
-- associated with the VPC.
dnsRuleGroupPriorityConflictViolation_conflictingPolicyId :: Lens.Lens' DnsRuleGroupPriorityConflictViolation (Prelude.Maybe Prelude.Text)
dnsRuleGroupPriorityConflictViolation_conflictingPolicyId = Lens.lens (\DnsRuleGroupPriorityConflictViolation' {conflictingPolicyId} -> conflictingPolicyId) (\s@DnsRuleGroupPriorityConflictViolation' {} a -> s {conflictingPolicyId = a} :: DnsRuleGroupPriorityConflictViolation)

-- | The priority setting of the two conflicting rule groups.
dnsRuleGroupPriorityConflictViolation_conflictingPriority :: Lens.Lens' DnsRuleGroupPriorityConflictViolation (Prelude.Maybe Prelude.Natural)
dnsRuleGroupPriorityConflictViolation_conflictingPriority = Lens.lens (\DnsRuleGroupPriorityConflictViolation' {conflictingPriority} -> conflictingPriority) (\s@DnsRuleGroupPriorityConflictViolation' {} a -> s {conflictingPriority = a} :: DnsRuleGroupPriorityConflictViolation)

-- | The priorities of rule groups that are already associated with the VPC.
-- To retry your operation, choose priority settings that aren\'t in this
-- list for the rule groups in your new DNS Firewall policy.
dnsRuleGroupPriorityConflictViolation_unavailablePriorities :: Lens.Lens' DnsRuleGroupPriorityConflictViolation (Prelude.Maybe [Prelude.Natural])
dnsRuleGroupPriorityConflictViolation_unavailablePriorities = Lens.lens (\DnsRuleGroupPriorityConflictViolation' {unavailablePriorities} -> unavailablePriorities) (\s@DnsRuleGroupPriorityConflictViolation' {} a -> s {unavailablePriorities = a} :: DnsRuleGroupPriorityConflictViolation) Prelude.. Lens.mapping Lens.coerced

-- | Information about the VPC ID.
dnsRuleGroupPriorityConflictViolation_violationTarget :: Lens.Lens' DnsRuleGroupPriorityConflictViolation (Prelude.Maybe Prelude.Text)
dnsRuleGroupPriorityConflictViolation_violationTarget = Lens.lens (\DnsRuleGroupPriorityConflictViolation' {violationTarget} -> violationTarget) (\s@DnsRuleGroupPriorityConflictViolation' {} a -> s {violationTarget = a} :: DnsRuleGroupPriorityConflictViolation)

-- | A description of the violation that specifies the VPC and the rule group
-- that\'s already associated with it.
dnsRuleGroupPriorityConflictViolation_violationTargetDescription :: Lens.Lens' DnsRuleGroupPriorityConflictViolation (Prelude.Maybe Prelude.Text)
dnsRuleGroupPriorityConflictViolation_violationTargetDescription = Lens.lens (\DnsRuleGroupPriorityConflictViolation' {violationTargetDescription} -> violationTargetDescription) (\s@DnsRuleGroupPriorityConflictViolation' {} a -> s {violationTargetDescription = a} :: DnsRuleGroupPriorityConflictViolation)

instance
  Data.FromJSON
    DnsRuleGroupPriorityConflictViolation
  where
  parseJSON =
    Data.withObject
      "DnsRuleGroupPriorityConflictViolation"
      ( \x ->
          DnsRuleGroupPriorityConflictViolation'
            Prelude.<$> (x Data..:? "ConflictingPolicyId")
            Prelude.<*> (x Data..:? "ConflictingPriority")
            Prelude.<*> ( x Data..:? "UnavailablePriorities"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ViolationTarget")
            Prelude.<*> (x Data..:? "ViolationTargetDescription")
      )

instance
  Prelude.Hashable
    DnsRuleGroupPriorityConflictViolation
  where
  hashWithSalt
    _salt
    DnsRuleGroupPriorityConflictViolation' {..} =
      _salt `Prelude.hashWithSalt` conflictingPolicyId
        `Prelude.hashWithSalt` conflictingPriority
        `Prelude.hashWithSalt` unavailablePriorities
        `Prelude.hashWithSalt` violationTarget
        `Prelude.hashWithSalt` violationTargetDescription

instance
  Prelude.NFData
    DnsRuleGroupPriorityConflictViolation
  where
  rnf DnsRuleGroupPriorityConflictViolation' {..} =
    Prelude.rnf conflictingPolicyId
      `Prelude.seq` Prelude.rnf conflictingPriority
      `Prelude.seq` Prelude.rnf unavailablePriorities
      `Prelude.seq` Prelude.rnf violationTarget
      `Prelude.seq` Prelude.rnf violationTargetDescription
