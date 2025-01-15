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
-- Module      : Amazonka.FMS.Types.DnsRuleGroupLimitExceededViolation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.DnsRuleGroupLimitExceededViolation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The VPC that Firewall Manager was applying a DNS Fireall policy to
-- reached the limit for associated DNS Firewall rule groups. Firewall
-- Manager tried to associate another rule group with the VPC and failed
-- due to the limit.
--
-- /See:/ 'newDnsRuleGroupLimitExceededViolation' smart constructor.
data DnsRuleGroupLimitExceededViolation = DnsRuleGroupLimitExceededViolation'
  { -- | The number of rule groups currently associated with the VPC.
    numberOfRuleGroupsAlreadyAssociated :: Prelude.Maybe Prelude.Int,
    -- | Information about the VPC ID.
    violationTarget :: Prelude.Maybe Prelude.Text,
    -- | A description of the violation that specifies the rule group and VPC.
    violationTargetDescription :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DnsRuleGroupLimitExceededViolation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'numberOfRuleGroupsAlreadyAssociated', 'dnsRuleGroupLimitExceededViolation_numberOfRuleGroupsAlreadyAssociated' - The number of rule groups currently associated with the VPC.
--
-- 'violationTarget', 'dnsRuleGroupLimitExceededViolation_violationTarget' - Information about the VPC ID.
--
-- 'violationTargetDescription', 'dnsRuleGroupLimitExceededViolation_violationTargetDescription' - A description of the violation that specifies the rule group and VPC.
newDnsRuleGroupLimitExceededViolation ::
  DnsRuleGroupLimitExceededViolation
newDnsRuleGroupLimitExceededViolation =
  DnsRuleGroupLimitExceededViolation'
    { numberOfRuleGroupsAlreadyAssociated =
        Prelude.Nothing,
      violationTarget = Prelude.Nothing,
      violationTargetDescription =
        Prelude.Nothing
    }

-- | The number of rule groups currently associated with the VPC.
dnsRuleGroupLimitExceededViolation_numberOfRuleGroupsAlreadyAssociated :: Lens.Lens' DnsRuleGroupLimitExceededViolation (Prelude.Maybe Prelude.Int)
dnsRuleGroupLimitExceededViolation_numberOfRuleGroupsAlreadyAssociated = Lens.lens (\DnsRuleGroupLimitExceededViolation' {numberOfRuleGroupsAlreadyAssociated} -> numberOfRuleGroupsAlreadyAssociated) (\s@DnsRuleGroupLimitExceededViolation' {} a -> s {numberOfRuleGroupsAlreadyAssociated = a} :: DnsRuleGroupLimitExceededViolation)

-- | Information about the VPC ID.
dnsRuleGroupLimitExceededViolation_violationTarget :: Lens.Lens' DnsRuleGroupLimitExceededViolation (Prelude.Maybe Prelude.Text)
dnsRuleGroupLimitExceededViolation_violationTarget = Lens.lens (\DnsRuleGroupLimitExceededViolation' {violationTarget} -> violationTarget) (\s@DnsRuleGroupLimitExceededViolation' {} a -> s {violationTarget = a} :: DnsRuleGroupLimitExceededViolation)

-- | A description of the violation that specifies the rule group and VPC.
dnsRuleGroupLimitExceededViolation_violationTargetDescription :: Lens.Lens' DnsRuleGroupLimitExceededViolation (Prelude.Maybe Prelude.Text)
dnsRuleGroupLimitExceededViolation_violationTargetDescription = Lens.lens (\DnsRuleGroupLimitExceededViolation' {violationTargetDescription} -> violationTargetDescription) (\s@DnsRuleGroupLimitExceededViolation' {} a -> s {violationTargetDescription = a} :: DnsRuleGroupLimitExceededViolation)

instance
  Data.FromJSON
    DnsRuleGroupLimitExceededViolation
  where
  parseJSON =
    Data.withObject
      "DnsRuleGroupLimitExceededViolation"
      ( \x ->
          DnsRuleGroupLimitExceededViolation'
            Prelude.<$> (x Data..:? "NumberOfRuleGroupsAlreadyAssociated")
            Prelude.<*> (x Data..:? "ViolationTarget")
            Prelude.<*> (x Data..:? "ViolationTargetDescription")
      )

instance
  Prelude.Hashable
    DnsRuleGroupLimitExceededViolation
  where
  hashWithSalt
    _salt
    DnsRuleGroupLimitExceededViolation' {..} =
      _salt
        `Prelude.hashWithSalt` numberOfRuleGroupsAlreadyAssociated
        `Prelude.hashWithSalt` violationTarget
        `Prelude.hashWithSalt` violationTargetDescription

instance
  Prelude.NFData
    DnsRuleGroupLimitExceededViolation
  where
  rnf DnsRuleGroupLimitExceededViolation' {..} =
    Prelude.rnf numberOfRuleGroupsAlreadyAssociated `Prelude.seq`
      Prelude.rnf violationTarget `Prelude.seq`
        Prelude.rnf violationTargetDescription
