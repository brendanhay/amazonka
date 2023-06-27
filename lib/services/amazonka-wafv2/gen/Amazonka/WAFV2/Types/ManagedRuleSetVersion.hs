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
-- Module      : Amazonka.WAFV2.Types.ManagedRuleSetVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.ManagedRuleSetVersion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information for a single version of a managed rule set.
--
-- This is intended for use only by vendors of managed rule sets. Vendors
-- are Amazon Web Services and Amazon Web Services Marketplace sellers.
--
-- Vendors, you can use the managed rule set APIs to provide controlled
-- rollout of your versioned managed rule group offerings for your
-- customers. The APIs are @ListManagedRuleSets@, @GetManagedRuleSet@,
-- @PutManagedRuleSetVersions@, and
-- @UpdateManagedRuleSetVersionExpiryDate@.
--
-- /See:/ 'newManagedRuleSetVersion' smart constructor.
data ManagedRuleSetVersion = ManagedRuleSetVersion'
  { -- | The Amazon Resource Name (ARN) of the vendor rule group that\'s used to
    -- define the published version of your managed rule group.
    associatedRuleGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The web ACL capacity units (WCUs) required for this rule group.
    --
    -- WAF uses WCUs to calculate and control the operating resources that are
    -- used to run your rules, rule groups, and web ACLs. WAF calculates
    -- capacity differently for each rule type, to reflect the relative cost of
    -- each rule. Simple rules that cost little to run use fewer WCUs than more
    -- complex rules that use more processing power. Rule group capacity is
    -- fixed at creation, which helps users plan their web ACL WCU usage when
    -- they use a rule group. For more information, see
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/aws-waf-capacity-units.html WAF web ACL capacity units (WCU)>
    -- in the /WAF Developer Guide/.
    capacity :: Prelude.Maybe Prelude.Natural,
    -- | The time that this version is set to expire.
    --
    -- Times are in Coordinated Universal Time (UTC) format. UTC format
    -- includes the special designator, Z. For example, \"2016-09-27T14:50Z\".
    expiryTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The amount of time you expect this version of your managed rule group to
    -- last, in days.
    forecastedLifetime :: Prelude.Maybe Prelude.Natural,
    -- | The last time that you updated this version.
    --
    -- Times are in Coordinated Universal Time (UTC) format. UTC format
    -- includes the special designator, Z. For example, \"2016-09-27T14:50Z\".
    lastUpdateTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The time that you first published this version.
    --
    -- Times are in Coordinated Universal Time (UTC) format. UTC format
    -- includes the special designator, Z. For example, \"2016-09-27T14:50Z\".
    publishTimestamp :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ManagedRuleSetVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associatedRuleGroupArn', 'managedRuleSetVersion_associatedRuleGroupArn' - The Amazon Resource Name (ARN) of the vendor rule group that\'s used to
-- define the published version of your managed rule group.
--
-- 'capacity', 'managedRuleSetVersion_capacity' - The web ACL capacity units (WCUs) required for this rule group.
--
-- WAF uses WCUs to calculate and control the operating resources that are
-- used to run your rules, rule groups, and web ACLs. WAF calculates
-- capacity differently for each rule type, to reflect the relative cost of
-- each rule. Simple rules that cost little to run use fewer WCUs than more
-- complex rules that use more processing power. Rule group capacity is
-- fixed at creation, which helps users plan their web ACL WCU usage when
-- they use a rule group. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/aws-waf-capacity-units.html WAF web ACL capacity units (WCU)>
-- in the /WAF Developer Guide/.
--
-- 'expiryTimestamp', 'managedRuleSetVersion_expiryTimestamp' - The time that this version is set to expire.
--
-- Times are in Coordinated Universal Time (UTC) format. UTC format
-- includes the special designator, Z. For example, \"2016-09-27T14:50Z\".
--
-- 'forecastedLifetime', 'managedRuleSetVersion_forecastedLifetime' - The amount of time you expect this version of your managed rule group to
-- last, in days.
--
-- 'lastUpdateTimestamp', 'managedRuleSetVersion_lastUpdateTimestamp' - The last time that you updated this version.
--
-- Times are in Coordinated Universal Time (UTC) format. UTC format
-- includes the special designator, Z. For example, \"2016-09-27T14:50Z\".
--
-- 'publishTimestamp', 'managedRuleSetVersion_publishTimestamp' - The time that you first published this version.
--
-- Times are in Coordinated Universal Time (UTC) format. UTC format
-- includes the special designator, Z. For example, \"2016-09-27T14:50Z\".
newManagedRuleSetVersion ::
  ManagedRuleSetVersion
newManagedRuleSetVersion =
  ManagedRuleSetVersion'
    { associatedRuleGroupArn =
        Prelude.Nothing,
      capacity = Prelude.Nothing,
      expiryTimestamp = Prelude.Nothing,
      forecastedLifetime = Prelude.Nothing,
      lastUpdateTimestamp = Prelude.Nothing,
      publishTimestamp = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the vendor rule group that\'s used to
-- define the published version of your managed rule group.
managedRuleSetVersion_associatedRuleGroupArn :: Lens.Lens' ManagedRuleSetVersion (Prelude.Maybe Prelude.Text)
managedRuleSetVersion_associatedRuleGroupArn = Lens.lens (\ManagedRuleSetVersion' {associatedRuleGroupArn} -> associatedRuleGroupArn) (\s@ManagedRuleSetVersion' {} a -> s {associatedRuleGroupArn = a} :: ManagedRuleSetVersion)

-- | The web ACL capacity units (WCUs) required for this rule group.
--
-- WAF uses WCUs to calculate and control the operating resources that are
-- used to run your rules, rule groups, and web ACLs. WAF calculates
-- capacity differently for each rule type, to reflect the relative cost of
-- each rule. Simple rules that cost little to run use fewer WCUs than more
-- complex rules that use more processing power. Rule group capacity is
-- fixed at creation, which helps users plan their web ACL WCU usage when
-- they use a rule group. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/aws-waf-capacity-units.html WAF web ACL capacity units (WCU)>
-- in the /WAF Developer Guide/.
managedRuleSetVersion_capacity :: Lens.Lens' ManagedRuleSetVersion (Prelude.Maybe Prelude.Natural)
managedRuleSetVersion_capacity = Lens.lens (\ManagedRuleSetVersion' {capacity} -> capacity) (\s@ManagedRuleSetVersion' {} a -> s {capacity = a} :: ManagedRuleSetVersion)

-- | The time that this version is set to expire.
--
-- Times are in Coordinated Universal Time (UTC) format. UTC format
-- includes the special designator, Z. For example, \"2016-09-27T14:50Z\".
managedRuleSetVersion_expiryTimestamp :: Lens.Lens' ManagedRuleSetVersion (Prelude.Maybe Prelude.UTCTime)
managedRuleSetVersion_expiryTimestamp = Lens.lens (\ManagedRuleSetVersion' {expiryTimestamp} -> expiryTimestamp) (\s@ManagedRuleSetVersion' {} a -> s {expiryTimestamp = a} :: ManagedRuleSetVersion) Prelude.. Lens.mapping Data._Time

-- | The amount of time you expect this version of your managed rule group to
-- last, in days.
managedRuleSetVersion_forecastedLifetime :: Lens.Lens' ManagedRuleSetVersion (Prelude.Maybe Prelude.Natural)
managedRuleSetVersion_forecastedLifetime = Lens.lens (\ManagedRuleSetVersion' {forecastedLifetime} -> forecastedLifetime) (\s@ManagedRuleSetVersion' {} a -> s {forecastedLifetime = a} :: ManagedRuleSetVersion)

-- | The last time that you updated this version.
--
-- Times are in Coordinated Universal Time (UTC) format. UTC format
-- includes the special designator, Z. For example, \"2016-09-27T14:50Z\".
managedRuleSetVersion_lastUpdateTimestamp :: Lens.Lens' ManagedRuleSetVersion (Prelude.Maybe Prelude.UTCTime)
managedRuleSetVersion_lastUpdateTimestamp = Lens.lens (\ManagedRuleSetVersion' {lastUpdateTimestamp} -> lastUpdateTimestamp) (\s@ManagedRuleSetVersion' {} a -> s {lastUpdateTimestamp = a} :: ManagedRuleSetVersion) Prelude.. Lens.mapping Data._Time

-- | The time that you first published this version.
--
-- Times are in Coordinated Universal Time (UTC) format. UTC format
-- includes the special designator, Z. For example, \"2016-09-27T14:50Z\".
managedRuleSetVersion_publishTimestamp :: Lens.Lens' ManagedRuleSetVersion (Prelude.Maybe Prelude.UTCTime)
managedRuleSetVersion_publishTimestamp = Lens.lens (\ManagedRuleSetVersion' {publishTimestamp} -> publishTimestamp) (\s@ManagedRuleSetVersion' {} a -> s {publishTimestamp = a} :: ManagedRuleSetVersion) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON ManagedRuleSetVersion where
  parseJSON =
    Data.withObject
      "ManagedRuleSetVersion"
      ( \x ->
          ManagedRuleSetVersion'
            Prelude.<$> (x Data..:? "AssociatedRuleGroupArn")
            Prelude.<*> (x Data..:? "Capacity")
            Prelude.<*> (x Data..:? "ExpiryTimestamp")
            Prelude.<*> (x Data..:? "ForecastedLifetime")
            Prelude.<*> (x Data..:? "LastUpdateTimestamp")
            Prelude.<*> (x Data..:? "PublishTimestamp")
      )

instance Prelude.Hashable ManagedRuleSetVersion where
  hashWithSalt _salt ManagedRuleSetVersion' {..} =
    _salt
      `Prelude.hashWithSalt` associatedRuleGroupArn
      `Prelude.hashWithSalt` capacity
      `Prelude.hashWithSalt` expiryTimestamp
      `Prelude.hashWithSalt` forecastedLifetime
      `Prelude.hashWithSalt` lastUpdateTimestamp
      `Prelude.hashWithSalt` publishTimestamp

instance Prelude.NFData ManagedRuleSetVersion where
  rnf ManagedRuleSetVersion' {..} =
    Prelude.rnf associatedRuleGroupArn
      `Prelude.seq` Prelude.rnf capacity
      `Prelude.seq` Prelude.rnf expiryTimestamp
      `Prelude.seq` Prelude.rnf forecastedLifetime
      `Prelude.seq` Prelude.rnf lastUpdateTimestamp
      `Prelude.seq` Prelude.rnf publishTimestamp
