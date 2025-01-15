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
-- Module      : Amazonka.WAFV2.Types.ManagedRuleGroupSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.ManagedRuleGroupSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | High-level information about a managed rule group, returned by
-- ListAvailableManagedRuleGroups. This provides information like the name
-- and vendor name, that you provide when you add a
-- ManagedRuleGroupStatement to a web ACL. Managed rule groups include
-- Amazon Web Services Managed Rules rule groups, which are free of charge
-- to WAF customers, and Amazon Web Services Marketplace managed rule
-- groups, which you can subscribe to through Amazon Web Services
-- Marketplace.
--
-- /See:/ 'newManagedRuleGroupSummary' smart constructor.
data ManagedRuleGroupSummary = ManagedRuleGroupSummary'
  { -- | The description of the managed rule group, provided by Amazon Web
    -- Services Managed Rules or the Amazon Web Services Marketplace seller who
    -- manages it.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the managed rule group. You use this, along with the vendor
    -- name, to identify the rule group.
    name :: Prelude.Maybe Prelude.Text,
    -- | The name of the managed rule group vendor. You use this, along with the
    -- rule group name, to identify the rule group.
    vendorName :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the managed rule group is versioned. If it is, you can
    -- retrieve the versions list by calling
    -- ListAvailableManagedRuleGroupVersions.
    versioningSupported :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ManagedRuleGroupSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'managedRuleGroupSummary_description' - The description of the managed rule group, provided by Amazon Web
-- Services Managed Rules or the Amazon Web Services Marketplace seller who
-- manages it.
--
-- 'name', 'managedRuleGroupSummary_name' - The name of the managed rule group. You use this, along with the vendor
-- name, to identify the rule group.
--
-- 'vendorName', 'managedRuleGroupSummary_vendorName' - The name of the managed rule group vendor. You use this, along with the
-- rule group name, to identify the rule group.
--
-- 'versioningSupported', 'managedRuleGroupSummary_versioningSupported' - Indicates whether the managed rule group is versioned. If it is, you can
-- retrieve the versions list by calling
-- ListAvailableManagedRuleGroupVersions.
newManagedRuleGroupSummary ::
  ManagedRuleGroupSummary
newManagedRuleGroupSummary =
  ManagedRuleGroupSummary'
    { description =
        Prelude.Nothing,
      name = Prelude.Nothing,
      vendorName = Prelude.Nothing,
      versioningSupported = Prelude.Nothing
    }

-- | The description of the managed rule group, provided by Amazon Web
-- Services Managed Rules or the Amazon Web Services Marketplace seller who
-- manages it.
managedRuleGroupSummary_description :: Lens.Lens' ManagedRuleGroupSummary (Prelude.Maybe Prelude.Text)
managedRuleGroupSummary_description = Lens.lens (\ManagedRuleGroupSummary' {description} -> description) (\s@ManagedRuleGroupSummary' {} a -> s {description = a} :: ManagedRuleGroupSummary)

-- | The name of the managed rule group. You use this, along with the vendor
-- name, to identify the rule group.
managedRuleGroupSummary_name :: Lens.Lens' ManagedRuleGroupSummary (Prelude.Maybe Prelude.Text)
managedRuleGroupSummary_name = Lens.lens (\ManagedRuleGroupSummary' {name} -> name) (\s@ManagedRuleGroupSummary' {} a -> s {name = a} :: ManagedRuleGroupSummary)

-- | The name of the managed rule group vendor. You use this, along with the
-- rule group name, to identify the rule group.
managedRuleGroupSummary_vendorName :: Lens.Lens' ManagedRuleGroupSummary (Prelude.Maybe Prelude.Text)
managedRuleGroupSummary_vendorName = Lens.lens (\ManagedRuleGroupSummary' {vendorName} -> vendorName) (\s@ManagedRuleGroupSummary' {} a -> s {vendorName = a} :: ManagedRuleGroupSummary)

-- | Indicates whether the managed rule group is versioned. If it is, you can
-- retrieve the versions list by calling
-- ListAvailableManagedRuleGroupVersions.
managedRuleGroupSummary_versioningSupported :: Lens.Lens' ManagedRuleGroupSummary (Prelude.Maybe Prelude.Bool)
managedRuleGroupSummary_versioningSupported = Lens.lens (\ManagedRuleGroupSummary' {versioningSupported} -> versioningSupported) (\s@ManagedRuleGroupSummary' {} a -> s {versioningSupported = a} :: ManagedRuleGroupSummary)

instance Data.FromJSON ManagedRuleGroupSummary where
  parseJSON =
    Data.withObject
      "ManagedRuleGroupSummary"
      ( \x ->
          ManagedRuleGroupSummary'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "VendorName")
            Prelude.<*> (x Data..:? "VersioningSupported")
      )

instance Prelude.Hashable ManagedRuleGroupSummary where
  hashWithSalt _salt ManagedRuleGroupSummary' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` vendorName
      `Prelude.hashWithSalt` versioningSupported

instance Prelude.NFData ManagedRuleGroupSummary where
  rnf ManagedRuleGroupSummary' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf name `Prelude.seq`
        Prelude.rnf vendorName `Prelude.seq`
          Prelude.rnf versioningSupported
