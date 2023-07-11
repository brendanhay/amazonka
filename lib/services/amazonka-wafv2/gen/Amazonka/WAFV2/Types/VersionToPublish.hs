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
-- Module      : Amazonka.WAFV2.Types.VersionToPublish
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.VersionToPublish where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A version of the named managed rule group, that the rule group\'s vendor
-- publishes for use by customers.
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
-- /See:/ 'newVersionToPublish' smart constructor.
data VersionToPublish = VersionToPublish'
  { -- | The Amazon Resource Name (ARN) of the vendor\'s rule group that\'s used
    -- in the published managed rule group version.
    associatedRuleGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The amount of time the vendor expects this version of the managed rule
    -- group to last, in days.
    forecastedLifetime :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VersionToPublish' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associatedRuleGroupArn', 'versionToPublish_associatedRuleGroupArn' - The Amazon Resource Name (ARN) of the vendor\'s rule group that\'s used
-- in the published managed rule group version.
--
-- 'forecastedLifetime', 'versionToPublish_forecastedLifetime' - The amount of time the vendor expects this version of the managed rule
-- group to last, in days.
newVersionToPublish ::
  VersionToPublish
newVersionToPublish =
  VersionToPublish'
    { associatedRuleGroupArn =
        Prelude.Nothing,
      forecastedLifetime = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the vendor\'s rule group that\'s used
-- in the published managed rule group version.
versionToPublish_associatedRuleGroupArn :: Lens.Lens' VersionToPublish (Prelude.Maybe Prelude.Text)
versionToPublish_associatedRuleGroupArn = Lens.lens (\VersionToPublish' {associatedRuleGroupArn} -> associatedRuleGroupArn) (\s@VersionToPublish' {} a -> s {associatedRuleGroupArn = a} :: VersionToPublish)

-- | The amount of time the vendor expects this version of the managed rule
-- group to last, in days.
versionToPublish_forecastedLifetime :: Lens.Lens' VersionToPublish (Prelude.Maybe Prelude.Natural)
versionToPublish_forecastedLifetime = Lens.lens (\VersionToPublish' {forecastedLifetime} -> forecastedLifetime) (\s@VersionToPublish' {} a -> s {forecastedLifetime = a} :: VersionToPublish)

instance Prelude.Hashable VersionToPublish where
  hashWithSalt _salt VersionToPublish' {..} =
    _salt
      `Prelude.hashWithSalt` associatedRuleGroupArn
      `Prelude.hashWithSalt` forecastedLifetime

instance Prelude.NFData VersionToPublish where
  rnf VersionToPublish' {..} =
    Prelude.rnf associatedRuleGroupArn
      `Prelude.seq` Prelude.rnf forecastedLifetime

instance Data.ToJSON VersionToPublish where
  toJSON VersionToPublish' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AssociatedRuleGroupArn" Data..=)
              Prelude.<$> associatedRuleGroupArn,
            ("ForecastedLifetime" Data..=)
              Prelude.<$> forecastedLifetime
          ]
      )
