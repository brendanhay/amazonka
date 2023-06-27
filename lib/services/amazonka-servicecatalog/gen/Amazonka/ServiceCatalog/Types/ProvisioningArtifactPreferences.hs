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
-- Module      : Amazonka.ServiceCatalog.Types.ProvisioningArtifactPreferences
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.ProvisioningArtifactPreferences where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The user-defined preferences that will be applied during product
-- provisioning, unless overridden by @ProvisioningPreferences@ or
-- @UpdateProvisioningPreferences@.
--
-- For more information on maximum concurrent accounts and failure
-- tolerance, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-ops-options Stack set operation options>
-- in the /CloudFormation User Guide/.
--
-- /See:/ 'newProvisioningArtifactPreferences' smart constructor.
data ProvisioningArtifactPreferences = ProvisioningArtifactPreferences'
  { -- | One or more Amazon Web Services accounts where stack instances are
    -- deployed from the stack set. These accounts can be scoped in
    -- @ProvisioningPreferences$StackSetAccounts@ and
    -- @UpdateProvisioningPreferences$StackSetAccounts@.
    --
    -- Applicable only to a @CFN_STACKSET@ provisioned product type.
    stackSetAccounts :: Prelude.Maybe [Prelude.Text],
    -- | One or more Amazon Web Services Regions where stack instances are
    -- deployed from the stack set. These Regions can be scoped in
    -- @ProvisioningPreferences$StackSetRegions@ and
    -- @UpdateProvisioningPreferences$StackSetRegions@.
    --
    -- Applicable only to a @CFN_STACKSET@ provisioned product type.
    stackSetRegions :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProvisioningArtifactPreferences' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackSetAccounts', 'provisioningArtifactPreferences_stackSetAccounts' - One or more Amazon Web Services accounts where stack instances are
-- deployed from the stack set. These accounts can be scoped in
-- @ProvisioningPreferences$StackSetAccounts@ and
-- @UpdateProvisioningPreferences$StackSetAccounts@.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
--
-- 'stackSetRegions', 'provisioningArtifactPreferences_stackSetRegions' - One or more Amazon Web Services Regions where stack instances are
-- deployed from the stack set. These Regions can be scoped in
-- @ProvisioningPreferences$StackSetRegions@ and
-- @UpdateProvisioningPreferences$StackSetRegions@.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
newProvisioningArtifactPreferences ::
  ProvisioningArtifactPreferences
newProvisioningArtifactPreferences =
  ProvisioningArtifactPreferences'
    { stackSetAccounts =
        Prelude.Nothing,
      stackSetRegions = Prelude.Nothing
    }

-- | One or more Amazon Web Services accounts where stack instances are
-- deployed from the stack set. These accounts can be scoped in
-- @ProvisioningPreferences$StackSetAccounts@ and
-- @UpdateProvisioningPreferences$StackSetAccounts@.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
provisioningArtifactPreferences_stackSetAccounts :: Lens.Lens' ProvisioningArtifactPreferences (Prelude.Maybe [Prelude.Text])
provisioningArtifactPreferences_stackSetAccounts = Lens.lens (\ProvisioningArtifactPreferences' {stackSetAccounts} -> stackSetAccounts) (\s@ProvisioningArtifactPreferences' {} a -> s {stackSetAccounts = a} :: ProvisioningArtifactPreferences) Prelude.. Lens.mapping Lens.coerced

-- | One or more Amazon Web Services Regions where stack instances are
-- deployed from the stack set. These Regions can be scoped in
-- @ProvisioningPreferences$StackSetRegions@ and
-- @UpdateProvisioningPreferences$StackSetRegions@.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
provisioningArtifactPreferences_stackSetRegions :: Lens.Lens' ProvisioningArtifactPreferences (Prelude.Maybe [Prelude.Text])
provisioningArtifactPreferences_stackSetRegions = Lens.lens (\ProvisioningArtifactPreferences' {stackSetRegions} -> stackSetRegions) (\s@ProvisioningArtifactPreferences' {} a -> s {stackSetRegions = a} :: ProvisioningArtifactPreferences) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    ProvisioningArtifactPreferences
  where
  parseJSON =
    Data.withObject
      "ProvisioningArtifactPreferences"
      ( \x ->
          ProvisioningArtifactPreferences'
            Prelude.<$> ( x
                            Data..:? "StackSetAccounts"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "StackSetRegions"
                            Data..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    ProvisioningArtifactPreferences
  where
  hashWithSalt
    _salt
    ProvisioningArtifactPreferences' {..} =
      _salt
        `Prelude.hashWithSalt` stackSetAccounts
        `Prelude.hashWithSalt` stackSetRegions

instance
  Prelude.NFData
    ProvisioningArtifactPreferences
  where
  rnf ProvisioningArtifactPreferences' {..} =
    Prelude.rnf stackSetAccounts
      `Prelude.seq` Prelude.rnf stackSetRegions
