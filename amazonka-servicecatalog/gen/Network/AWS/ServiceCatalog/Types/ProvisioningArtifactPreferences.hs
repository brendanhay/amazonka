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
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisioningArtifactPreferences
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProvisioningArtifactPreferences where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The user-defined preferences that will be applied during product
-- provisioning, unless overridden by @ProvisioningPreferences@ or
-- @UpdateProvisioningPreferences@.
--
-- For more information on maximum concurrent accounts and failure
-- tolerance, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-ops-options Stack set operation options>
-- in the /AWS CloudFormation User Guide/.
--
-- /See:/ 'newProvisioningArtifactPreferences' smart constructor.
data ProvisioningArtifactPreferences = ProvisioningArtifactPreferences'
  { -- | One or more AWS accounts where stack instances are deployed from the
    -- stack set. These accounts can be scoped in
    -- @ProvisioningPreferences$StackSetAccounts@ and
    -- @UpdateProvisioningPreferences$StackSetAccounts@.
    --
    -- Applicable only to a @CFN_STACKSET@ provisioned product type.
    stackSetAccounts :: Core.Maybe [Core.Text],
    -- | One or more AWS Regions where stack instances are deployed from the
    -- stack set. These regions can be scoped in
    -- @ProvisioningPreferences$StackSetRegions@ and
    -- @UpdateProvisioningPreferences$StackSetRegions@.
    --
    -- Applicable only to a @CFN_STACKSET@ provisioned product type.
    stackSetRegions :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ProvisioningArtifactPreferences' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackSetAccounts', 'provisioningArtifactPreferences_stackSetAccounts' - One or more AWS accounts where stack instances are deployed from the
-- stack set. These accounts can be scoped in
-- @ProvisioningPreferences$StackSetAccounts@ and
-- @UpdateProvisioningPreferences$StackSetAccounts@.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
--
-- 'stackSetRegions', 'provisioningArtifactPreferences_stackSetRegions' - One or more AWS Regions where stack instances are deployed from the
-- stack set. These regions can be scoped in
-- @ProvisioningPreferences$StackSetRegions@ and
-- @UpdateProvisioningPreferences$StackSetRegions@.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
newProvisioningArtifactPreferences ::
  ProvisioningArtifactPreferences
newProvisioningArtifactPreferences =
  ProvisioningArtifactPreferences'
    { stackSetAccounts =
        Core.Nothing,
      stackSetRegions = Core.Nothing
    }

-- | One or more AWS accounts where stack instances are deployed from the
-- stack set. These accounts can be scoped in
-- @ProvisioningPreferences$StackSetAccounts@ and
-- @UpdateProvisioningPreferences$StackSetAccounts@.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
provisioningArtifactPreferences_stackSetAccounts :: Lens.Lens' ProvisioningArtifactPreferences (Core.Maybe [Core.Text])
provisioningArtifactPreferences_stackSetAccounts = Lens.lens (\ProvisioningArtifactPreferences' {stackSetAccounts} -> stackSetAccounts) (\s@ProvisioningArtifactPreferences' {} a -> s {stackSetAccounts = a} :: ProvisioningArtifactPreferences) Core.. Lens.mapping Lens._Coerce

-- | One or more AWS Regions where stack instances are deployed from the
-- stack set. These regions can be scoped in
-- @ProvisioningPreferences$StackSetRegions@ and
-- @UpdateProvisioningPreferences$StackSetRegions@.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
provisioningArtifactPreferences_stackSetRegions :: Lens.Lens' ProvisioningArtifactPreferences (Core.Maybe [Core.Text])
provisioningArtifactPreferences_stackSetRegions = Lens.lens (\ProvisioningArtifactPreferences' {stackSetRegions} -> stackSetRegions) (\s@ProvisioningArtifactPreferences' {} a -> s {stackSetRegions = a} :: ProvisioningArtifactPreferences) Core.. Lens.mapping Lens._Coerce

instance
  Core.FromJSON
    ProvisioningArtifactPreferences
  where
  parseJSON =
    Core.withObject
      "ProvisioningArtifactPreferences"
      ( \x ->
          ProvisioningArtifactPreferences'
            Core.<$> (x Core..:? "StackSetAccounts" Core..!= Core.mempty)
            Core.<*> (x Core..:? "StackSetRegions" Core..!= Core.mempty)
      )

instance
  Core.Hashable
    ProvisioningArtifactPreferences

instance Core.NFData ProvisioningArtifactPreferences
