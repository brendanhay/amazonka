{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisioningArtifactPreferences
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProvisioningArtifactPreferences
  ( ProvisioningArtifactPreferences (..),

    -- * Smart constructor
    mkProvisioningArtifactPreferences,

    -- * Lenses
    papStackSetAccounts,
    papStackSetRegions,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ServiceCatalog.Types.AccountId as Types
import qualified Network.AWS.ServiceCatalog.Types.Region as Types

-- | The user-defined preferences that will be applied during product provisioning, unless overridden by @ProvisioningPreferences@ or @UpdateProvisioningPreferences@ .
--
-- For more information on maximum concurrent accounts and failure tolerance, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-ops-options Stack set operation options> in the /AWS CloudFormation User Guide/ .
--
-- /See:/ 'mkProvisioningArtifactPreferences' smart constructor.
data ProvisioningArtifactPreferences = ProvisioningArtifactPreferences'
  { -- | One or more AWS accounts where stack instances are deployed from the stack set. These accounts can be scoped in @ProvisioningPreferences$StackSetAccounts@ and @UpdateProvisioningPreferences$StackSetAccounts@ .
    --
    -- Applicable only to a @CFN_STACKSET@ provisioned product type.
    stackSetAccounts :: Core.Maybe [Types.AccountId],
    -- | One or more AWS Regions where stack instances are deployed from the stack set. These regions can be scoped in @ProvisioningPreferences$StackSetRegions@ and @UpdateProvisioningPreferences$StackSetRegions@ .
    --
    -- Applicable only to a @CFN_STACKSET@ provisioned product type.
    stackSetRegions :: Core.Maybe [Types.Region]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProvisioningArtifactPreferences' value with any optional fields omitted.
mkProvisioningArtifactPreferences ::
  ProvisioningArtifactPreferences
mkProvisioningArtifactPreferences =
  ProvisioningArtifactPreferences'
    { stackSetAccounts = Core.Nothing,
      stackSetRegions = Core.Nothing
    }

-- | One or more AWS accounts where stack instances are deployed from the stack set. These accounts can be scoped in @ProvisioningPreferences$StackSetAccounts@ and @UpdateProvisioningPreferences$StackSetAccounts@ .
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
--
-- /Note:/ Consider using 'stackSetAccounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
papStackSetAccounts :: Lens.Lens' ProvisioningArtifactPreferences (Core.Maybe [Types.AccountId])
papStackSetAccounts = Lens.field @"stackSetAccounts"
{-# DEPRECATED papStackSetAccounts "Use generic-lens or generic-optics with 'stackSetAccounts' instead." #-}

-- | One or more AWS Regions where stack instances are deployed from the stack set. These regions can be scoped in @ProvisioningPreferences$StackSetRegions@ and @UpdateProvisioningPreferences$StackSetRegions@ .
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
--
-- /Note:/ Consider using 'stackSetRegions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
papStackSetRegions :: Lens.Lens' ProvisioningArtifactPreferences (Core.Maybe [Types.Region])
papStackSetRegions = Lens.field @"stackSetRegions"
{-# DEPRECATED papStackSetRegions "Use generic-lens or generic-optics with 'stackSetRegions' instead." #-}

instance Core.FromJSON ProvisioningArtifactPreferences where
  parseJSON =
    Core.withObject "ProvisioningArtifactPreferences" Core.$
      \x ->
        ProvisioningArtifactPreferences'
          Core.<$> (x Core..:? "StackSetAccounts")
          Core.<*> (x Core..:? "StackSetRegions")
