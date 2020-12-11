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
    papStackSetRegions,
    papStackSetAccounts,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The user-defined preferences that will be applied during product provisioning, unless overridden by @ProvisioningPreferences@ or @UpdateProvisioningPreferences@ .
--
-- For more information on maximum concurrent accounts and failure tolerance, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-ops-options Stack set operation options> in the /AWS CloudFormation User Guide/ .
--
-- /See:/ 'mkProvisioningArtifactPreferences' smart constructor.
data ProvisioningArtifactPreferences = ProvisioningArtifactPreferences'
  { stackSetRegions ::
      Lude.Maybe [Lude.Text],
    stackSetAccounts ::
      Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProvisioningArtifactPreferences' with the minimum fields required to make a request.
--
-- * 'stackSetAccounts' - One or more AWS accounts where stack instances are deployed from the stack set. These accounts can be scoped in @ProvisioningPreferences$StackSetAccounts@ and @UpdateProvisioningPreferences$StackSetAccounts@ .
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
-- * 'stackSetRegions' - One or more AWS Regions where stack instances are deployed from the stack set. These regions can be scoped in @ProvisioningPreferences$StackSetRegions@ and @UpdateProvisioningPreferences$StackSetRegions@ .
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
mkProvisioningArtifactPreferences ::
  ProvisioningArtifactPreferences
mkProvisioningArtifactPreferences =
  ProvisioningArtifactPreferences'
    { stackSetRegions = Lude.Nothing,
      stackSetAccounts = Lude.Nothing
    }

-- | One or more AWS Regions where stack instances are deployed from the stack set. These regions can be scoped in @ProvisioningPreferences$StackSetRegions@ and @UpdateProvisioningPreferences$StackSetRegions@ .
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
--
-- /Note:/ Consider using 'stackSetRegions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
papStackSetRegions :: Lens.Lens' ProvisioningArtifactPreferences (Lude.Maybe [Lude.Text])
papStackSetRegions = Lens.lens (stackSetRegions :: ProvisioningArtifactPreferences -> Lude.Maybe [Lude.Text]) (\s a -> s {stackSetRegions = a} :: ProvisioningArtifactPreferences)
{-# DEPRECATED papStackSetRegions "Use generic-lens or generic-optics with 'stackSetRegions' instead." #-}

-- | One or more AWS accounts where stack instances are deployed from the stack set. These accounts can be scoped in @ProvisioningPreferences$StackSetAccounts@ and @UpdateProvisioningPreferences$StackSetAccounts@ .
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
--
-- /Note:/ Consider using 'stackSetAccounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
papStackSetAccounts :: Lens.Lens' ProvisioningArtifactPreferences (Lude.Maybe [Lude.Text])
papStackSetAccounts = Lens.lens (stackSetAccounts :: ProvisioningArtifactPreferences -> Lude.Maybe [Lude.Text]) (\s a -> s {stackSetAccounts = a} :: ProvisioningArtifactPreferences)
{-# DEPRECATED papStackSetAccounts "Use generic-lens or generic-optics with 'stackSetAccounts' instead." #-}

instance Lude.FromJSON ProvisioningArtifactPreferences where
  parseJSON =
    Lude.withObject
      "ProvisioningArtifactPreferences"
      ( \x ->
          ProvisioningArtifactPreferences'
            Lude.<$> (x Lude..:? "StackSetRegions" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "StackSetAccounts" Lude..!= Lude.mempty)
      )
