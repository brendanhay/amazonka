-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisionedProductPlanSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProvisionedProductPlanSummary
  ( ProvisionedProductPlanSummary (..),

    -- * Smart constructor
    mkProvisionedProductPlanSummary,

    -- * Lenses
    pppsProvisionProductId,
    pppsProvisioningArtifactId,
    pppsProvisionProductName,
    pppsPlanId,
    pppsPlanName,
    pppsPlanType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.ServiceCatalog.Types.ProvisionedProductPlanType

-- | Summary information about a plan.
--
-- /See:/ 'mkProvisionedProductPlanSummary' smart constructor.
data ProvisionedProductPlanSummary = ProvisionedProductPlanSummary'
  { provisionProductId ::
      Lude.Maybe Lude.Text,
    provisioningArtifactId ::
      Lude.Maybe Lude.Text,
    provisionProductName ::
      Lude.Maybe Lude.Text,
    planId :: Lude.Maybe Lude.Text,
    planName ::
      Lude.Maybe Lude.Text,
    planType ::
      Lude.Maybe
        ProvisionedProductPlanType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProvisionedProductPlanSummary' with the minimum fields required to make a request.
--
-- * 'planId' - The plan identifier.
-- * 'planName' - The name of the plan.
-- * 'planType' - The plan type.
-- * 'provisionProductId' - The product identifier.
-- * 'provisionProductName' - The user-friendly name of the provisioned product.
-- * 'provisioningArtifactId' - The identifier of the provisioning artifact.
mkProvisionedProductPlanSummary ::
  ProvisionedProductPlanSummary
mkProvisionedProductPlanSummary =
  ProvisionedProductPlanSummary'
    { provisionProductId = Lude.Nothing,
      provisioningArtifactId = Lude.Nothing,
      provisionProductName = Lude.Nothing,
      planId = Lude.Nothing,
      planName = Lude.Nothing,
      planType = Lude.Nothing
    }

-- | The product identifier.
--
-- /Note:/ Consider using 'provisionProductId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pppsProvisionProductId :: Lens.Lens' ProvisionedProductPlanSummary (Lude.Maybe Lude.Text)
pppsProvisionProductId = Lens.lens (provisionProductId :: ProvisionedProductPlanSummary -> Lude.Maybe Lude.Text) (\s a -> s {provisionProductId = a} :: ProvisionedProductPlanSummary)
{-# DEPRECATED pppsProvisionProductId "Use generic-lens or generic-optics with 'provisionProductId' instead." #-}

-- | The identifier of the provisioning artifact.
--
-- /Note:/ Consider using 'provisioningArtifactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pppsProvisioningArtifactId :: Lens.Lens' ProvisionedProductPlanSummary (Lude.Maybe Lude.Text)
pppsProvisioningArtifactId = Lens.lens (provisioningArtifactId :: ProvisionedProductPlanSummary -> Lude.Maybe Lude.Text) (\s a -> s {provisioningArtifactId = a} :: ProvisionedProductPlanSummary)
{-# DEPRECATED pppsProvisioningArtifactId "Use generic-lens or generic-optics with 'provisioningArtifactId' instead." #-}

-- | The user-friendly name of the provisioned product.
--
-- /Note:/ Consider using 'provisionProductName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pppsProvisionProductName :: Lens.Lens' ProvisionedProductPlanSummary (Lude.Maybe Lude.Text)
pppsProvisionProductName = Lens.lens (provisionProductName :: ProvisionedProductPlanSummary -> Lude.Maybe Lude.Text) (\s a -> s {provisionProductName = a} :: ProvisionedProductPlanSummary)
{-# DEPRECATED pppsProvisionProductName "Use generic-lens or generic-optics with 'provisionProductName' instead." #-}

-- | The plan identifier.
--
-- /Note:/ Consider using 'planId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pppsPlanId :: Lens.Lens' ProvisionedProductPlanSummary (Lude.Maybe Lude.Text)
pppsPlanId = Lens.lens (planId :: ProvisionedProductPlanSummary -> Lude.Maybe Lude.Text) (\s a -> s {planId = a} :: ProvisionedProductPlanSummary)
{-# DEPRECATED pppsPlanId "Use generic-lens or generic-optics with 'planId' instead." #-}

-- | The name of the plan.
--
-- /Note:/ Consider using 'planName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pppsPlanName :: Lens.Lens' ProvisionedProductPlanSummary (Lude.Maybe Lude.Text)
pppsPlanName = Lens.lens (planName :: ProvisionedProductPlanSummary -> Lude.Maybe Lude.Text) (\s a -> s {planName = a} :: ProvisionedProductPlanSummary)
{-# DEPRECATED pppsPlanName "Use generic-lens or generic-optics with 'planName' instead." #-}

-- | The plan type.
--
-- /Note:/ Consider using 'planType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pppsPlanType :: Lens.Lens' ProvisionedProductPlanSummary (Lude.Maybe ProvisionedProductPlanType)
pppsPlanType = Lens.lens (planType :: ProvisionedProductPlanSummary -> Lude.Maybe ProvisionedProductPlanType) (\s a -> s {planType = a} :: ProvisionedProductPlanSummary)
{-# DEPRECATED pppsPlanType "Use generic-lens or generic-optics with 'planType' instead." #-}

instance Lude.FromJSON ProvisionedProductPlanSummary where
  parseJSON =
    Lude.withObject
      "ProvisionedProductPlanSummary"
      ( \x ->
          ProvisionedProductPlanSummary'
            Lude.<$> (x Lude..:? "ProvisionProductId")
            Lude.<*> (x Lude..:? "ProvisioningArtifactId")
            Lude.<*> (x Lude..:? "ProvisionProductName")
            Lude.<*> (x Lude..:? "PlanId")
            Lude.<*> (x Lude..:? "PlanName")
            Lude.<*> (x Lude..:? "PlanType")
      )
