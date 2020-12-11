-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProductionVariantSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProductionVariantSummary
  ( ProductionVariantSummary (..),

    -- * Smart constructor
    mkProductionVariantSummary,

    -- * Lenses
    pvsDesiredInstanceCount,
    pvsDesiredWeight,
    pvsCurrentWeight,
    pvsCurrentInstanceCount,
    pvsDeployedImages,
    pvsVariantName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.DeployedImage

-- | Describes weight and capacities for a production variant associated with an endpoint. If you sent a request to the @UpdateEndpointWeightsAndCapacities@ API and the endpoint status is @Updating@ , you get different desired and current values.
--
-- /See:/ 'mkProductionVariantSummary' smart constructor.
data ProductionVariantSummary = ProductionVariantSummary'
  { desiredInstanceCount ::
      Lude.Maybe Lude.Natural,
    desiredWeight :: Lude.Maybe Lude.Double,
    currentWeight :: Lude.Maybe Lude.Double,
    currentInstanceCount ::
      Lude.Maybe Lude.Natural,
    deployedImages ::
      Lude.Maybe [DeployedImage],
    variantName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProductionVariantSummary' with the minimum fields required to make a request.
--
-- * 'currentInstanceCount' - The number of instances associated with the variant.
-- * 'currentWeight' - The weight associated with the variant.
-- * 'deployedImages' - An array of @DeployedImage@ objects that specify the Amazon EC2 Container Registry paths of the inference images deployed on instances of this @ProductionVariant@ .
-- * 'desiredInstanceCount' - The number of instances requested in the @UpdateEndpointWeightsAndCapacities@ request.
-- * 'desiredWeight' - The requested weight, as specified in the @UpdateEndpointWeightsAndCapacities@ request.
-- * 'variantName' - The name of the variant.
mkProductionVariantSummary ::
  -- | 'variantName'
  Lude.Text ->
  ProductionVariantSummary
mkProductionVariantSummary pVariantName_ =
  ProductionVariantSummary'
    { desiredInstanceCount = Lude.Nothing,
      desiredWeight = Lude.Nothing,
      currentWeight = Lude.Nothing,
      currentInstanceCount = Lude.Nothing,
      deployedImages = Lude.Nothing,
      variantName = pVariantName_
    }

-- | The number of instances requested in the @UpdateEndpointWeightsAndCapacities@ request.
--
-- /Note:/ Consider using 'desiredInstanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvsDesiredInstanceCount :: Lens.Lens' ProductionVariantSummary (Lude.Maybe Lude.Natural)
pvsDesiredInstanceCount = Lens.lens (desiredInstanceCount :: ProductionVariantSummary -> Lude.Maybe Lude.Natural) (\s a -> s {desiredInstanceCount = a} :: ProductionVariantSummary)
{-# DEPRECATED pvsDesiredInstanceCount "Use generic-lens or generic-optics with 'desiredInstanceCount' instead." #-}

-- | The requested weight, as specified in the @UpdateEndpointWeightsAndCapacities@ request.
--
-- /Note:/ Consider using 'desiredWeight' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvsDesiredWeight :: Lens.Lens' ProductionVariantSummary (Lude.Maybe Lude.Double)
pvsDesiredWeight = Lens.lens (desiredWeight :: ProductionVariantSummary -> Lude.Maybe Lude.Double) (\s a -> s {desiredWeight = a} :: ProductionVariantSummary)
{-# DEPRECATED pvsDesiredWeight "Use generic-lens or generic-optics with 'desiredWeight' instead." #-}

-- | The weight associated with the variant.
--
-- /Note:/ Consider using 'currentWeight' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvsCurrentWeight :: Lens.Lens' ProductionVariantSummary (Lude.Maybe Lude.Double)
pvsCurrentWeight = Lens.lens (currentWeight :: ProductionVariantSummary -> Lude.Maybe Lude.Double) (\s a -> s {currentWeight = a} :: ProductionVariantSummary)
{-# DEPRECATED pvsCurrentWeight "Use generic-lens or generic-optics with 'currentWeight' instead." #-}

-- | The number of instances associated with the variant.
--
-- /Note:/ Consider using 'currentInstanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvsCurrentInstanceCount :: Lens.Lens' ProductionVariantSummary (Lude.Maybe Lude.Natural)
pvsCurrentInstanceCount = Lens.lens (currentInstanceCount :: ProductionVariantSummary -> Lude.Maybe Lude.Natural) (\s a -> s {currentInstanceCount = a} :: ProductionVariantSummary)
{-# DEPRECATED pvsCurrentInstanceCount "Use generic-lens or generic-optics with 'currentInstanceCount' instead." #-}

-- | An array of @DeployedImage@ objects that specify the Amazon EC2 Container Registry paths of the inference images deployed on instances of this @ProductionVariant@ .
--
-- /Note:/ Consider using 'deployedImages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvsDeployedImages :: Lens.Lens' ProductionVariantSummary (Lude.Maybe [DeployedImage])
pvsDeployedImages = Lens.lens (deployedImages :: ProductionVariantSummary -> Lude.Maybe [DeployedImage]) (\s a -> s {deployedImages = a} :: ProductionVariantSummary)
{-# DEPRECATED pvsDeployedImages "Use generic-lens or generic-optics with 'deployedImages' instead." #-}

-- | The name of the variant.
--
-- /Note:/ Consider using 'variantName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvsVariantName :: Lens.Lens' ProductionVariantSummary Lude.Text
pvsVariantName = Lens.lens (variantName :: ProductionVariantSummary -> Lude.Text) (\s a -> s {variantName = a} :: ProductionVariantSummary)
{-# DEPRECATED pvsVariantName "Use generic-lens or generic-optics with 'variantName' instead." #-}

instance Lude.FromJSON ProductionVariantSummary where
  parseJSON =
    Lude.withObject
      "ProductionVariantSummary"
      ( \x ->
          ProductionVariantSummary'
            Lude.<$> (x Lude..:? "DesiredInstanceCount")
            Lude.<*> (x Lude..:? "DesiredWeight")
            Lude.<*> (x Lude..:? "CurrentWeight")
            Lude.<*> (x Lude..:? "CurrentInstanceCount")
            Lude.<*> (x Lude..:? "DeployedImages" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "VariantName")
      )
