{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.VariantProperty
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.VariantProperty
  ( VariantProperty (..),

    -- * Smart constructor
    mkVariantProperty,

    -- * Lenses
    vpVariantPropertyType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.VariantPropertyType

-- | Specifies a production variant property type for an Endpoint.
--
-- If you are updating an endpoint with the 'UpdateEndpointInput$RetainAllVariantProperties' option set to @true@ , the @VariantProperty@ objects listed in 'UpdateEndpointInput$ExcludeRetainedVariantProperties' override the existing variant properties of the endpoint.
--
-- /See:/ 'mkVariantProperty' smart constructor.
newtype VariantProperty = VariantProperty'
  { -- | The type of variant property. The supported values are:
    --
    --
    --     * @DesiredInstanceCount@ : Overrides the existing variant instance counts using the 'ProductionVariant$InitialInstanceCount' values in the 'CreateEndpointConfigInput$ProductionVariants' .
    --
    --
    --     * @DesiredWeight@ : Overrides the existing variant weights using the 'ProductionVariant$InitialVariantWeight' values in the 'CreateEndpointConfigInput$ProductionVariants' .
    --
    --
    --     * @DataCaptureConfig@ : (Not currently supported.)
    variantPropertyType :: VariantPropertyType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VariantProperty' with the minimum fields required to make a request.
--
-- * 'variantPropertyType' - The type of variant property. The supported values are:
--
--
--     * @DesiredInstanceCount@ : Overrides the existing variant instance counts using the 'ProductionVariant$InitialInstanceCount' values in the 'CreateEndpointConfigInput$ProductionVariants' .
--
--
--     * @DesiredWeight@ : Overrides the existing variant weights using the 'ProductionVariant$InitialVariantWeight' values in the 'CreateEndpointConfigInput$ProductionVariants' .
--
--
--     * @DataCaptureConfig@ : (Not currently supported.)
mkVariantProperty ::
  -- | 'variantPropertyType'
  VariantPropertyType ->
  VariantProperty
mkVariantProperty pVariantPropertyType_ =
  VariantProperty' {variantPropertyType = pVariantPropertyType_}

-- | The type of variant property. The supported values are:
--
--
--     * @DesiredInstanceCount@ : Overrides the existing variant instance counts using the 'ProductionVariant$InitialInstanceCount' values in the 'CreateEndpointConfigInput$ProductionVariants' .
--
--
--     * @DesiredWeight@ : Overrides the existing variant weights using the 'ProductionVariant$InitialVariantWeight' values in the 'CreateEndpointConfigInput$ProductionVariants' .
--
--
--     * @DataCaptureConfig@ : (Not currently supported.)
--
--
--
-- /Note:/ Consider using 'variantPropertyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpVariantPropertyType :: Lens.Lens' VariantProperty VariantPropertyType
vpVariantPropertyType = Lens.lens (variantPropertyType :: VariantProperty -> VariantPropertyType) (\s a -> s {variantPropertyType = a} :: VariantProperty)
{-# DEPRECATED vpVariantPropertyType "Use generic-lens or generic-optics with 'variantPropertyType' instead." #-}

instance Lude.ToJSON VariantProperty where
  toJSON VariantProperty' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("VariantPropertyType" Lude..= variantPropertyType)]
      )
