{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.VariantProperty
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.VariantProperty
  ( VariantProperty (..)
  -- * Smart constructor
  , mkVariantProperty
  -- * Lenses
  , vpVariantPropertyType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.VariantPropertyType as Types

-- | Specifies a production variant property type for an Endpoint.
--
-- If you are updating an endpoint with the 'UpdateEndpointInput$RetainAllVariantProperties' option set to @true@ , the @VariantProperty@ objects listed in 'UpdateEndpointInput$ExcludeRetainedVariantProperties' override the existing variant properties of the endpoint.
--
-- /See:/ 'mkVariantProperty' smart constructor.
newtype VariantProperty = VariantProperty'
  { variantPropertyType :: Types.VariantPropertyType
    -- ^ The type of variant property. The supported values are:
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'VariantProperty' value with any optional fields omitted.
mkVariantProperty
    :: Types.VariantPropertyType -- ^ 'variantPropertyType'
    -> VariantProperty
mkVariantProperty variantPropertyType
  = VariantProperty'{variantPropertyType}

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
vpVariantPropertyType :: Lens.Lens' VariantProperty Types.VariantPropertyType
vpVariantPropertyType = Lens.field @"variantPropertyType"
{-# INLINEABLE vpVariantPropertyType #-}
{-# DEPRECATED variantPropertyType "Use generic-lens or generic-optics with 'variantPropertyType' instead"  #-}

instance Core.FromJSON VariantProperty where
        toJSON VariantProperty{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("VariantPropertyType" Core..= variantPropertyType)])
