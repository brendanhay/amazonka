{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pricing.Types.PricingService
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pricing.Types.PricingService
  ( PricingService (..),

    -- * Smart constructor
    mkPricingService,

    -- * Lenses
    psAttributeNames,
    psServiceCode,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Pricing.Types.String as Types

-- | The metadata for a service, such as the service code and available attribute names.
--
-- /See:/ 'mkPricingService' smart constructor.
data PricingService = PricingService'
  { -- | The attributes that are available for this service.
    attributeNames :: Core.Maybe [Types.String],
    -- | The code for the AWS service.
    serviceCode :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PricingService' value with any optional fields omitted.
mkPricingService ::
  PricingService
mkPricingService =
  PricingService'
    { attributeNames = Core.Nothing,
      serviceCode = Core.Nothing
    }

-- | The attributes that are available for this service.
--
-- /Note:/ Consider using 'attributeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psAttributeNames :: Lens.Lens' PricingService (Core.Maybe [Types.String])
psAttributeNames = Lens.field @"attributeNames"
{-# DEPRECATED psAttributeNames "Use generic-lens or generic-optics with 'attributeNames' instead." #-}

-- | The code for the AWS service.
--
-- /Note:/ Consider using 'serviceCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psServiceCode :: Lens.Lens' PricingService (Core.Maybe Types.String)
psServiceCode = Lens.field @"serviceCode"
{-# DEPRECATED psServiceCode "Use generic-lens or generic-optics with 'serviceCode' instead." #-}

instance Core.FromJSON PricingService where
  parseJSON =
    Core.withObject "PricingService" Core.$
      \x ->
        PricingService'
          Core.<$> (x Core..:? "AttributeNames") Core.<*> (x Core..:? "ServiceCode")
