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
import qualified Network.AWS.Prelude as Lude

-- | The metadata for a service, such as the service code and available attribute names.
--
-- /See:/ 'mkPricingService' smart constructor.
data PricingService = PricingService'
  { -- | The attributes that are available for this service.
    attributeNames :: Lude.Maybe [Lude.Text],
    -- | The code for the AWS service.
    serviceCode :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PricingService' with the minimum fields required to make a request.
--
-- * 'attributeNames' - The attributes that are available for this service.
-- * 'serviceCode' - The code for the AWS service.
mkPricingService ::
  PricingService
mkPricingService =
  PricingService'
    { attributeNames = Lude.Nothing,
      serviceCode = Lude.Nothing
    }

-- | The attributes that are available for this service.
--
-- /Note:/ Consider using 'attributeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psAttributeNames :: Lens.Lens' PricingService (Lude.Maybe [Lude.Text])
psAttributeNames = Lens.lens (attributeNames :: PricingService -> Lude.Maybe [Lude.Text]) (\s a -> s {attributeNames = a} :: PricingService)
{-# DEPRECATED psAttributeNames "Use generic-lens or generic-optics with 'attributeNames' instead." #-}

-- | The code for the AWS service.
--
-- /Note:/ Consider using 'serviceCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psServiceCode :: Lens.Lens' PricingService (Lude.Maybe Lude.Text)
psServiceCode = Lens.lens (serviceCode :: PricingService -> Lude.Maybe Lude.Text) (\s a -> s {serviceCode = a} :: PricingService)
{-# DEPRECATED psServiceCode "Use generic-lens or generic-optics with 'serviceCode' instead." #-}

instance Lude.FromJSON PricingService where
  parseJSON =
    Lude.withObject
      "PricingService"
      ( \x ->
          PricingService'
            Lude.<$> (x Lude..:? "AttributeNames" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ServiceCode")
      )
