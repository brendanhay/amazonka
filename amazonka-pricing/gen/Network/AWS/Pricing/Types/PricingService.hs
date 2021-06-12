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
-- Module      : Network.AWS.Pricing.Types.PricingService
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pricing.Types.PricingService where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The metadata for a service, such as the service code and available
-- attribute names.
--
-- /See:/ 'newPricingService' smart constructor.
data PricingService = PricingService'
  { -- | The code for the AWS service.
    serviceCode :: Core.Maybe Core.Text,
    -- | The attributes that are available for this service.
    attributeNames :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PricingService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceCode', 'pricingService_serviceCode' - The code for the AWS service.
--
-- 'attributeNames', 'pricingService_attributeNames' - The attributes that are available for this service.
newPricingService ::
  PricingService
newPricingService =
  PricingService'
    { serviceCode = Core.Nothing,
      attributeNames = Core.Nothing
    }

-- | The code for the AWS service.
pricingService_serviceCode :: Lens.Lens' PricingService (Core.Maybe Core.Text)
pricingService_serviceCode = Lens.lens (\PricingService' {serviceCode} -> serviceCode) (\s@PricingService' {} a -> s {serviceCode = a} :: PricingService)

-- | The attributes that are available for this service.
pricingService_attributeNames :: Lens.Lens' PricingService (Core.Maybe [Core.Text])
pricingService_attributeNames = Lens.lens (\PricingService' {attributeNames} -> attributeNames) (\s@PricingService' {} a -> s {attributeNames = a} :: PricingService) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON PricingService where
  parseJSON =
    Core.withObject
      "PricingService"
      ( \x ->
          PricingService'
            Core.<$> (x Core..:? "ServiceCode")
            Core.<*> (x Core..:? "AttributeNames" Core..!= Core.mempty)
      )

instance Core.Hashable PricingService

instance Core.NFData PricingService
