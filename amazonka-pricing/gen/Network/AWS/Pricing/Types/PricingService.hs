{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The metadata for a service, such as the service code and available
-- attribute names.
--
-- /See:/ 'newPricingService' smart constructor.
data PricingService = PricingService'
  { -- | The code for the AWS service.
    serviceCode :: Prelude.Maybe Prelude.Text,
    -- | The attributes that are available for this service.
    attributeNames :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { serviceCode = Prelude.Nothing,
      attributeNames = Prelude.Nothing
    }

-- | The code for the AWS service.
pricingService_serviceCode :: Lens.Lens' PricingService (Prelude.Maybe Prelude.Text)
pricingService_serviceCode = Lens.lens (\PricingService' {serviceCode} -> serviceCode) (\s@PricingService' {} a -> s {serviceCode = a} :: PricingService)

-- | The attributes that are available for this service.
pricingService_attributeNames :: Lens.Lens' PricingService (Prelude.Maybe [Prelude.Text])
pricingService_attributeNames = Lens.lens (\PricingService' {attributeNames} -> attributeNames) (\s@PricingService' {} a -> s {attributeNames = a} :: PricingService) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON PricingService where
  parseJSON =
    Prelude.withObject
      "PricingService"
      ( \x ->
          PricingService'
            Prelude.<$> (x Prelude..:? "ServiceCode")
            Prelude.<*> ( x Prelude..:? "AttributeNames"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable PricingService

instance Prelude.NFData PricingService
