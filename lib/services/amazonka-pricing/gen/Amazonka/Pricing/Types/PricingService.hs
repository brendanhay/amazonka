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
-- Module      : Amazonka.Pricing.Types.PricingService
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pricing.Types.PricingService where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The metadata for a service, such as the service code and available
-- attribute names.
--
-- /See:/ 'newPricingService' smart constructor.
data PricingService = PricingService'
  { -- | The attributes that are available for this service.
    attributeNames :: Prelude.Maybe [Prelude.Text],
    -- | The code for the Amazon Web Services service.
    serviceCode :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PricingService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeNames', 'pricingService_attributeNames' - The attributes that are available for this service.
--
-- 'serviceCode', 'pricingService_serviceCode' - The code for the Amazon Web Services service.
newPricingService ::
  -- | 'serviceCode'
  Prelude.Text ->
  PricingService
newPricingService pServiceCode_ =
  PricingService'
    { attributeNames = Prelude.Nothing,
      serviceCode = pServiceCode_
    }

-- | The attributes that are available for this service.
pricingService_attributeNames :: Lens.Lens' PricingService (Prelude.Maybe [Prelude.Text])
pricingService_attributeNames = Lens.lens (\PricingService' {attributeNames} -> attributeNames) (\s@PricingService' {} a -> s {attributeNames = a} :: PricingService) Prelude.. Lens.mapping Lens.coerced

-- | The code for the Amazon Web Services service.
pricingService_serviceCode :: Lens.Lens' PricingService Prelude.Text
pricingService_serviceCode = Lens.lens (\PricingService' {serviceCode} -> serviceCode) (\s@PricingService' {} a -> s {serviceCode = a} :: PricingService)

instance Data.FromJSON PricingService where
  parseJSON =
    Data.withObject
      "PricingService"
      ( \x ->
          PricingService'
            Prelude.<$> (x Data..:? "AttributeNames" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "ServiceCode")
      )

instance Prelude.Hashable PricingService where
  hashWithSalt _salt PricingService' {..} =
    _salt
      `Prelude.hashWithSalt` attributeNames
      `Prelude.hashWithSalt` serviceCode

instance Prelude.NFData PricingService where
  rnf PricingService' {..} =
    Prelude.rnf attributeNames
      `Prelude.seq` Prelude.rnf serviceCode
