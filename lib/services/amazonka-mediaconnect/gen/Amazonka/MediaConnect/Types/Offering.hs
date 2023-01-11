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
-- Module      : Amazonka.MediaConnect.Types.Offering
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.Offering where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types.DurationUnits
import Amazonka.MediaConnect.Types.PriceUnits
import Amazonka.MediaConnect.Types.ResourceSpecification
import qualified Amazonka.Prelude as Prelude

-- | A savings plan that reserves a certain amount of outbound bandwidth
-- usage at a discounted rate each month over a period of time.
--
-- /See:/ 'newOffering' smart constructor.
data Offering = Offering'
  { -- | The type of currency that is used for billing. The currencyCode used for
    -- all reservations is US dollars.
    currencyCode :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) that MediaConnect assigns to the
    -- offering.
    offeringArn :: Prelude.Text,
    -- | A description of the offering.
    offeringDescription :: Prelude.Text,
    -- | The unit of measurement for the duration of the offering.
    durationUnits :: DurationUnits,
    -- | The length of time that your reservation would be active.
    duration :: Prelude.Int,
    -- | The cost of a single unit. This value, in combination with priceUnits,
    -- makes up the rate.
    pricePerUnit :: Prelude.Text,
    -- | A definition of the amount of outbound bandwidth that you would be
    -- reserving if you purchase the offering.
    resourceSpecification :: ResourceSpecification,
    -- | The unit of measurement that is used for billing. This value, in
    -- combination with pricePerUnit, makes up the rate.
    priceUnits :: PriceUnits
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Offering' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'currencyCode', 'offering_currencyCode' - The type of currency that is used for billing. The currencyCode used for
-- all reservations is US dollars.
--
-- 'offeringArn', 'offering_offeringArn' - The Amazon Resource Name (ARN) that MediaConnect assigns to the
-- offering.
--
-- 'offeringDescription', 'offering_offeringDescription' - A description of the offering.
--
-- 'durationUnits', 'offering_durationUnits' - The unit of measurement for the duration of the offering.
--
-- 'duration', 'offering_duration' - The length of time that your reservation would be active.
--
-- 'pricePerUnit', 'offering_pricePerUnit' - The cost of a single unit. This value, in combination with priceUnits,
-- makes up the rate.
--
-- 'resourceSpecification', 'offering_resourceSpecification' - A definition of the amount of outbound bandwidth that you would be
-- reserving if you purchase the offering.
--
-- 'priceUnits', 'offering_priceUnits' - The unit of measurement that is used for billing. This value, in
-- combination with pricePerUnit, makes up the rate.
newOffering ::
  -- | 'currencyCode'
  Prelude.Text ->
  -- | 'offeringArn'
  Prelude.Text ->
  -- | 'offeringDescription'
  Prelude.Text ->
  -- | 'durationUnits'
  DurationUnits ->
  -- | 'duration'
  Prelude.Int ->
  -- | 'pricePerUnit'
  Prelude.Text ->
  -- | 'resourceSpecification'
  ResourceSpecification ->
  -- | 'priceUnits'
  PriceUnits ->
  Offering
newOffering
  pCurrencyCode_
  pOfferingArn_
  pOfferingDescription_
  pDurationUnits_
  pDuration_
  pPricePerUnit_
  pResourceSpecification_
  pPriceUnits_ =
    Offering'
      { currencyCode = pCurrencyCode_,
        offeringArn = pOfferingArn_,
        offeringDescription = pOfferingDescription_,
        durationUnits = pDurationUnits_,
        duration = pDuration_,
        pricePerUnit = pPricePerUnit_,
        resourceSpecification = pResourceSpecification_,
        priceUnits = pPriceUnits_
      }

-- | The type of currency that is used for billing. The currencyCode used for
-- all reservations is US dollars.
offering_currencyCode :: Lens.Lens' Offering Prelude.Text
offering_currencyCode = Lens.lens (\Offering' {currencyCode} -> currencyCode) (\s@Offering' {} a -> s {currencyCode = a} :: Offering)

-- | The Amazon Resource Name (ARN) that MediaConnect assigns to the
-- offering.
offering_offeringArn :: Lens.Lens' Offering Prelude.Text
offering_offeringArn = Lens.lens (\Offering' {offeringArn} -> offeringArn) (\s@Offering' {} a -> s {offeringArn = a} :: Offering)

-- | A description of the offering.
offering_offeringDescription :: Lens.Lens' Offering Prelude.Text
offering_offeringDescription = Lens.lens (\Offering' {offeringDescription} -> offeringDescription) (\s@Offering' {} a -> s {offeringDescription = a} :: Offering)

-- | The unit of measurement for the duration of the offering.
offering_durationUnits :: Lens.Lens' Offering DurationUnits
offering_durationUnits = Lens.lens (\Offering' {durationUnits} -> durationUnits) (\s@Offering' {} a -> s {durationUnits = a} :: Offering)

-- | The length of time that your reservation would be active.
offering_duration :: Lens.Lens' Offering Prelude.Int
offering_duration = Lens.lens (\Offering' {duration} -> duration) (\s@Offering' {} a -> s {duration = a} :: Offering)

-- | The cost of a single unit. This value, in combination with priceUnits,
-- makes up the rate.
offering_pricePerUnit :: Lens.Lens' Offering Prelude.Text
offering_pricePerUnit = Lens.lens (\Offering' {pricePerUnit} -> pricePerUnit) (\s@Offering' {} a -> s {pricePerUnit = a} :: Offering)

-- | A definition of the amount of outbound bandwidth that you would be
-- reserving if you purchase the offering.
offering_resourceSpecification :: Lens.Lens' Offering ResourceSpecification
offering_resourceSpecification = Lens.lens (\Offering' {resourceSpecification} -> resourceSpecification) (\s@Offering' {} a -> s {resourceSpecification = a} :: Offering)

-- | The unit of measurement that is used for billing. This value, in
-- combination with pricePerUnit, makes up the rate.
offering_priceUnits :: Lens.Lens' Offering PriceUnits
offering_priceUnits = Lens.lens (\Offering' {priceUnits} -> priceUnits) (\s@Offering' {} a -> s {priceUnits = a} :: Offering)

instance Data.FromJSON Offering where
  parseJSON =
    Data.withObject
      "Offering"
      ( \x ->
          Offering'
            Prelude.<$> (x Data..: "currencyCode")
            Prelude.<*> (x Data..: "offeringArn")
            Prelude.<*> (x Data..: "offeringDescription")
            Prelude.<*> (x Data..: "durationUnits")
            Prelude.<*> (x Data..: "duration")
            Prelude.<*> (x Data..: "pricePerUnit")
            Prelude.<*> (x Data..: "resourceSpecification")
            Prelude.<*> (x Data..: "priceUnits")
      )

instance Prelude.Hashable Offering where
  hashWithSalt _salt Offering' {..} =
    _salt `Prelude.hashWithSalt` currencyCode
      `Prelude.hashWithSalt` offeringArn
      `Prelude.hashWithSalt` offeringDescription
      `Prelude.hashWithSalt` durationUnits
      `Prelude.hashWithSalt` duration
      `Prelude.hashWithSalt` pricePerUnit
      `Prelude.hashWithSalt` resourceSpecification
      `Prelude.hashWithSalt` priceUnits

instance Prelude.NFData Offering where
  rnf Offering' {..} =
    Prelude.rnf currencyCode
      `Prelude.seq` Prelude.rnf offeringArn
      `Prelude.seq` Prelude.rnf offeringDescription
      `Prelude.seq` Prelude.rnf durationUnits
      `Prelude.seq` Prelude.rnf duration
      `Prelude.seq` Prelude.rnf pricePerUnit
      `Prelude.seq` Prelude.rnf resourceSpecification
      `Prelude.seq` Prelude.rnf priceUnits
