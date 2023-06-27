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
-- Module      : Amazonka.Pricing.Types.PriceList
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pricing.Types.PriceList where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /__This feature is in preview release and is subject to change. Your use
-- of Amazon Web Services Price List API is subject to the Beta Service
-- Participation terms of the
-- <https://aws.amazon.com/service-terms/ Amazon Web Services Service Terms>
-- (Section 1.10).__/
--
-- This is the type of price list references that match your request.
--
-- /See:/ 'newPriceList' smart constructor.
data PriceList = PriceList'
  { -- | The three alphabetical character ISO-4217 currency code the Price List
    -- files are denominated in.
    currencyCode :: Prelude.Maybe Prelude.Text,
    -- | The format you want to retrieve your Price List files. The @FileFormat@
    -- can be obtained from the
    -- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_pricing_ListPriceLists.html ListPriceList>
    -- response.
    fileFormats :: Prelude.Maybe [Prelude.Text],
    -- | The unique identifier that maps to where your Price List files are
    -- located. @PriceListArn@ can be obtained from the
    -- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_pricing_ListPriceLists.html ListPriceList>
    -- response.
    priceListArn :: Prelude.Maybe Prelude.Text,
    -- | This is used to filter the Price List by Amazon Web Services Region. For
    -- example, to get the price list only for the @US East (N. Virginia)@
    -- Region, use @us-east-1@. If nothing is specified, you retrieve price
    -- lists for all applicable Regions. The available @RegionCode@ list can be
    -- retrieved from
    -- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_pricing_GetAttributeValues.html GetAttributeValues>
    -- API.
    regionCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PriceList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'currencyCode', 'priceList_currencyCode' - The three alphabetical character ISO-4217 currency code the Price List
-- files are denominated in.
--
-- 'fileFormats', 'priceList_fileFormats' - The format you want to retrieve your Price List files. The @FileFormat@
-- can be obtained from the
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_pricing_ListPriceLists.html ListPriceList>
-- response.
--
-- 'priceListArn', 'priceList_priceListArn' - The unique identifier that maps to where your Price List files are
-- located. @PriceListArn@ can be obtained from the
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_pricing_ListPriceLists.html ListPriceList>
-- response.
--
-- 'regionCode', 'priceList_regionCode' - This is used to filter the Price List by Amazon Web Services Region. For
-- example, to get the price list only for the @US East (N. Virginia)@
-- Region, use @us-east-1@. If nothing is specified, you retrieve price
-- lists for all applicable Regions. The available @RegionCode@ list can be
-- retrieved from
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_pricing_GetAttributeValues.html GetAttributeValues>
-- API.
newPriceList ::
  PriceList
newPriceList =
  PriceList'
    { currencyCode = Prelude.Nothing,
      fileFormats = Prelude.Nothing,
      priceListArn = Prelude.Nothing,
      regionCode = Prelude.Nothing
    }

-- | The three alphabetical character ISO-4217 currency code the Price List
-- files are denominated in.
priceList_currencyCode :: Lens.Lens' PriceList (Prelude.Maybe Prelude.Text)
priceList_currencyCode = Lens.lens (\PriceList' {currencyCode} -> currencyCode) (\s@PriceList' {} a -> s {currencyCode = a} :: PriceList)

-- | The format you want to retrieve your Price List files. The @FileFormat@
-- can be obtained from the
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_pricing_ListPriceLists.html ListPriceList>
-- response.
priceList_fileFormats :: Lens.Lens' PriceList (Prelude.Maybe [Prelude.Text])
priceList_fileFormats = Lens.lens (\PriceList' {fileFormats} -> fileFormats) (\s@PriceList' {} a -> s {fileFormats = a} :: PriceList) Prelude.. Lens.mapping Lens.coerced

-- | The unique identifier that maps to where your Price List files are
-- located. @PriceListArn@ can be obtained from the
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_pricing_ListPriceLists.html ListPriceList>
-- response.
priceList_priceListArn :: Lens.Lens' PriceList (Prelude.Maybe Prelude.Text)
priceList_priceListArn = Lens.lens (\PriceList' {priceListArn} -> priceListArn) (\s@PriceList' {} a -> s {priceListArn = a} :: PriceList)

-- | This is used to filter the Price List by Amazon Web Services Region. For
-- example, to get the price list only for the @US East (N. Virginia)@
-- Region, use @us-east-1@. If nothing is specified, you retrieve price
-- lists for all applicable Regions. The available @RegionCode@ list can be
-- retrieved from
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_pricing_GetAttributeValues.html GetAttributeValues>
-- API.
priceList_regionCode :: Lens.Lens' PriceList (Prelude.Maybe Prelude.Text)
priceList_regionCode = Lens.lens (\PriceList' {regionCode} -> regionCode) (\s@PriceList' {} a -> s {regionCode = a} :: PriceList)

instance Data.FromJSON PriceList where
  parseJSON =
    Data.withObject
      "PriceList"
      ( \x ->
          PriceList'
            Prelude.<$> (x Data..:? "CurrencyCode")
            Prelude.<*> (x Data..:? "FileFormats" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "PriceListArn")
            Prelude.<*> (x Data..:? "RegionCode")
      )

instance Prelude.Hashable PriceList where
  hashWithSalt _salt PriceList' {..} =
    _salt
      `Prelude.hashWithSalt` currencyCode
      `Prelude.hashWithSalt` fileFormats
      `Prelude.hashWithSalt` priceListArn
      `Prelude.hashWithSalt` regionCode

instance Prelude.NFData PriceList where
  rnf PriceList' {..} =
    Prelude.rnf currencyCode
      `Prelude.seq` Prelude.rnf fileFormats
      `Prelude.seq` Prelude.rnf priceListArn
      `Prelude.seq` Prelude.rnf regionCode
