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
-- Module      : Amazonka.Forecast.Types.AdditionalDataset
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.AdditionalDataset where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an additional dataset. This object is part of the DataConfig
-- object. Forecast supports the Weather Index and Holidays additional
-- datasets.
--
-- __Weather Index__
--
-- The Amazon Forecast Weather Index is a built-in dataset that
-- incorporates historical and projected weather information into your
-- model. The Weather Index supplements your datasets with over two years
-- of historical weather data and up to 14 days of projected weather data.
-- For more information, see
-- <https://docs.aws.amazon.com/forecast/latest/dg/weather.html Amazon Forecast Weather Index>.
--
-- __Holidays__
--
-- Holidays is a built-in dataset that incorporates national holiday
-- information into your model. It provides native support for the holiday
-- calendars of 66 countries. To view the holiday calendars, refer to the
-- <http://jollyday.sourceforge.net/data.html Jollyday> library. For more
-- information, see
-- <https://docs.aws.amazon.com/forecast/latest/dg/holidays.html Holidays Featurization>.
--
-- /See:/ 'newAdditionalDataset' smart constructor.
data AdditionalDataset = AdditionalDataset'
  { -- | __Weather Index__
    --
    -- To enable the Weather Index, do not specify a value for @Configuration@.
    --
    -- __Holidays__
    --
    -- __Holidays__
    --
    -- To enable Holidays, set @CountryCode@ to one of the following two-letter
    -- country codes:
    --
    -- -   \"AL\" - ALBANIA
    --
    -- -   \"AR\" - ARGENTINA
    --
    -- -   \"AT\" - AUSTRIA
    --
    -- -   \"AU\" - AUSTRALIA
    --
    -- -   \"BA\" - BOSNIA HERZEGOVINA
    --
    -- -   \"BE\" - BELGIUM
    --
    -- -   \"BG\" - BULGARIA
    --
    -- -   \"BO\" - BOLIVIA
    --
    -- -   \"BR\" - BRAZIL
    --
    -- -   \"BY\" - BELARUS
    --
    -- -   \"CA\" - CANADA
    --
    -- -   \"CL\" - CHILE
    --
    -- -   \"CO\" - COLOMBIA
    --
    -- -   \"CR\" - COSTA RICA
    --
    -- -   \"HR\" - CROATIA
    --
    -- -   \"CZ\" - CZECH REPUBLIC
    --
    -- -   \"DK\" - DENMARK
    --
    -- -   \"EC\" - ECUADOR
    --
    -- -   \"EE\" - ESTONIA
    --
    -- -   \"ET\" - ETHIOPIA
    --
    -- -   \"FI\" - FINLAND
    --
    -- -   \"FR\" - FRANCE
    --
    -- -   \"DE\" - GERMANY
    --
    -- -   \"GR\" - GREECE
    --
    -- -   \"HU\" - HUNGARY
    --
    -- -   \"IS\" - ICELAND
    --
    -- -   \"IN\" - INDIA
    --
    -- -   \"IE\" - IRELAND
    --
    -- -   \"IT\" - ITALY
    --
    -- -   \"JP\" - JAPAN
    --
    -- -   \"KZ\" - KAZAKHSTAN
    --
    -- -   \"KR\" - KOREA
    --
    -- -   \"LV\" - LATVIA
    --
    -- -   \"LI\" - LIECHTENSTEIN
    --
    -- -   \"LT\" - LITHUANIA
    --
    -- -   \"LU\" - LUXEMBOURG
    --
    -- -   \"MK\" - MACEDONIA
    --
    -- -   \"MT\" - MALTA
    --
    -- -   \"MX\" - MEXICO
    --
    -- -   \"MD\" - MOLDOVA
    --
    -- -   \"ME\" - MONTENEGRO
    --
    -- -   \"NL\" - NETHERLANDS
    --
    -- -   \"NZ\" - NEW ZEALAND
    --
    -- -   \"NI\" - NICARAGUA
    --
    -- -   \"NG\" - NIGERIA
    --
    -- -   \"NO\" - NORWAY
    --
    -- -   \"PA\" - PANAMA
    --
    -- -   \"PY\" - PARAGUAY
    --
    -- -   \"PE\" - PERU
    --
    -- -   \"PL\" - POLAND
    --
    -- -   \"PT\" - PORTUGAL
    --
    -- -   \"RO\" - ROMANIA
    --
    -- -   \"RU\" - RUSSIA
    --
    -- -   \"RS\" - SERBIA
    --
    -- -   \"SK\" - SLOVAKIA
    --
    -- -   \"SI\" - SLOVENIA
    --
    -- -   \"ZA\" - SOUTH AFRICA
    --
    -- -   \"ES\" - SPAIN
    --
    -- -   \"SE\" - SWEDEN
    --
    -- -   \"CH\" - SWITZERLAND
    --
    -- -   \"UA\" - UKRAINE
    --
    -- -   \"AE\" - UNITED ARAB EMIRATES
    --
    -- -   \"US\" - UNITED STATES
    --
    -- -   \"UK\" - UNITED KINGDOM
    --
    -- -   \"UY\" - URUGUAY
    --
    -- -   \"VE\" - VENEZUELA
    configuration :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.NonEmpty Prelude.Text)),
    -- | The name of the additional dataset. Valid names: @\"holiday\"@ and
    -- @\"weather\"@.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AdditionalDataset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configuration', 'additionalDataset_configuration' - __Weather Index__
--
-- To enable the Weather Index, do not specify a value for @Configuration@.
--
-- __Holidays__
--
-- __Holidays__
--
-- To enable Holidays, set @CountryCode@ to one of the following two-letter
-- country codes:
--
-- -   \"AL\" - ALBANIA
--
-- -   \"AR\" - ARGENTINA
--
-- -   \"AT\" - AUSTRIA
--
-- -   \"AU\" - AUSTRALIA
--
-- -   \"BA\" - BOSNIA HERZEGOVINA
--
-- -   \"BE\" - BELGIUM
--
-- -   \"BG\" - BULGARIA
--
-- -   \"BO\" - BOLIVIA
--
-- -   \"BR\" - BRAZIL
--
-- -   \"BY\" - BELARUS
--
-- -   \"CA\" - CANADA
--
-- -   \"CL\" - CHILE
--
-- -   \"CO\" - COLOMBIA
--
-- -   \"CR\" - COSTA RICA
--
-- -   \"HR\" - CROATIA
--
-- -   \"CZ\" - CZECH REPUBLIC
--
-- -   \"DK\" - DENMARK
--
-- -   \"EC\" - ECUADOR
--
-- -   \"EE\" - ESTONIA
--
-- -   \"ET\" - ETHIOPIA
--
-- -   \"FI\" - FINLAND
--
-- -   \"FR\" - FRANCE
--
-- -   \"DE\" - GERMANY
--
-- -   \"GR\" - GREECE
--
-- -   \"HU\" - HUNGARY
--
-- -   \"IS\" - ICELAND
--
-- -   \"IN\" - INDIA
--
-- -   \"IE\" - IRELAND
--
-- -   \"IT\" - ITALY
--
-- -   \"JP\" - JAPAN
--
-- -   \"KZ\" - KAZAKHSTAN
--
-- -   \"KR\" - KOREA
--
-- -   \"LV\" - LATVIA
--
-- -   \"LI\" - LIECHTENSTEIN
--
-- -   \"LT\" - LITHUANIA
--
-- -   \"LU\" - LUXEMBOURG
--
-- -   \"MK\" - MACEDONIA
--
-- -   \"MT\" - MALTA
--
-- -   \"MX\" - MEXICO
--
-- -   \"MD\" - MOLDOVA
--
-- -   \"ME\" - MONTENEGRO
--
-- -   \"NL\" - NETHERLANDS
--
-- -   \"NZ\" - NEW ZEALAND
--
-- -   \"NI\" - NICARAGUA
--
-- -   \"NG\" - NIGERIA
--
-- -   \"NO\" - NORWAY
--
-- -   \"PA\" - PANAMA
--
-- -   \"PY\" - PARAGUAY
--
-- -   \"PE\" - PERU
--
-- -   \"PL\" - POLAND
--
-- -   \"PT\" - PORTUGAL
--
-- -   \"RO\" - ROMANIA
--
-- -   \"RU\" - RUSSIA
--
-- -   \"RS\" - SERBIA
--
-- -   \"SK\" - SLOVAKIA
--
-- -   \"SI\" - SLOVENIA
--
-- -   \"ZA\" - SOUTH AFRICA
--
-- -   \"ES\" - SPAIN
--
-- -   \"SE\" - SWEDEN
--
-- -   \"CH\" - SWITZERLAND
--
-- -   \"UA\" - UKRAINE
--
-- -   \"AE\" - UNITED ARAB EMIRATES
--
-- -   \"US\" - UNITED STATES
--
-- -   \"UK\" - UNITED KINGDOM
--
-- -   \"UY\" - URUGUAY
--
-- -   \"VE\" - VENEZUELA
--
-- 'name', 'additionalDataset_name' - The name of the additional dataset. Valid names: @\"holiday\"@ and
-- @\"weather\"@.
newAdditionalDataset ::
  -- | 'name'
  Prelude.Text ->
  AdditionalDataset
newAdditionalDataset pName_ =
  AdditionalDataset'
    { configuration = Prelude.Nothing,
      name = pName_
    }

-- | __Weather Index__
--
-- To enable the Weather Index, do not specify a value for @Configuration@.
--
-- __Holidays__
--
-- __Holidays__
--
-- To enable Holidays, set @CountryCode@ to one of the following two-letter
-- country codes:
--
-- -   \"AL\" - ALBANIA
--
-- -   \"AR\" - ARGENTINA
--
-- -   \"AT\" - AUSTRIA
--
-- -   \"AU\" - AUSTRALIA
--
-- -   \"BA\" - BOSNIA HERZEGOVINA
--
-- -   \"BE\" - BELGIUM
--
-- -   \"BG\" - BULGARIA
--
-- -   \"BO\" - BOLIVIA
--
-- -   \"BR\" - BRAZIL
--
-- -   \"BY\" - BELARUS
--
-- -   \"CA\" - CANADA
--
-- -   \"CL\" - CHILE
--
-- -   \"CO\" - COLOMBIA
--
-- -   \"CR\" - COSTA RICA
--
-- -   \"HR\" - CROATIA
--
-- -   \"CZ\" - CZECH REPUBLIC
--
-- -   \"DK\" - DENMARK
--
-- -   \"EC\" - ECUADOR
--
-- -   \"EE\" - ESTONIA
--
-- -   \"ET\" - ETHIOPIA
--
-- -   \"FI\" - FINLAND
--
-- -   \"FR\" - FRANCE
--
-- -   \"DE\" - GERMANY
--
-- -   \"GR\" - GREECE
--
-- -   \"HU\" - HUNGARY
--
-- -   \"IS\" - ICELAND
--
-- -   \"IN\" - INDIA
--
-- -   \"IE\" - IRELAND
--
-- -   \"IT\" - ITALY
--
-- -   \"JP\" - JAPAN
--
-- -   \"KZ\" - KAZAKHSTAN
--
-- -   \"KR\" - KOREA
--
-- -   \"LV\" - LATVIA
--
-- -   \"LI\" - LIECHTENSTEIN
--
-- -   \"LT\" - LITHUANIA
--
-- -   \"LU\" - LUXEMBOURG
--
-- -   \"MK\" - MACEDONIA
--
-- -   \"MT\" - MALTA
--
-- -   \"MX\" - MEXICO
--
-- -   \"MD\" - MOLDOVA
--
-- -   \"ME\" - MONTENEGRO
--
-- -   \"NL\" - NETHERLANDS
--
-- -   \"NZ\" - NEW ZEALAND
--
-- -   \"NI\" - NICARAGUA
--
-- -   \"NG\" - NIGERIA
--
-- -   \"NO\" - NORWAY
--
-- -   \"PA\" - PANAMA
--
-- -   \"PY\" - PARAGUAY
--
-- -   \"PE\" - PERU
--
-- -   \"PL\" - POLAND
--
-- -   \"PT\" - PORTUGAL
--
-- -   \"RO\" - ROMANIA
--
-- -   \"RU\" - RUSSIA
--
-- -   \"RS\" - SERBIA
--
-- -   \"SK\" - SLOVAKIA
--
-- -   \"SI\" - SLOVENIA
--
-- -   \"ZA\" - SOUTH AFRICA
--
-- -   \"ES\" - SPAIN
--
-- -   \"SE\" - SWEDEN
--
-- -   \"CH\" - SWITZERLAND
--
-- -   \"UA\" - UKRAINE
--
-- -   \"AE\" - UNITED ARAB EMIRATES
--
-- -   \"US\" - UNITED STATES
--
-- -   \"UK\" - UNITED KINGDOM
--
-- -   \"UY\" - URUGUAY
--
-- -   \"VE\" - VENEZUELA
additionalDataset_configuration :: Lens.Lens' AdditionalDataset (Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.NonEmpty Prelude.Text)))
additionalDataset_configuration = Lens.lens (\AdditionalDataset' {configuration} -> configuration) (\s@AdditionalDataset' {} a -> s {configuration = a} :: AdditionalDataset) Prelude.. Lens.mapping Lens.coerced

-- | The name of the additional dataset. Valid names: @\"holiday\"@ and
-- @\"weather\"@.
additionalDataset_name :: Lens.Lens' AdditionalDataset Prelude.Text
additionalDataset_name = Lens.lens (\AdditionalDataset' {name} -> name) (\s@AdditionalDataset' {} a -> s {name = a} :: AdditionalDataset)

instance Data.FromJSON AdditionalDataset where
  parseJSON =
    Data.withObject
      "AdditionalDataset"
      ( \x ->
          AdditionalDataset'
            Prelude.<$> (x Data..:? "Configuration" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "Name")
      )

instance Prelude.Hashable AdditionalDataset where
  hashWithSalt _salt AdditionalDataset' {..} =
    _salt
      `Prelude.hashWithSalt` configuration
      `Prelude.hashWithSalt` name

instance Prelude.NFData AdditionalDataset where
  rnf AdditionalDataset' {..} =
    Prelude.rnf configuration `Prelude.seq`
      Prelude.rnf name

instance Data.ToJSON AdditionalDataset where
  toJSON AdditionalDataset' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Configuration" Data..=) Prelude.<$> configuration,
            Prelude.Just ("Name" Data..= name)
          ]
      )
