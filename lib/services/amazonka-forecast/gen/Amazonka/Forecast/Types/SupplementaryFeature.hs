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
-- Module      : Amazonka.Forecast.Types.SupplementaryFeature
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.SupplementaryFeature where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | This object belongs to the CreatePredictor operation. If you created
-- your predictor with CreateAutoPredictor, see AdditionalDataset.
--
-- Describes a supplementary feature of a dataset group. This object is
-- part of the InputDataConfig object. Forecast supports the Weather Index
-- and Holidays built-in featurizations.
--
-- __Weather Index__
--
-- The Amazon Forecast Weather Index is a built-in featurization that
-- incorporates historical and projected weather information into your
-- model. The Weather Index supplements your datasets with over two years
-- of historical weather data and up to 14 days of projected weather data.
-- For more information, see
-- <https://docs.aws.amazon.com/forecast/latest/dg/weather.html Amazon Forecast Weather Index>.
--
-- __Holidays__
--
-- Holidays is a built-in featurization that incorporates a
-- feature-engineered dataset of national holiday information into your
-- model. It provides native support for the holiday calendars of 66
-- countries. To view the holiday calendars, refer to the
-- <http://jollyday.sourceforge.net/data.html Jollyday> library. For more
-- information, see
-- <https://docs.aws.amazon.com/forecast/latest/dg/holidays.html Holidays Featurization>.
--
-- /See:/ 'newSupplementaryFeature' smart constructor.
data SupplementaryFeature = SupplementaryFeature'
  { -- | The name of the feature. Valid values: @\"holiday\"@ and @\"weather\"@.
    name :: Prelude.Text,
    -- | __Weather Index__
    --
    -- To enable the Weather Index, set the value to @\"true\"@
    --
    -- __Holidays__
    --
    -- To enable Holidays, specify a country with one of the following
    -- two-letter country codes:
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
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SupplementaryFeature' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'supplementaryFeature_name' - The name of the feature. Valid values: @\"holiday\"@ and @\"weather\"@.
--
-- 'value', 'supplementaryFeature_value' - __Weather Index__
--
-- To enable the Weather Index, set the value to @\"true\"@
--
-- __Holidays__
--
-- To enable Holidays, specify a country with one of the following
-- two-letter country codes:
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
newSupplementaryFeature ::
  -- | 'name'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  SupplementaryFeature
newSupplementaryFeature pName_ pValue_ =
  SupplementaryFeature'
    { name = pName_,
      value = pValue_
    }

-- | The name of the feature. Valid values: @\"holiday\"@ and @\"weather\"@.
supplementaryFeature_name :: Lens.Lens' SupplementaryFeature Prelude.Text
supplementaryFeature_name = Lens.lens (\SupplementaryFeature' {name} -> name) (\s@SupplementaryFeature' {} a -> s {name = a} :: SupplementaryFeature)

-- | __Weather Index__
--
-- To enable the Weather Index, set the value to @\"true\"@
--
-- __Holidays__
--
-- To enable Holidays, specify a country with one of the following
-- two-letter country codes:
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
supplementaryFeature_value :: Lens.Lens' SupplementaryFeature Prelude.Text
supplementaryFeature_value = Lens.lens (\SupplementaryFeature' {value} -> value) (\s@SupplementaryFeature' {} a -> s {value = a} :: SupplementaryFeature)

instance Data.FromJSON SupplementaryFeature where
  parseJSON =
    Data.withObject
      "SupplementaryFeature"
      ( \x ->
          SupplementaryFeature'
            Prelude.<$> (x Data..: "Name") Prelude.<*> (x Data..: "Value")
      )

instance Prelude.Hashable SupplementaryFeature where
  hashWithSalt _salt SupplementaryFeature' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData SupplementaryFeature where
  rnf SupplementaryFeature' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf value

instance Data.ToJSON SupplementaryFeature where
  toJSON SupplementaryFeature' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Value" Data..= value)
          ]
      )
