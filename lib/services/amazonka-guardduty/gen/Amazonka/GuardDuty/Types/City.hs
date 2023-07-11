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
-- Module      : Amazonka.GuardDuty.Types.City
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.City where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the city associated with the IP address.
--
-- /See:/ 'newCity' smart constructor.
data City = City'
  { -- | The city name of the remote IP address.
    cityName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'City' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cityName', 'city_cityName' - The city name of the remote IP address.
newCity ::
  City
newCity = City' {cityName = Prelude.Nothing}

-- | The city name of the remote IP address.
city_cityName :: Lens.Lens' City (Prelude.Maybe Prelude.Text)
city_cityName = Lens.lens (\City' {cityName} -> cityName) (\s@City' {} a -> s {cityName = a} :: City)

instance Data.FromJSON City where
  parseJSON =
    Data.withObject
      "City"
      (\x -> City' Prelude.<$> (x Data..:? "cityName"))

instance Prelude.Hashable City where
  hashWithSalt _salt City' {..} =
    _salt `Prelude.hashWithSalt` cityName

instance Prelude.NFData City where
  rnf City' {..} = Prelude.rnf cityName
