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
-- Module      : Network.AWS.GuardDuty.Types.City
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.City where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about the city associated with the IP address.
--
-- /See:/ 'newCity' smart constructor.
data City = City'
  { -- | The city name of the remote IP address.
    cityName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON City where
  parseJSON =
    Prelude.withObject
      "City"
      (\x -> City' Prelude.<$> (x Prelude..:? "cityName"))

instance Prelude.Hashable City

instance Prelude.NFData City
