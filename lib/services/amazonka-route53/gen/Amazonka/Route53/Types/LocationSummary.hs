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
-- Module      : Amazonka.Route53.Types.LocationSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53.Types.LocationSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53.Internal

-- | A complex type that contains information about the CIDR location.
--
-- /See:/ 'newLocationSummary' smart constructor.
data LocationSummary = LocationSummary'
  { -- | A string that specifies a location name.
    locationName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LocationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'locationName', 'locationSummary_locationName' - A string that specifies a location name.
newLocationSummary ::
  LocationSummary
newLocationSummary =
  LocationSummary' {locationName = Prelude.Nothing}

-- | A string that specifies a location name.
locationSummary_locationName :: Lens.Lens' LocationSummary (Prelude.Maybe Prelude.Text)
locationSummary_locationName = Lens.lens (\LocationSummary' {locationName} -> locationName) (\s@LocationSummary' {} a -> s {locationName = a} :: LocationSummary)

instance Data.FromXML LocationSummary where
  parseXML x =
    LocationSummary'
      Prelude.<$> (x Data..@? "LocationName")

instance Prelude.Hashable LocationSummary where
  hashWithSalt _salt LocationSummary' {..} =
    _salt `Prelude.hashWithSalt` locationName

instance Prelude.NFData LocationSummary where
  rnf LocationSummary' {..} = Prelude.rnf locationName
