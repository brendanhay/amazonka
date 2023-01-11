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
-- Module      : Amazonka.ChimeSdkVoice.Types.GeoMatchParams
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkVoice.Types.GeoMatchParams where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newGeoMatchParams' smart constructor.
data GeoMatchParams = GeoMatchParams'
  { country :: Prelude.Text,
    areaCode :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GeoMatchParams' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'country', 'geoMatchParams_country' - Undocumented member.
--
-- 'areaCode', 'geoMatchParams_areaCode' - Undocumented member.
newGeoMatchParams ::
  -- | 'country'
  Prelude.Text ->
  -- | 'areaCode'
  Prelude.Text ->
  GeoMatchParams
newGeoMatchParams pCountry_ pAreaCode_ =
  GeoMatchParams'
    { country = pCountry_,
      areaCode = pAreaCode_
    }

-- | Undocumented member.
geoMatchParams_country :: Lens.Lens' GeoMatchParams Prelude.Text
geoMatchParams_country = Lens.lens (\GeoMatchParams' {country} -> country) (\s@GeoMatchParams' {} a -> s {country = a} :: GeoMatchParams)

-- | Undocumented member.
geoMatchParams_areaCode :: Lens.Lens' GeoMatchParams Prelude.Text
geoMatchParams_areaCode = Lens.lens (\GeoMatchParams' {areaCode} -> areaCode) (\s@GeoMatchParams' {} a -> s {areaCode = a} :: GeoMatchParams)

instance Data.FromJSON GeoMatchParams where
  parseJSON =
    Data.withObject
      "GeoMatchParams"
      ( \x ->
          GeoMatchParams'
            Prelude.<$> (x Data..: "Country")
            Prelude.<*> (x Data..: "AreaCode")
      )

instance Prelude.Hashable GeoMatchParams where
  hashWithSalt _salt GeoMatchParams' {..} =
    _salt `Prelude.hashWithSalt` country
      `Prelude.hashWithSalt` areaCode

instance Prelude.NFData GeoMatchParams where
  rnf GeoMatchParams' {..} =
    Prelude.rnf country
      `Prelude.seq` Prelude.rnf areaCode

instance Data.ToJSON GeoMatchParams where
  toJSON GeoMatchParams' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Country" Data..= country),
            Prelude.Just ("AreaCode" Data..= areaCode)
          ]
      )
