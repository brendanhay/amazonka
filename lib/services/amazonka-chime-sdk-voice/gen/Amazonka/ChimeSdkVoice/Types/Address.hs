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
-- Module      : Amazonka.ChimeSdkVoice.Types.Address
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkVoice.Types.Address where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newAddress' smart constructor.
data Address = Address'
  { city :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    country :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    postDirectional :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    postalCode :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    postalCodePlus4 :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    preDirectional :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    state :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    streetName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    streetNumber :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    streetSuffix :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Address' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'city', 'address_city' - Undocumented member.
--
-- 'country', 'address_country' - Undocumented member.
--
-- 'postDirectional', 'address_postDirectional' - Undocumented member.
--
-- 'postalCode', 'address_postalCode' - Undocumented member.
--
-- 'postalCodePlus4', 'address_postalCodePlus4' - Undocumented member.
--
-- 'preDirectional', 'address_preDirectional' - Undocumented member.
--
-- 'state', 'address_state' - Undocumented member.
--
-- 'streetName', 'address_streetName' - Undocumented member.
--
-- 'streetNumber', 'address_streetNumber' - Undocumented member.
--
-- 'streetSuffix', 'address_streetSuffix' - Undocumented member.
newAddress ::
  Address
newAddress =
  Address'
    { city = Prelude.Nothing,
      country = Prelude.Nothing,
      postDirectional = Prelude.Nothing,
      postalCode = Prelude.Nothing,
      postalCodePlus4 = Prelude.Nothing,
      preDirectional = Prelude.Nothing,
      state = Prelude.Nothing,
      streetName = Prelude.Nothing,
      streetNumber = Prelude.Nothing,
      streetSuffix = Prelude.Nothing
    }

-- | Undocumented member.
address_city :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_city = Lens.lens (\Address' {city} -> city) (\s@Address' {} a -> s {city = a} :: Address) Prelude.. Lens.mapping Data._Sensitive

-- | Undocumented member.
address_country :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_country = Lens.lens (\Address' {country} -> country) (\s@Address' {} a -> s {country = a} :: Address) Prelude.. Lens.mapping Data._Sensitive

-- | Undocumented member.
address_postDirectional :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_postDirectional = Lens.lens (\Address' {postDirectional} -> postDirectional) (\s@Address' {} a -> s {postDirectional = a} :: Address) Prelude.. Lens.mapping Data._Sensitive

-- | Undocumented member.
address_postalCode :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_postalCode = Lens.lens (\Address' {postalCode} -> postalCode) (\s@Address' {} a -> s {postalCode = a} :: Address) Prelude.. Lens.mapping Data._Sensitive

-- | Undocumented member.
address_postalCodePlus4 :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_postalCodePlus4 = Lens.lens (\Address' {postalCodePlus4} -> postalCodePlus4) (\s@Address' {} a -> s {postalCodePlus4 = a} :: Address) Prelude.. Lens.mapping Data._Sensitive

-- | Undocumented member.
address_preDirectional :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_preDirectional = Lens.lens (\Address' {preDirectional} -> preDirectional) (\s@Address' {} a -> s {preDirectional = a} :: Address) Prelude.. Lens.mapping Data._Sensitive

-- | Undocumented member.
address_state :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_state = Lens.lens (\Address' {state} -> state) (\s@Address' {} a -> s {state = a} :: Address) Prelude.. Lens.mapping Data._Sensitive

-- | Undocumented member.
address_streetName :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_streetName = Lens.lens (\Address' {streetName} -> streetName) (\s@Address' {} a -> s {streetName = a} :: Address) Prelude.. Lens.mapping Data._Sensitive

-- | Undocumented member.
address_streetNumber :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_streetNumber = Lens.lens (\Address' {streetNumber} -> streetNumber) (\s@Address' {} a -> s {streetNumber = a} :: Address) Prelude.. Lens.mapping Data._Sensitive

-- | Undocumented member.
address_streetSuffix :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_streetSuffix = Lens.lens (\Address' {streetSuffix} -> streetSuffix) (\s@Address' {} a -> s {streetSuffix = a} :: Address) Prelude.. Lens.mapping Data._Sensitive

instance Data.FromJSON Address where
  parseJSON =
    Data.withObject
      "Address"
      ( \x ->
          Address'
            Prelude.<$> (x Data..:? "city")
            Prelude.<*> (x Data..:? "country")
            Prelude.<*> (x Data..:? "postDirectional")
            Prelude.<*> (x Data..:? "postalCode")
            Prelude.<*> (x Data..:? "postalCodePlus4")
            Prelude.<*> (x Data..:? "preDirectional")
            Prelude.<*> (x Data..:? "state")
            Prelude.<*> (x Data..:? "streetName")
            Prelude.<*> (x Data..:? "streetNumber")
            Prelude.<*> (x Data..:? "streetSuffix")
      )

instance Prelude.Hashable Address where
  hashWithSalt _salt Address' {..} =
    _salt `Prelude.hashWithSalt` city
      `Prelude.hashWithSalt` country
      `Prelude.hashWithSalt` postDirectional
      `Prelude.hashWithSalt` postalCode
      `Prelude.hashWithSalt` postalCodePlus4
      `Prelude.hashWithSalt` preDirectional
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` streetName
      `Prelude.hashWithSalt` streetNumber
      `Prelude.hashWithSalt` streetSuffix

instance Prelude.NFData Address where
  rnf Address' {..} =
    Prelude.rnf city
      `Prelude.seq` Prelude.rnf country
      `Prelude.seq` Prelude.rnf postDirectional
      `Prelude.seq` Prelude.rnf postalCode
      `Prelude.seq` Prelude.rnf postalCodePlus4
      `Prelude.seq` Prelude.rnf preDirectional
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf streetName
      `Prelude.seq` Prelude.rnf streetNumber
      `Prelude.seq` Prelude.rnf streetSuffix
