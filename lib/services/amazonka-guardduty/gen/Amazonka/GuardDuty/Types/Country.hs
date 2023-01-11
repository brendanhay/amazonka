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
-- Module      : Amazonka.GuardDuty.Types.Country
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.Country where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the country where the remote IP address is
-- located.
--
-- /See:/ 'newCountry' smart constructor.
data Country = Country'
  { -- | The country code of the remote IP address.
    countryCode :: Prelude.Maybe Prelude.Text,
    -- | The country name of the remote IP address.
    countryName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Country' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'countryCode', 'country_countryCode' - The country code of the remote IP address.
--
-- 'countryName', 'country_countryName' - The country name of the remote IP address.
newCountry ::
  Country
newCountry =
  Country'
    { countryCode = Prelude.Nothing,
      countryName = Prelude.Nothing
    }

-- | The country code of the remote IP address.
country_countryCode :: Lens.Lens' Country (Prelude.Maybe Prelude.Text)
country_countryCode = Lens.lens (\Country' {countryCode} -> countryCode) (\s@Country' {} a -> s {countryCode = a} :: Country)

-- | The country name of the remote IP address.
country_countryName :: Lens.Lens' Country (Prelude.Maybe Prelude.Text)
country_countryName = Lens.lens (\Country' {countryName} -> countryName) (\s@Country' {} a -> s {countryName = a} :: Country)

instance Data.FromJSON Country where
  parseJSON =
    Data.withObject
      "Country"
      ( \x ->
          Country'
            Prelude.<$> (x Data..:? "countryCode")
            Prelude.<*> (x Data..:? "countryName")
      )

instance Prelude.Hashable Country where
  hashWithSalt _salt Country' {..} =
    _salt `Prelude.hashWithSalt` countryCode
      `Prelude.hashWithSalt` countryName

instance Prelude.NFData Country where
  rnf Country' {..} =
    Prelude.rnf countryCode
      `Prelude.seq` Prelude.rnf countryName
