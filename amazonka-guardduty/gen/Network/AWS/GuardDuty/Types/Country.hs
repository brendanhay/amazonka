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
-- Module      : Network.AWS.GuardDuty.Types.Country
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.Country where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information about the country where the remote IP address is
-- located.
--
-- /See:/ 'newCountry' smart constructor.
data Country = Country'
  { -- | The country name of the remote IP address.
    countryName :: Core.Maybe Core.Text,
    -- | The country code of the remote IP address.
    countryCode :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Country' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'countryName', 'country_countryName' - The country name of the remote IP address.
--
-- 'countryCode', 'country_countryCode' - The country code of the remote IP address.
newCountry ::
  Country
newCountry =
  Country'
    { countryName = Core.Nothing,
      countryCode = Core.Nothing
    }

-- | The country name of the remote IP address.
country_countryName :: Lens.Lens' Country (Core.Maybe Core.Text)
country_countryName = Lens.lens (\Country' {countryName} -> countryName) (\s@Country' {} a -> s {countryName = a} :: Country)

-- | The country code of the remote IP address.
country_countryCode :: Lens.Lens' Country (Core.Maybe Core.Text)
country_countryCode = Lens.lens (\Country' {countryCode} -> countryCode) (\s@Country' {} a -> s {countryCode = a} :: Country)

instance Core.FromJSON Country where
  parseJSON =
    Core.withObject
      "Country"
      ( \x ->
          Country'
            Core.<$> (x Core..:? "countryName")
            Core.<*> (x Core..:? "countryCode")
      )

instance Core.Hashable Country

instance Core.NFData Country
