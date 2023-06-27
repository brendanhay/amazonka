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
-- Module      : Amazonka.LicenseManager.Types.LicenseUsage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManager.Types.LicenseUsage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManager.Types.EntitlementUsage
import qualified Amazonka.Prelude as Prelude

-- | Describes the entitlement usage associated with a license.
--
-- /See:/ 'newLicenseUsage' smart constructor.
data LicenseUsage = LicenseUsage'
  { -- | License entitlement usages.
    entitlementUsages :: Prelude.Maybe [EntitlementUsage]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LicenseUsage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entitlementUsages', 'licenseUsage_entitlementUsages' - License entitlement usages.
newLicenseUsage ::
  LicenseUsage
newLicenseUsage =
  LicenseUsage' {entitlementUsages = Prelude.Nothing}

-- | License entitlement usages.
licenseUsage_entitlementUsages :: Lens.Lens' LicenseUsage (Prelude.Maybe [EntitlementUsage])
licenseUsage_entitlementUsages = Lens.lens (\LicenseUsage' {entitlementUsages} -> entitlementUsages) (\s@LicenseUsage' {} a -> s {entitlementUsages = a} :: LicenseUsage) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON LicenseUsage where
  parseJSON =
    Data.withObject
      "LicenseUsage"
      ( \x ->
          LicenseUsage'
            Prelude.<$> ( x
                            Data..:? "EntitlementUsages"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable LicenseUsage where
  hashWithSalt _salt LicenseUsage' {..} =
    _salt `Prelude.hashWithSalt` entitlementUsages

instance Prelude.NFData LicenseUsage where
  rnf LicenseUsage' {..} = Prelude.rnf entitlementUsages
