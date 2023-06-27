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
-- Module      : Amazonka.FMS.Types.RegionScope
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.RegionScope where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Defines the Amazon Web Services Regions that the specified Firewall
-- Manager administrator can manage.
--
-- /See:/ 'newRegionScope' smart constructor.
data RegionScope = RegionScope'
  { -- | Allows the specified Firewall Manager administrator to manage all Amazon
    -- Web Services Regions.
    allRegionsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Web Services Regions that the specified Firewall Manager
    -- administrator can perform actions in.
    regions :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegionScope' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allRegionsEnabled', 'regionScope_allRegionsEnabled' - Allows the specified Firewall Manager administrator to manage all Amazon
-- Web Services Regions.
--
-- 'regions', 'regionScope_regions' - The Amazon Web Services Regions that the specified Firewall Manager
-- administrator can perform actions in.
newRegionScope ::
  RegionScope
newRegionScope =
  RegionScope'
    { allRegionsEnabled = Prelude.Nothing,
      regions = Prelude.Nothing
    }

-- | Allows the specified Firewall Manager administrator to manage all Amazon
-- Web Services Regions.
regionScope_allRegionsEnabled :: Lens.Lens' RegionScope (Prelude.Maybe Prelude.Bool)
regionScope_allRegionsEnabled = Lens.lens (\RegionScope' {allRegionsEnabled} -> allRegionsEnabled) (\s@RegionScope' {} a -> s {allRegionsEnabled = a} :: RegionScope)

-- | The Amazon Web Services Regions that the specified Firewall Manager
-- administrator can perform actions in.
regionScope_regions :: Lens.Lens' RegionScope (Prelude.Maybe [Prelude.Text])
regionScope_regions = Lens.lens (\RegionScope' {regions} -> regions) (\s@RegionScope' {} a -> s {regions = a} :: RegionScope) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON RegionScope where
  parseJSON =
    Data.withObject
      "RegionScope"
      ( \x ->
          RegionScope'
            Prelude.<$> (x Data..:? "AllRegionsEnabled")
            Prelude.<*> (x Data..:? "Regions" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable RegionScope where
  hashWithSalt _salt RegionScope' {..} =
    _salt
      `Prelude.hashWithSalt` allRegionsEnabled
      `Prelude.hashWithSalt` regions

instance Prelude.NFData RegionScope where
  rnf RegionScope' {..} =
    Prelude.rnf allRegionsEnabled
      `Prelude.seq` Prelude.rnf regions

instance Data.ToJSON RegionScope where
  toJSON RegionScope' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AllRegionsEnabled" Data..=)
              Prelude.<$> allRegionsEnabled,
            ("Regions" Data..=) Prelude.<$> regions
          ]
      )
