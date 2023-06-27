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
-- Module      : Amazonka.Account.Types.Region
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Account.Types.Region where

import Amazonka.Account.Types.RegionOptStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | This is a structure that expresses the Region for a given account,
-- consisting of a name and opt-in status.
--
-- /See:/ 'newRegion' smart constructor.
data Region = Region'
  { -- | The Region code of a given Region (for example, @us-east-1@).
    regionName :: Prelude.Maybe Prelude.Text,
    -- | One of potential statuses a Region can undergo (Enabled, Enabling,
    -- Disabled, Disabling, Enabled_By_Default).
    regionOptStatus :: Prelude.Maybe RegionOptStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Region' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regionName', 'region_regionName' - The Region code of a given Region (for example, @us-east-1@).
--
-- 'regionOptStatus', 'region_regionOptStatus' - One of potential statuses a Region can undergo (Enabled, Enabling,
-- Disabled, Disabling, Enabled_By_Default).
newRegion ::
  Region
newRegion =
  Region'
    { regionName = Prelude.Nothing,
      regionOptStatus = Prelude.Nothing
    }

-- | The Region code of a given Region (for example, @us-east-1@).
region_regionName :: Lens.Lens' Region (Prelude.Maybe Prelude.Text)
region_regionName = Lens.lens (\Region' {regionName} -> regionName) (\s@Region' {} a -> s {regionName = a} :: Region)

-- | One of potential statuses a Region can undergo (Enabled, Enabling,
-- Disabled, Disabling, Enabled_By_Default).
region_regionOptStatus :: Lens.Lens' Region (Prelude.Maybe RegionOptStatus)
region_regionOptStatus = Lens.lens (\Region' {regionOptStatus} -> regionOptStatus) (\s@Region' {} a -> s {regionOptStatus = a} :: Region)

instance Data.FromJSON Region where
  parseJSON =
    Data.withObject
      "Region"
      ( \x ->
          Region'
            Prelude.<$> (x Data..:? "RegionName")
            Prelude.<*> (x Data..:? "RegionOptStatus")
      )

instance Prelude.Hashable Region where
  hashWithSalt _salt Region' {..} =
    _salt
      `Prelude.hashWithSalt` regionName
      `Prelude.hashWithSalt` regionOptStatus

instance Prelude.NFData Region where
  rnf Region' {..} =
    Prelude.rnf regionName
      `Prelude.seq` Prelude.rnf regionOptStatus
