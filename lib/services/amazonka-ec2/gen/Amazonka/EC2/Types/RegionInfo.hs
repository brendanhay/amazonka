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
-- Module      : Amazonka.EC2.Types.RegionInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.RegionInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes a Region.
--
-- /See:/ 'newRegionInfo' smart constructor.
data RegionInfo = RegionInfo'
  { -- | The Region opt-in status. The possible values are @opt-in-not-required@,
    -- @opted-in@, and @not-opted-in@.
    optInStatus :: Prelude.Maybe Prelude.Text,
    -- | The name of the Region.
    regionName :: Prelude.Maybe Prelude.Text,
    -- | The Region service endpoint.
    endpoint :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegionInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'optInStatus', 'regionInfo_optInStatus' - The Region opt-in status. The possible values are @opt-in-not-required@,
-- @opted-in@, and @not-opted-in@.
--
-- 'regionName', 'regionInfo_regionName' - The name of the Region.
--
-- 'endpoint', 'regionInfo_endpoint' - The Region service endpoint.
newRegionInfo ::
  RegionInfo
newRegionInfo =
  RegionInfo'
    { optInStatus = Prelude.Nothing,
      regionName = Prelude.Nothing,
      endpoint = Prelude.Nothing
    }

-- | The Region opt-in status. The possible values are @opt-in-not-required@,
-- @opted-in@, and @not-opted-in@.
regionInfo_optInStatus :: Lens.Lens' RegionInfo (Prelude.Maybe Prelude.Text)
regionInfo_optInStatus = Lens.lens (\RegionInfo' {optInStatus} -> optInStatus) (\s@RegionInfo' {} a -> s {optInStatus = a} :: RegionInfo)

-- | The name of the Region.
regionInfo_regionName :: Lens.Lens' RegionInfo (Prelude.Maybe Prelude.Text)
regionInfo_regionName = Lens.lens (\RegionInfo' {regionName} -> regionName) (\s@RegionInfo' {} a -> s {regionName = a} :: RegionInfo)

-- | The Region service endpoint.
regionInfo_endpoint :: Lens.Lens' RegionInfo (Prelude.Maybe Prelude.Text)
regionInfo_endpoint = Lens.lens (\RegionInfo' {endpoint} -> endpoint) (\s@RegionInfo' {} a -> s {endpoint = a} :: RegionInfo)

instance Data.FromXML RegionInfo where
  parseXML x =
    RegionInfo'
      Prelude.<$> (x Data..@? "optInStatus")
      Prelude.<*> (x Data..@? "regionName")
      Prelude.<*> (x Data..@? "regionEndpoint")

instance Prelude.Hashable RegionInfo where
  hashWithSalt _salt RegionInfo' {..} =
    _salt `Prelude.hashWithSalt` optInStatus
      `Prelude.hashWithSalt` regionName
      `Prelude.hashWithSalt` endpoint

instance Prelude.NFData RegionInfo where
  rnf RegionInfo' {..} =
    Prelude.rnf optInStatus
      `Prelude.seq` Prelude.rnf regionName
      `Prelude.seq` Prelude.rnf endpoint
