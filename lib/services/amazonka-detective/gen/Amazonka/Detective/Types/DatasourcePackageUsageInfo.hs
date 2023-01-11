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
-- Module      : Amazonka.Detective.Types.DatasourcePackageUsageInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Detective.Types.DatasourcePackageUsageInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information on the usage of a data source package in the behavior graph.
--
-- /See:/ 'newDatasourcePackageUsageInfo' smart constructor.
data DatasourcePackageUsageInfo = DatasourcePackageUsageInfo'
  { -- | Total volume of data in bytes per day ingested for a given data source
    -- package.
    volumeUsageInBytes :: Prelude.Maybe Prelude.Integer,
    -- | The data and time when the member account data volume was last updated.
    -- The value is an ISO8601 formatted string. For example,
    -- @2021-08-18T16:35:56.284Z@.
    volumeUsageUpdateTime :: Prelude.Maybe Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatasourcePackageUsageInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'volumeUsageInBytes', 'datasourcePackageUsageInfo_volumeUsageInBytes' - Total volume of data in bytes per day ingested for a given data source
-- package.
--
-- 'volumeUsageUpdateTime', 'datasourcePackageUsageInfo_volumeUsageUpdateTime' - The data and time when the member account data volume was last updated.
-- The value is an ISO8601 formatted string. For example,
-- @2021-08-18T16:35:56.284Z@.
newDatasourcePackageUsageInfo ::
  DatasourcePackageUsageInfo
newDatasourcePackageUsageInfo =
  DatasourcePackageUsageInfo'
    { volumeUsageInBytes =
        Prelude.Nothing,
      volumeUsageUpdateTime = Prelude.Nothing
    }

-- | Total volume of data in bytes per day ingested for a given data source
-- package.
datasourcePackageUsageInfo_volumeUsageInBytes :: Lens.Lens' DatasourcePackageUsageInfo (Prelude.Maybe Prelude.Integer)
datasourcePackageUsageInfo_volumeUsageInBytes = Lens.lens (\DatasourcePackageUsageInfo' {volumeUsageInBytes} -> volumeUsageInBytes) (\s@DatasourcePackageUsageInfo' {} a -> s {volumeUsageInBytes = a} :: DatasourcePackageUsageInfo)

-- | The data and time when the member account data volume was last updated.
-- The value is an ISO8601 formatted string. For example,
-- @2021-08-18T16:35:56.284Z@.
datasourcePackageUsageInfo_volumeUsageUpdateTime :: Lens.Lens' DatasourcePackageUsageInfo (Prelude.Maybe Prelude.UTCTime)
datasourcePackageUsageInfo_volumeUsageUpdateTime = Lens.lens (\DatasourcePackageUsageInfo' {volumeUsageUpdateTime} -> volumeUsageUpdateTime) (\s@DatasourcePackageUsageInfo' {} a -> s {volumeUsageUpdateTime = a} :: DatasourcePackageUsageInfo) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON DatasourcePackageUsageInfo where
  parseJSON =
    Data.withObject
      "DatasourcePackageUsageInfo"
      ( \x ->
          DatasourcePackageUsageInfo'
            Prelude.<$> (x Data..:? "VolumeUsageInBytes")
            Prelude.<*> (x Data..:? "VolumeUsageUpdateTime")
      )

instance Prelude.Hashable DatasourcePackageUsageInfo where
  hashWithSalt _salt DatasourcePackageUsageInfo' {..} =
    _salt `Prelude.hashWithSalt` volumeUsageInBytes
      `Prelude.hashWithSalt` volumeUsageUpdateTime

instance Prelude.NFData DatasourcePackageUsageInfo where
  rnf DatasourcePackageUsageInfo' {..} =
    Prelude.rnf volumeUsageInBytes
      `Prelude.seq` Prelude.rnf volumeUsageUpdateTime
