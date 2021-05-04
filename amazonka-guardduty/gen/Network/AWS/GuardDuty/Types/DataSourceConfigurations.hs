{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.GuardDuty.Types.DataSourceConfigurations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.DataSourceConfigurations where

import Network.AWS.GuardDuty.Types.S3LogsConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about which data sources are enabled.
--
-- /See:/ 'newDataSourceConfigurations' smart constructor.
data DataSourceConfigurations = DataSourceConfigurations'
  { -- | Describes whether S3 data event logs are enabled as a data source.
    s3Logs :: Prelude.Maybe S3LogsConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DataSourceConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Logs', 'dataSourceConfigurations_s3Logs' - Describes whether S3 data event logs are enabled as a data source.
newDataSourceConfigurations ::
  DataSourceConfigurations
newDataSourceConfigurations =
  DataSourceConfigurations' {s3Logs = Prelude.Nothing}

-- | Describes whether S3 data event logs are enabled as a data source.
dataSourceConfigurations_s3Logs :: Lens.Lens' DataSourceConfigurations (Prelude.Maybe S3LogsConfiguration)
dataSourceConfigurations_s3Logs = Lens.lens (\DataSourceConfigurations' {s3Logs} -> s3Logs) (\s@DataSourceConfigurations' {} a -> s {s3Logs = a} :: DataSourceConfigurations)

instance Prelude.Hashable DataSourceConfigurations

instance Prelude.NFData DataSourceConfigurations

instance Prelude.ToJSON DataSourceConfigurations where
  toJSON DataSourceConfigurations' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("s3Logs" Prelude..=) Prelude.<$> s3Logs]
      )
