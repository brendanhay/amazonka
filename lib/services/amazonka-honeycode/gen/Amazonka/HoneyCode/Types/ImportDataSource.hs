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
-- Module      : Amazonka.HoneyCode.Types.ImportDataSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.HoneyCode.Types.ImportDataSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.HoneyCode.Types.ImportDataSourceConfig
import qualified Amazonka.Prelude as Prelude

-- | An object that has details about the source of the data that was
-- submitted for import.
--
-- /See:/ 'newImportDataSource' smart constructor.
data ImportDataSource = ImportDataSource'
  { -- | The configuration parameters for the data source of the import
    dataSourceConfig :: ImportDataSourceConfig
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportDataSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSourceConfig', 'importDataSource_dataSourceConfig' - The configuration parameters for the data source of the import
newImportDataSource ::
  -- | 'dataSourceConfig'
  ImportDataSourceConfig ->
  ImportDataSource
newImportDataSource pDataSourceConfig_ =
  ImportDataSource'
    { dataSourceConfig =
        pDataSourceConfig_
    }

-- | The configuration parameters for the data source of the import
importDataSource_dataSourceConfig :: Lens.Lens' ImportDataSource ImportDataSourceConfig
importDataSource_dataSourceConfig = Lens.lens (\ImportDataSource' {dataSourceConfig} -> dataSourceConfig) (\s@ImportDataSource' {} a -> s {dataSourceConfig = a} :: ImportDataSource)

instance Core.FromJSON ImportDataSource where
  parseJSON =
    Core.withObject
      "ImportDataSource"
      ( \x ->
          ImportDataSource'
            Prelude.<$> (x Core..: "dataSourceConfig")
      )

instance Prelude.Hashable ImportDataSource where
  hashWithSalt _salt ImportDataSource' {..} =
    _salt `Prelude.hashWithSalt` dataSourceConfig

instance Prelude.NFData ImportDataSource where
  rnf ImportDataSource' {..} =
    Prelude.rnf dataSourceConfig

instance Core.ToJSON ImportDataSource where
  toJSON ImportDataSource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("dataSourceConfig" Core..= dataSourceConfig)
          ]
      )
