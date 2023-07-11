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
-- Module      : Amazonka.HoneyCode.Types.ImportDataSourceConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.HoneyCode.Types.ImportDataSourceConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that contains the configuration parameters for the data source
-- of an import request.
--
-- /See:/ 'newImportDataSourceConfig' smart constructor.
data ImportDataSourceConfig = ImportDataSourceConfig'
  { -- | The URL from which source data will be downloaded for the import
    -- request.
    dataSourceUrl :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportDataSourceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSourceUrl', 'importDataSourceConfig_dataSourceUrl' - The URL from which source data will be downloaded for the import
-- request.
newImportDataSourceConfig ::
  ImportDataSourceConfig
newImportDataSourceConfig =
  ImportDataSourceConfig'
    { dataSourceUrl =
        Prelude.Nothing
    }

-- | The URL from which source data will be downloaded for the import
-- request.
importDataSourceConfig_dataSourceUrl :: Lens.Lens' ImportDataSourceConfig (Prelude.Maybe Prelude.Text)
importDataSourceConfig_dataSourceUrl = Lens.lens (\ImportDataSourceConfig' {dataSourceUrl} -> dataSourceUrl) (\s@ImportDataSourceConfig' {} a -> s {dataSourceUrl = a} :: ImportDataSourceConfig) Prelude.. Lens.mapping Data._Sensitive

instance Data.FromJSON ImportDataSourceConfig where
  parseJSON =
    Data.withObject
      "ImportDataSourceConfig"
      ( \x ->
          ImportDataSourceConfig'
            Prelude.<$> (x Data..:? "dataSourceUrl")
      )

instance Prelude.Hashable ImportDataSourceConfig where
  hashWithSalt _salt ImportDataSourceConfig' {..} =
    _salt `Prelude.hashWithSalt` dataSourceUrl

instance Prelude.NFData ImportDataSourceConfig where
  rnf ImportDataSourceConfig' {..} =
    Prelude.rnf dataSourceUrl

instance Data.ToJSON ImportDataSourceConfig where
  toJSON ImportDataSourceConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("dataSourceUrl" Data..=)
              Prelude.<$> dataSourceUrl
          ]
      )
