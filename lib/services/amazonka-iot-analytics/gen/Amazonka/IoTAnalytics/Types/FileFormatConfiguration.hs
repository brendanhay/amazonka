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
-- Module      : Amazonka.IoTAnalytics.Types.FileFormatConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.FileFormatConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTAnalytics.Types.JsonConfiguration
import Amazonka.IoTAnalytics.Types.ParquetConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Contains the configuration information of file formats. IoT Analytics
-- data stores support JSON and <https://parquet.apache.org/ Parquet>.
--
-- The default file format is JSON. You can specify only one format.
--
-- You can\'t change the file format after you create the data store.
--
-- /See:/ 'newFileFormatConfiguration' smart constructor.
data FileFormatConfiguration = FileFormatConfiguration'
  { -- | Contains the configuration information of the JSON format.
    jsonConfiguration :: Prelude.Maybe JsonConfiguration,
    -- | Contains the configuration information of the Parquet format.
    parquetConfiguration :: Prelude.Maybe ParquetConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FileFormatConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jsonConfiguration', 'fileFormatConfiguration_jsonConfiguration' - Contains the configuration information of the JSON format.
--
-- 'parquetConfiguration', 'fileFormatConfiguration_parquetConfiguration' - Contains the configuration information of the Parquet format.
newFileFormatConfiguration ::
  FileFormatConfiguration
newFileFormatConfiguration =
  FileFormatConfiguration'
    { jsonConfiguration =
        Prelude.Nothing,
      parquetConfiguration = Prelude.Nothing
    }

-- | Contains the configuration information of the JSON format.
fileFormatConfiguration_jsonConfiguration :: Lens.Lens' FileFormatConfiguration (Prelude.Maybe JsonConfiguration)
fileFormatConfiguration_jsonConfiguration = Lens.lens (\FileFormatConfiguration' {jsonConfiguration} -> jsonConfiguration) (\s@FileFormatConfiguration' {} a -> s {jsonConfiguration = a} :: FileFormatConfiguration)

-- | Contains the configuration information of the Parquet format.
fileFormatConfiguration_parquetConfiguration :: Lens.Lens' FileFormatConfiguration (Prelude.Maybe ParquetConfiguration)
fileFormatConfiguration_parquetConfiguration = Lens.lens (\FileFormatConfiguration' {parquetConfiguration} -> parquetConfiguration) (\s@FileFormatConfiguration' {} a -> s {parquetConfiguration = a} :: FileFormatConfiguration)

instance Data.FromJSON FileFormatConfiguration where
  parseJSON =
    Data.withObject
      "FileFormatConfiguration"
      ( \x ->
          FileFormatConfiguration'
            Prelude.<$> (x Data..:? "jsonConfiguration")
            Prelude.<*> (x Data..:? "parquetConfiguration")
      )

instance Prelude.Hashable FileFormatConfiguration where
  hashWithSalt _salt FileFormatConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` jsonConfiguration
      `Prelude.hashWithSalt` parquetConfiguration

instance Prelude.NFData FileFormatConfiguration where
  rnf FileFormatConfiguration' {..} =
    Prelude.rnf jsonConfiguration `Prelude.seq`
      Prelude.rnf parquetConfiguration

instance Data.ToJSON FileFormatConfiguration where
  toJSON FileFormatConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("jsonConfiguration" Data..=)
              Prelude.<$> jsonConfiguration,
            ("parquetConfiguration" Data..=)
              Prelude.<$> parquetConfiguration
          ]
      )
