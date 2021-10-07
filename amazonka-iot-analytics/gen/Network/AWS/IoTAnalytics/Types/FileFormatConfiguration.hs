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
-- Module      : Network.AWS.IoTAnalytics.Types.FileFormatConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.FileFormatConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTAnalytics.Types.JsonConfiguration
import Network.AWS.IoTAnalytics.Types.ParquetConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains the configuration information of file formats. IoT Analytics
-- data stores support JSON and <https://parquet.apache.org/ Parquet>.
--
-- The default file format is JSON. You can specify only one format.
--
-- You can\'t change the file format after you create the data store.
--
-- /See:/ 'newFileFormatConfiguration' smart constructor.
data FileFormatConfiguration = FileFormatConfiguration'
  { -- | Contains the configuration information of the Parquet format.
    parquetConfiguration :: Prelude.Maybe ParquetConfiguration,
    -- | Contains the configuration information of the JSON format.
    jsonConfiguration :: Prelude.Maybe JsonConfiguration
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
-- 'parquetConfiguration', 'fileFormatConfiguration_parquetConfiguration' - Contains the configuration information of the Parquet format.
--
-- 'jsonConfiguration', 'fileFormatConfiguration_jsonConfiguration' - Contains the configuration information of the JSON format.
newFileFormatConfiguration ::
  FileFormatConfiguration
newFileFormatConfiguration =
  FileFormatConfiguration'
    { parquetConfiguration =
        Prelude.Nothing,
      jsonConfiguration = Prelude.Nothing
    }

-- | Contains the configuration information of the Parquet format.
fileFormatConfiguration_parquetConfiguration :: Lens.Lens' FileFormatConfiguration (Prelude.Maybe ParquetConfiguration)
fileFormatConfiguration_parquetConfiguration = Lens.lens (\FileFormatConfiguration' {parquetConfiguration} -> parquetConfiguration) (\s@FileFormatConfiguration' {} a -> s {parquetConfiguration = a} :: FileFormatConfiguration)

-- | Contains the configuration information of the JSON format.
fileFormatConfiguration_jsonConfiguration :: Lens.Lens' FileFormatConfiguration (Prelude.Maybe JsonConfiguration)
fileFormatConfiguration_jsonConfiguration = Lens.lens (\FileFormatConfiguration' {jsonConfiguration} -> jsonConfiguration) (\s@FileFormatConfiguration' {} a -> s {jsonConfiguration = a} :: FileFormatConfiguration)

instance Core.FromJSON FileFormatConfiguration where
  parseJSON =
    Core.withObject
      "FileFormatConfiguration"
      ( \x ->
          FileFormatConfiguration'
            Prelude.<$> (x Core..:? "parquetConfiguration")
            Prelude.<*> (x Core..:? "jsonConfiguration")
      )

instance Prelude.Hashable FileFormatConfiguration

instance Prelude.NFData FileFormatConfiguration

instance Core.ToJSON FileFormatConfiguration where
  toJSON FileFormatConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("parquetConfiguration" Core..=)
              Prelude.<$> parquetConfiguration,
            ("jsonConfiguration" Core..=)
              Prelude.<$> jsonConfiguration
          ]
      )
