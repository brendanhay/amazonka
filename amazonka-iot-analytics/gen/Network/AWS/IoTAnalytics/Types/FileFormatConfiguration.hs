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

-- | Contains the configuration information of file formats. AWS IoT
-- Analytics data stores support JSON and
-- <https://parquet.apache.org/ Parquet>.
--
-- The default file format is JSON. You can specify only one format.
--
-- You can\'t change the file format after you create the data store.
--
-- /See:/ 'newFileFormatConfiguration' smart constructor.
data FileFormatConfiguration = FileFormatConfiguration'
  { -- | Contains the configuration information of the Parquet format.
    parquetConfiguration :: Core.Maybe ParquetConfiguration,
    -- | Contains the configuration information of the JSON format.
    jsonConfiguration :: Core.Maybe JsonConfiguration
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      jsonConfiguration = Core.Nothing
    }

-- | Contains the configuration information of the Parquet format.
fileFormatConfiguration_parquetConfiguration :: Lens.Lens' FileFormatConfiguration (Core.Maybe ParquetConfiguration)
fileFormatConfiguration_parquetConfiguration = Lens.lens (\FileFormatConfiguration' {parquetConfiguration} -> parquetConfiguration) (\s@FileFormatConfiguration' {} a -> s {parquetConfiguration = a} :: FileFormatConfiguration)

-- | Contains the configuration information of the JSON format.
fileFormatConfiguration_jsonConfiguration :: Lens.Lens' FileFormatConfiguration (Core.Maybe JsonConfiguration)
fileFormatConfiguration_jsonConfiguration = Lens.lens (\FileFormatConfiguration' {jsonConfiguration} -> jsonConfiguration) (\s@FileFormatConfiguration' {} a -> s {jsonConfiguration = a} :: FileFormatConfiguration)

instance Core.FromJSON FileFormatConfiguration where
  parseJSON =
    Core.withObject
      "FileFormatConfiguration"
      ( \x ->
          FileFormatConfiguration'
            Core.<$> (x Core..:? "parquetConfiguration")
            Core.<*> (x Core..:? "jsonConfiguration")
      )

instance Core.Hashable FileFormatConfiguration

instance Core.NFData FileFormatConfiguration

instance Core.ToJSON FileFormatConfiguration where
  toJSON FileFormatConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("parquetConfiguration" Core..=)
              Core.<$> parquetConfiguration,
            ("jsonConfiguration" Core..=)
              Core.<$> jsonConfiguration
          ]
      )
