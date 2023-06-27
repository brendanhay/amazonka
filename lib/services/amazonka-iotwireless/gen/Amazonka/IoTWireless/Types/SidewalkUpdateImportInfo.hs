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
-- Module      : Amazonka.IoTWireless.Types.SidewalkUpdateImportInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.SidewalkUpdateImportInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Sidewalk object information for updating an import task.
--
-- /See:/ 'newSidewalkUpdateImportInfo' smart constructor.
data SidewalkUpdateImportInfo = SidewalkUpdateImportInfo'
  { -- | The CSV file contained in an S3 bucket that\'s used for appending
    -- devices to an existing import task.
    deviceCreationFile :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SidewalkUpdateImportInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceCreationFile', 'sidewalkUpdateImportInfo_deviceCreationFile' - The CSV file contained in an S3 bucket that\'s used for appending
-- devices to an existing import task.
newSidewalkUpdateImportInfo ::
  SidewalkUpdateImportInfo
newSidewalkUpdateImportInfo =
  SidewalkUpdateImportInfo'
    { deviceCreationFile =
        Prelude.Nothing
    }

-- | The CSV file contained in an S3 bucket that\'s used for appending
-- devices to an existing import task.
sidewalkUpdateImportInfo_deviceCreationFile :: Lens.Lens' SidewalkUpdateImportInfo (Prelude.Maybe Prelude.Text)
sidewalkUpdateImportInfo_deviceCreationFile = Lens.lens (\SidewalkUpdateImportInfo' {deviceCreationFile} -> deviceCreationFile) (\s@SidewalkUpdateImportInfo' {} a -> s {deviceCreationFile = a} :: SidewalkUpdateImportInfo)

instance Prelude.Hashable SidewalkUpdateImportInfo where
  hashWithSalt _salt SidewalkUpdateImportInfo' {..} =
    _salt `Prelude.hashWithSalt` deviceCreationFile

instance Prelude.NFData SidewalkUpdateImportInfo where
  rnf SidewalkUpdateImportInfo' {..} =
    Prelude.rnf deviceCreationFile

instance Data.ToJSON SidewalkUpdateImportInfo where
  toJSON SidewalkUpdateImportInfo' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DeviceCreationFile" Data..=)
              Prelude.<$> deviceCreationFile
          ]
      )
