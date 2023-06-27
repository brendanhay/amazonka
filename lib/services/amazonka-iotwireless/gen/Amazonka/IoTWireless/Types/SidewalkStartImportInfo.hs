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
-- Module      : Amazonka.IoTWireless.Types.SidewalkStartImportInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.SidewalkStartImportInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about an import task created for bulk provisioning.
--
-- /See:/ 'newSidewalkStartImportInfo' smart constructor.
data SidewalkStartImportInfo = SidewalkStartImportInfo'
  { -- | The CSV file contained in an S3 bucket that\'s used for adding devices
    -- to an import task.
    deviceCreationFile :: Prelude.Maybe Prelude.Text,
    -- | The IAM role that allows AWS IoT Wireless to access the CSV file in the
    -- S3 bucket.
    role' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SidewalkStartImportInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceCreationFile', 'sidewalkStartImportInfo_deviceCreationFile' - The CSV file contained in an S3 bucket that\'s used for adding devices
-- to an import task.
--
-- 'role'', 'sidewalkStartImportInfo_role' - The IAM role that allows AWS IoT Wireless to access the CSV file in the
-- S3 bucket.
newSidewalkStartImportInfo ::
  SidewalkStartImportInfo
newSidewalkStartImportInfo =
  SidewalkStartImportInfo'
    { deviceCreationFile =
        Prelude.Nothing,
      role' = Prelude.Nothing
    }

-- | The CSV file contained in an S3 bucket that\'s used for adding devices
-- to an import task.
sidewalkStartImportInfo_deviceCreationFile :: Lens.Lens' SidewalkStartImportInfo (Prelude.Maybe Prelude.Text)
sidewalkStartImportInfo_deviceCreationFile = Lens.lens (\SidewalkStartImportInfo' {deviceCreationFile} -> deviceCreationFile) (\s@SidewalkStartImportInfo' {} a -> s {deviceCreationFile = a} :: SidewalkStartImportInfo)

-- | The IAM role that allows AWS IoT Wireless to access the CSV file in the
-- S3 bucket.
sidewalkStartImportInfo_role :: Lens.Lens' SidewalkStartImportInfo (Prelude.Maybe Prelude.Text)
sidewalkStartImportInfo_role = Lens.lens (\SidewalkStartImportInfo' {role'} -> role') (\s@SidewalkStartImportInfo' {} a -> s {role' = a} :: SidewalkStartImportInfo)

instance Prelude.Hashable SidewalkStartImportInfo where
  hashWithSalt _salt SidewalkStartImportInfo' {..} =
    _salt
      `Prelude.hashWithSalt` deviceCreationFile
      `Prelude.hashWithSalt` role'

instance Prelude.NFData SidewalkStartImportInfo where
  rnf SidewalkStartImportInfo' {..} =
    Prelude.rnf deviceCreationFile
      `Prelude.seq` Prelude.rnf role'

instance Data.ToJSON SidewalkStartImportInfo where
  toJSON SidewalkStartImportInfo' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DeviceCreationFile" Data..=)
              Prelude.<$> deviceCreationFile,
            ("Role" Data..=) Prelude.<$> role'
          ]
      )
