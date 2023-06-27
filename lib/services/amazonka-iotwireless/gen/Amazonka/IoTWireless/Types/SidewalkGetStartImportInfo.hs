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
-- Module      : Amazonka.IoTWireless.Types.SidewalkGetStartImportInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.SidewalkGetStartImportInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Sidewalk-related information for devices in an import task that are
-- being onboarded.
--
-- /See:/ 'newSidewalkGetStartImportInfo' smart constructor.
data SidewalkGetStartImportInfo = SidewalkGetStartImportInfo'
  { -- | List of Sidewalk devices that are added to the import task.
    deviceCreationFileList :: Prelude.Maybe [Prelude.Text],
    -- | The IAM role that allows AWS IoT Wireless to access the CSV file in the
    -- S3 bucket.
    role' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SidewalkGetStartImportInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceCreationFileList', 'sidewalkGetStartImportInfo_deviceCreationFileList' - List of Sidewalk devices that are added to the import task.
--
-- 'role'', 'sidewalkGetStartImportInfo_role' - The IAM role that allows AWS IoT Wireless to access the CSV file in the
-- S3 bucket.
newSidewalkGetStartImportInfo ::
  SidewalkGetStartImportInfo
newSidewalkGetStartImportInfo =
  SidewalkGetStartImportInfo'
    { deviceCreationFileList =
        Prelude.Nothing,
      role' = Prelude.Nothing
    }

-- | List of Sidewalk devices that are added to the import task.
sidewalkGetStartImportInfo_deviceCreationFileList :: Lens.Lens' SidewalkGetStartImportInfo (Prelude.Maybe [Prelude.Text])
sidewalkGetStartImportInfo_deviceCreationFileList = Lens.lens (\SidewalkGetStartImportInfo' {deviceCreationFileList} -> deviceCreationFileList) (\s@SidewalkGetStartImportInfo' {} a -> s {deviceCreationFileList = a} :: SidewalkGetStartImportInfo) Prelude.. Lens.mapping Lens.coerced

-- | The IAM role that allows AWS IoT Wireless to access the CSV file in the
-- S3 bucket.
sidewalkGetStartImportInfo_role :: Lens.Lens' SidewalkGetStartImportInfo (Prelude.Maybe Prelude.Text)
sidewalkGetStartImportInfo_role = Lens.lens (\SidewalkGetStartImportInfo' {role'} -> role') (\s@SidewalkGetStartImportInfo' {} a -> s {role' = a} :: SidewalkGetStartImportInfo)

instance Data.FromJSON SidewalkGetStartImportInfo where
  parseJSON =
    Data.withObject
      "SidewalkGetStartImportInfo"
      ( \x ->
          SidewalkGetStartImportInfo'
            Prelude.<$> ( x
                            Data..:? "DeviceCreationFileList"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Role")
      )

instance Prelude.Hashable SidewalkGetStartImportInfo where
  hashWithSalt _salt SidewalkGetStartImportInfo' {..} =
    _salt
      `Prelude.hashWithSalt` deviceCreationFileList
      `Prelude.hashWithSalt` role'

instance Prelude.NFData SidewalkGetStartImportInfo where
  rnf SidewalkGetStartImportInfo' {..} =
    Prelude.rnf deviceCreationFileList
      `Prelude.seq` Prelude.rnf role'
