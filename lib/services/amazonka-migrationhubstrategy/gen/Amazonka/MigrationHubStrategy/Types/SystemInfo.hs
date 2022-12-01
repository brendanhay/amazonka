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
-- Module      : Amazonka.MigrationHubStrategy.Types.SystemInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.SystemInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MigrationHubStrategy.Types.NetworkInfo
import Amazonka.MigrationHubStrategy.Types.OSInfo
import qualified Amazonka.Prelude as Prelude

-- | Information about the server that hosts application components.
--
-- /See:/ 'newSystemInfo' smart constructor.
data SystemInfo = SystemInfo'
  { -- | Operating system corresponding to a server.
    osInfo :: Prelude.Maybe OSInfo,
    -- | Networking information related to a server.
    networkInfoList :: Prelude.Maybe [NetworkInfo],
    -- | File system type for the server.
    fileSystemType :: Prelude.Maybe Prelude.Text,
    -- | CPU architecture type for the server.
    cpuArchitecture :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SystemInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'osInfo', 'systemInfo_osInfo' - Operating system corresponding to a server.
--
-- 'networkInfoList', 'systemInfo_networkInfoList' - Networking information related to a server.
--
-- 'fileSystemType', 'systemInfo_fileSystemType' - File system type for the server.
--
-- 'cpuArchitecture', 'systemInfo_cpuArchitecture' - CPU architecture type for the server.
newSystemInfo ::
  SystemInfo
newSystemInfo =
  SystemInfo'
    { osInfo = Prelude.Nothing,
      networkInfoList = Prelude.Nothing,
      fileSystemType = Prelude.Nothing,
      cpuArchitecture = Prelude.Nothing
    }

-- | Operating system corresponding to a server.
systemInfo_osInfo :: Lens.Lens' SystemInfo (Prelude.Maybe OSInfo)
systemInfo_osInfo = Lens.lens (\SystemInfo' {osInfo} -> osInfo) (\s@SystemInfo' {} a -> s {osInfo = a} :: SystemInfo)

-- | Networking information related to a server.
systemInfo_networkInfoList :: Lens.Lens' SystemInfo (Prelude.Maybe [NetworkInfo])
systemInfo_networkInfoList = Lens.lens (\SystemInfo' {networkInfoList} -> networkInfoList) (\s@SystemInfo' {} a -> s {networkInfoList = a} :: SystemInfo) Prelude.. Lens.mapping Lens.coerced

-- | File system type for the server.
systemInfo_fileSystemType :: Lens.Lens' SystemInfo (Prelude.Maybe Prelude.Text)
systemInfo_fileSystemType = Lens.lens (\SystemInfo' {fileSystemType} -> fileSystemType) (\s@SystemInfo' {} a -> s {fileSystemType = a} :: SystemInfo)

-- | CPU architecture type for the server.
systemInfo_cpuArchitecture :: Lens.Lens' SystemInfo (Prelude.Maybe Prelude.Text)
systemInfo_cpuArchitecture = Lens.lens (\SystemInfo' {cpuArchitecture} -> cpuArchitecture) (\s@SystemInfo' {} a -> s {cpuArchitecture = a} :: SystemInfo)

instance Core.FromJSON SystemInfo where
  parseJSON =
    Core.withObject
      "SystemInfo"
      ( \x ->
          SystemInfo'
            Prelude.<$> (x Core..:? "osInfo")
            Prelude.<*> ( x Core..:? "networkInfoList"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "fileSystemType")
            Prelude.<*> (x Core..:? "cpuArchitecture")
      )

instance Prelude.Hashable SystemInfo where
  hashWithSalt _salt SystemInfo' {..} =
    _salt `Prelude.hashWithSalt` osInfo
      `Prelude.hashWithSalt` networkInfoList
      `Prelude.hashWithSalt` fileSystemType
      `Prelude.hashWithSalt` cpuArchitecture

instance Prelude.NFData SystemInfo where
  rnf SystemInfo' {..} =
    Prelude.rnf osInfo
      `Prelude.seq` Prelude.rnf networkInfoList
      `Prelude.seq` Prelude.rnf fileSystemType
      `Prelude.seq` Prelude.rnf cpuArchitecture
