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
-- Module      : Amazonka.MigrationHubStrategy.Types.IPAddressBasedRemoteInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.IPAddressBasedRemoteInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types.AuthType
import Amazonka.MigrationHubStrategy.Types.OSType
import qualified Amazonka.Prelude as Prelude

-- | IP address based configurations.
--
-- /See:/ 'newIPAddressBasedRemoteInfo' smart constructor.
data IPAddressBasedRemoteInfo = IPAddressBasedRemoteInfo'
  { -- | The type of authorization.
    authType :: Prelude.Maybe AuthType,
    -- | The time stamp of the configuration.
    ipAddressConfigurationTimeStamp :: Prelude.Maybe Prelude.Text,
    -- | The type of the operating system.
    osType :: Prelude.Maybe OSType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IPAddressBasedRemoteInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authType', 'iPAddressBasedRemoteInfo_authType' - The type of authorization.
--
-- 'ipAddressConfigurationTimeStamp', 'iPAddressBasedRemoteInfo_ipAddressConfigurationTimeStamp' - The time stamp of the configuration.
--
-- 'osType', 'iPAddressBasedRemoteInfo_osType' - The type of the operating system.
newIPAddressBasedRemoteInfo ::
  IPAddressBasedRemoteInfo
newIPAddressBasedRemoteInfo =
  IPAddressBasedRemoteInfo'
    { authType =
        Prelude.Nothing,
      ipAddressConfigurationTimeStamp = Prelude.Nothing,
      osType = Prelude.Nothing
    }

-- | The type of authorization.
iPAddressBasedRemoteInfo_authType :: Lens.Lens' IPAddressBasedRemoteInfo (Prelude.Maybe AuthType)
iPAddressBasedRemoteInfo_authType = Lens.lens (\IPAddressBasedRemoteInfo' {authType} -> authType) (\s@IPAddressBasedRemoteInfo' {} a -> s {authType = a} :: IPAddressBasedRemoteInfo)

-- | The time stamp of the configuration.
iPAddressBasedRemoteInfo_ipAddressConfigurationTimeStamp :: Lens.Lens' IPAddressBasedRemoteInfo (Prelude.Maybe Prelude.Text)
iPAddressBasedRemoteInfo_ipAddressConfigurationTimeStamp = Lens.lens (\IPAddressBasedRemoteInfo' {ipAddressConfigurationTimeStamp} -> ipAddressConfigurationTimeStamp) (\s@IPAddressBasedRemoteInfo' {} a -> s {ipAddressConfigurationTimeStamp = a} :: IPAddressBasedRemoteInfo)

-- | The type of the operating system.
iPAddressBasedRemoteInfo_osType :: Lens.Lens' IPAddressBasedRemoteInfo (Prelude.Maybe OSType)
iPAddressBasedRemoteInfo_osType = Lens.lens (\IPAddressBasedRemoteInfo' {osType} -> osType) (\s@IPAddressBasedRemoteInfo' {} a -> s {osType = a} :: IPAddressBasedRemoteInfo)

instance Data.FromJSON IPAddressBasedRemoteInfo where
  parseJSON =
    Data.withObject
      "IPAddressBasedRemoteInfo"
      ( \x ->
          IPAddressBasedRemoteInfo'
            Prelude.<$> (x Data..:? "authType")
            Prelude.<*> (x Data..:? "ipAddressConfigurationTimeStamp")
            Prelude.<*> (x Data..:? "osType")
      )

instance Prelude.Hashable IPAddressBasedRemoteInfo where
  hashWithSalt _salt IPAddressBasedRemoteInfo' {..} =
    _salt `Prelude.hashWithSalt` authType
      `Prelude.hashWithSalt` ipAddressConfigurationTimeStamp
      `Prelude.hashWithSalt` osType

instance Prelude.NFData IPAddressBasedRemoteInfo where
  rnf IPAddressBasedRemoteInfo' {..} =
    Prelude.rnf authType
      `Prelude.seq` Prelude.rnf ipAddressConfigurationTimeStamp
      `Prelude.seq` Prelude.rnf osType
