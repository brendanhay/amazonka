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
-- Module      : Amazonka.MigrationHubStrategy.Types.VcenterBasedRemoteInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.VcenterBasedRemoteInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types.OSType
import qualified Amazonka.Prelude as Prelude

-- | Details about the server in vCenter.
--
-- /See:/ 'newVcenterBasedRemoteInfo' smart constructor.
data VcenterBasedRemoteInfo = VcenterBasedRemoteInfo'
  { -- | The type of the operating system.
    osType :: Prelude.Maybe OSType,
    -- | The time when the remote server based on vCenter was last configured.
    vcenterConfigurationTimeStamp :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VcenterBasedRemoteInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'osType', 'vcenterBasedRemoteInfo_osType' - The type of the operating system.
--
-- 'vcenterConfigurationTimeStamp', 'vcenterBasedRemoteInfo_vcenterConfigurationTimeStamp' - The time when the remote server based on vCenter was last configured.
newVcenterBasedRemoteInfo ::
  VcenterBasedRemoteInfo
newVcenterBasedRemoteInfo =
  VcenterBasedRemoteInfo'
    { osType = Prelude.Nothing,
      vcenterConfigurationTimeStamp = Prelude.Nothing
    }

-- | The type of the operating system.
vcenterBasedRemoteInfo_osType :: Lens.Lens' VcenterBasedRemoteInfo (Prelude.Maybe OSType)
vcenterBasedRemoteInfo_osType = Lens.lens (\VcenterBasedRemoteInfo' {osType} -> osType) (\s@VcenterBasedRemoteInfo' {} a -> s {osType = a} :: VcenterBasedRemoteInfo)

-- | The time when the remote server based on vCenter was last configured.
vcenterBasedRemoteInfo_vcenterConfigurationTimeStamp :: Lens.Lens' VcenterBasedRemoteInfo (Prelude.Maybe Prelude.Text)
vcenterBasedRemoteInfo_vcenterConfigurationTimeStamp = Lens.lens (\VcenterBasedRemoteInfo' {vcenterConfigurationTimeStamp} -> vcenterConfigurationTimeStamp) (\s@VcenterBasedRemoteInfo' {} a -> s {vcenterConfigurationTimeStamp = a} :: VcenterBasedRemoteInfo)

instance Data.FromJSON VcenterBasedRemoteInfo where
  parseJSON =
    Data.withObject
      "VcenterBasedRemoteInfo"
      ( \x ->
          VcenterBasedRemoteInfo'
            Prelude.<$> (x Data..:? "osType")
            Prelude.<*> (x Data..:? "vcenterConfigurationTimeStamp")
      )

instance Prelude.Hashable VcenterBasedRemoteInfo where
  hashWithSalt _salt VcenterBasedRemoteInfo' {..} =
    _salt
      `Prelude.hashWithSalt` osType
      `Prelude.hashWithSalt` vcenterConfigurationTimeStamp

instance Prelude.NFData VcenterBasedRemoteInfo where
  rnf VcenterBasedRemoteInfo' {..} =
    Prelude.rnf osType
      `Prelude.seq` Prelude.rnf vcenterConfigurationTimeStamp
