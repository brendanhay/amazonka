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
-- Module      : Amazonka.OpenSearch.Types.InstanceTypeDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.InstanceTypeDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.OpenSearch.Types.OpenSearchPartitionInstanceType
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newInstanceTypeDetails' smart constructor.
data InstanceTypeDetails = InstanceTypeDetails'
  { advancedSecurityEnabled :: Prelude.Maybe Prelude.Bool,
    encryptionEnabled :: Prelude.Maybe Prelude.Bool,
    instanceType :: Prelude.Maybe OpenSearchPartitionInstanceType,
    cognitoEnabled :: Prelude.Maybe Prelude.Bool,
    instanceRole :: Prelude.Maybe [Prelude.Text],
    appLogsEnabled :: Prelude.Maybe Prelude.Bool,
    warmEnabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceTypeDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'advancedSecurityEnabled', 'instanceTypeDetails_advancedSecurityEnabled' - Undocumented member.
--
-- 'encryptionEnabled', 'instanceTypeDetails_encryptionEnabled' - Undocumented member.
--
-- 'instanceType', 'instanceTypeDetails_instanceType' - Undocumented member.
--
-- 'cognitoEnabled', 'instanceTypeDetails_cognitoEnabled' - Undocumented member.
--
-- 'instanceRole', 'instanceTypeDetails_instanceRole' - Undocumented member.
--
-- 'appLogsEnabled', 'instanceTypeDetails_appLogsEnabled' - Undocumented member.
--
-- 'warmEnabled', 'instanceTypeDetails_warmEnabled' - Undocumented member.
newInstanceTypeDetails ::
  InstanceTypeDetails
newInstanceTypeDetails =
  InstanceTypeDetails'
    { advancedSecurityEnabled =
        Prelude.Nothing,
      encryptionEnabled = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      cognitoEnabled = Prelude.Nothing,
      instanceRole = Prelude.Nothing,
      appLogsEnabled = Prelude.Nothing,
      warmEnabled = Prelude.Nothing
    }

-- | Undocumented member.
instanceTypeDetails_advancedSecurityEnabled :: Lens.Lens' InstanceTypeDetails (Prelude.Maybe Prelude.Bool)
instanceTypeDetails_advancedSecurityEnabled = Lens.lens (\InstanceTypeDetails' {advancedSecurityEnabled} -> advancedSecurityEnabled) (\s@InstanceTypeDetails' {} a -> s {advancedSecurityEnabled = a} :: InstanceTypeDetails)

-- | Undocumented member.
instanceTypeDetails_encryptionEnabled :: Lens.Lens' InstanceTypeDetails (Prelude.Maybe Prelude.Bool)
instanceTypeDetails_encryptionEnabled = Lens.lens (\InstanceTypeDetails' {encryptionEnabled} -> encryptionEnabled) (\s@InstanceTypeDetails' {} a -> s {encryptionEnabled = a} :: InstanceTypeDetails)

-- | Undocumented member.
instanceTypeDetails_instanceType :: Lens.Lens' InstanceTypeDetails (Prelude.Maybe OpenSearchPartitionInstanceType)
instanceTypeDetails_instanceType = Lens.lens (\InstanceTypeDetails' {instanceType} -> instanceType) (\s@InstanceTypeDetails' {} a -> s {instanceType = a} :: InstanceTypeDetails)

-- | Undocumented member.
instanceTypeDetails_cognitoEnabled :: Lens.Lens' InstanceTypeDetails (Prelude.Maybe Prelude.Bool)
instanceTypeDetails_cognitoEnabled = Lens.lens (\InstanceTypeDetails' {cognitoEnabled} -> cognitoEnabled) (\s@InstanceTypeDetails' {} a -> s {cognitoEnabled = a} :: InstanceTypeDetails)

-- | Undocumented member.
instanceTypeDetails_instanceRole :: Lens.Lens' InstanceTypeDetails (Prelude.Maybe [Prelude.Text])
instanceTypeDetails_instanceRole = Lens.lens (\InstanceTypeDetails' {instanceRole} -> instanceRole) (\s@InstanceTypeDetails' {} a -> s {instanceRole = a} :: InstanceTypeDetails) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
instanceTypeDetails_appLogsEnabled :: Lens.Lens' InstanceTypeDetails (Prelude.Maybe Prelude.Bool)
instanceTypeDetails_appLogsEnabled = Lens.lens (\InstanceTypeDetails' {appLogsEnabled} -> appLogsEnabled) (\s@InstanceTypeDetails' {} a -> s {appLogsEnabled = a} :: InstanceTypeDetails)

-- | Undocumented member.
instanceTypeDetails_warmEnabled :: Lens.Lens' InstanceTypeDetails (Prelude.Maybe Prelude.Bool)
instanceTypeDetails_warmEnabled = Lens.lens (\InstanceTypeDetails' {warmEnabled} -> warmEnabled) (\s@InstanceTypeDetails' {} a -> s {warmEnabled = a} :: InstanceTypeDetails)

instance Core.FromJSON InstanceTypeDetails where
  parseJSON =
    Core.withObject
      "InstanceTypeDetails"
      ( \x ->
          InstanceTypeDetails'
            Prelude.<$> (x Core..:? "AdvancedSecurityEnabled")
            Prelude.<*> (x Core..:? "EncryptionEnabled")
            Prelude.<*> (x Core..:? "InstanceType")
            Prelude.<*> (x Core..:? "CognitoEnabled")
            Prelude.<*> (x Core..:? "InstanceRole" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "AppLogsEnabled")
            Prelude.<*> (x Core..:? "WarmEnabled")
      )

instance Prelude.Hashable InstanceTypeDetails where
  hashWithSalt _salt InstanceTypeDetails' {..} =
    _salt
      `Prelude.hashWithSalt` advancedSecurityEnabled
      `Prelude.hashWithSalt` encryptionEnabled
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` cognitoEnabled
      `Prelude.hashWithSalt` instanceRole
      `Prelude.hashWithSalt` appLogsEnabled
      `Prelude.hashWithSalt` warmEnabled

instance Prelude.NFData InstanceTypeDetails where
  rnf InstanceTypeDetails' {..} =
    Prelude.rnf advancedSecurityEnabled
      `Prelude.seq` Prelude.rnf encryptionEnabled
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf cognitoEnabled
      `Prelude.seq` Prelude.rnf instanceRole
      `Prelude.seq` Prelude.rnf appLogsEnabled
      `Prelude.seq` Prelude.rnf warmEnabled
