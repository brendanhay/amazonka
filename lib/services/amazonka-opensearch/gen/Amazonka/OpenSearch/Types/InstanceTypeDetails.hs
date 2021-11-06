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
-- Copyright   : (c) 2013-2021 Brendan Hay
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
  { encryptionEnabled :: Prelude.Maybe Prelude.Bool,
    cognitoEnabled :: Prelude.Maybe Prelude.Bool,
    instanceRole :: Prelude.Maybe [Prelude.Text],
    instanceType :: Prelude.Maybe OpenSearchPartitionInstanceType,
    warmEnabled :: Prelude.Maybe Prelude.Bool,
    advancedSecurityEnabled :: Prelude.Maybe Prelude.Bool,
    appLogsEnabled :: Prelude.Maybe Prelude.Bool
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
-- 'encryptionEnabled', 'instanceTypeDetails_encryptionEnabled' - Undocumented member.
--
-- 'cognitoEnabled', 'instanceTypeDetails_cognitoEnabled' - Undocumented member.
--
-- 'instanceRole', 'instanceTypeDetails_instanceRole' - Undocumented member.
--
-- 'instanceType', 'instanceTypeDetails_instanceType' - Undocumented member.
--
-- 'warmEnabled', 'instanceTypeDetails_warmEnabled' - Undocumented member.
--
-- 'advancedSecurityEnabled', 'instanceTypeDetails_advancedSecurityEnabled' - Undocumented member.
--
-- 'appLogsEnabled', 'instanceTypeDetails_appLogsEnabled' - Undocumented member.
newInstanceTypeDetails ::
  InstanceTypeDetails
newInstanceTypeDetails =
  InstanceTypeDetails'
    { encryptionEnabled =
        Prelude.Nothing,
      cognitoEnabled = Prelude.Nothing,
      instanceRole = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      warmEnabled = Prelude.Nothing,
      advancedSecurityEnabled = Prelude.Nothing,
      appLogsEnabled = Prelude.Nothing
    }

-- | Undocumented member.
instanceTypeDetails_encryptionEnabled :: Lens.Lens' InstanceTypeDetails (Prelude.Maybe Prelude.Bool)
instanceTypeDetails_encryptionEnabled = Lens.lens (\InstanceTypeDetails' {encryptionEnabled} -> encryptionEnabled) (\s@InstanceTypeDetails' {} a -> s {encryptionEnabled = a} :: InstanceTypeDetails)

-- | Undocumented member.
instanceTypeDetails_cognitoEnabled :: Lens.Lens' InstanceTypeDetails (Prelude.Maybe Prelude.Bool)
instanceTypeDetails_cognitoEnabled = Lens.lens (\InstanceTypeDetails' {cognitoEnabled} -> cognitoEnabled) (\s@InstanceTypeDetails' {} a -> s {cognitoEnabled = a} :: InstanceTypeDetails)

-- | Undocumented member.
instanceTypeDetails_instanceRole :: Lens.Lens' InstanceTypeDetails (Prelude.Maybe [Prelude.Text])
instanceTypeDetails_instanceRole = Lens.lens (\InstanceTypeDetails' {instanceRole} -> instanceRole) (\s@InstanceTypeDetails' {} a -> s {instanceRole = a} :: InstanceTypeDetails) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
instanceTypeDetails_instanceType :: Lens.Lens' InstanceTypeDetails (Prelude.Maybe OpenSearchPartitionInstanceType)
instanceTypeDetails_instanceType = Lens.lens (\InstanceTypeDetails' {instanceType} -> instanceType) (\s@InstanceTypeDetails' {} a -> s {instanceType = a} :: InstanceTypeDetails)

-- | Undocumented member.
instanceTypeDetails_warmEnabled :: Lens.Lens' InstanceTypeDetails (Prelude.Maybe Prelude.Bool)
instanceTypeDetails_warmEnabled = Lens.lens (\InstanceTypeDetails' {warmEnabled} -> warmEnabled) (\s@InstanceTypeDetails' {} a -> s {warmEnabled = a} :: InstanceTypeDetails)

-- | Undocumented member.
instanceTypeDetails_advancedSecurityEnabled :: Lens.Lens' InstanceTypeDetails (Prelude.Maybe Prelude.Bool)
instanceTypeDetails_advancedSecurityEnabled = Lens.lens (\InstanceTypeDetails' {advancedSecurityEnabled} -> advancedSecurityEnabled) (\s@InstanceTypeDetails' {} a -> s {advancedSecurityEnabled = a} :: InstanceTypeDetails)

-- | Undocumented member.
instanceTypeDetails_appLogsEnabled :: Lens.Lens' InstanceTypeDetails (Prelude.Maybe Prelude.Bool)
instanceTypeDetails_appLogsEnabled = Lens.lens (\InstanceTypeDetails' {appLogsEnabled} -> appLogsEnabled) (\s@InstanceTypeDetails' {} a -> s {appLogsEnabled = a} :: InstanceTypeDetails)

instance Core.FromJSON InstanceTypeDetails where
  parseJSON =
    Core.withObject
      "InstanceTypeDetails"
      ( \x ->
          InstanceTypeDetails'
            Prelude.<$> (x Core..:? "EncryptionEnabled")
            Prelude.<*> (x Core..:? "CognitoEnabled")
            Prelude.<*> (x Core..:? "InstanceRole" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "InstanceType")
            Prelude.<*> (x Core..:? "WarmEnabled")
            Prelude.<*> (x Core..:? "AdvancedSecurityEnabled")
            Prelude.<*> (x Core..:? "AppLogsEnabled")
      )

instance Prelude.Hashable InstanceTypeDetails

instance Prelude.NFData InstanceTypeDetails
