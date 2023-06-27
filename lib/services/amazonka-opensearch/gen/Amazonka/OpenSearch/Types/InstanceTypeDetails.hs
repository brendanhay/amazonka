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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.InstanceTypeDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types.OpenSearchPartitionInstanceType
import qualified Amazonka.Prelude as Prelude

-- | Lists all instance types and available features for a given OpenSearch
-- or Elasticsearch version.
--
-- /See:/ 'newInstanceTypeDetails' smart constructor.
data InstanceTypeDetails = InstanceTypeDetails'
  { -- | Whether fine-grained access control is supported for the instance type.
    advancedSecurityEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Whether logging is supported for the instance type.
    appLogsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The supported Availability Zones for the instance type.
    availabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | Whether Amazon Cognito access is supported for the instance type.
    cognitoEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Whether encryption at rest and node-to-node encryption are supported for
    -- the instance type.
    encryptionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Whether the instance acts as a data node, a dedicated master node, or an
    -- UltraWarm node.
    instanceRole :: Prelude.Maybe [Prelude.Text],
    -- | The instance type.
    instanceType :: Prelude.Maybe OpenSearchPartitionInstanceType,
    -- | Whether UltraWarm is supported for the instance type.
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
-- 'advancedSecurityEnabled', 'instanceTypeDetails_advancedSecurityEnabled' - Whether fine-grained access control is supported for the instance type.
--
-- 'appLogsEnabled', 'instanceTypeDetails_appLogsEnabled' - Whether logging is supported for the instance type.
--
-- 'availabilityZones', 'instanceTypeDetails_availabilityZones' - The supported Availability Zones for the instance type.
--
-- 'cognitoEnabled', 'instanceTypeDetails_cognitoEnabled' - Whether Amazon Cognito access is supported for the instance type.
--
-- 'encryptionEnabled', 'instanceTypeDetails_encryptionEnabled' - Whether encryption at rest and node-to-node encryption are supported for
-- the instance type.
--
-- 'instanceRole', 'instanceTypeDetails_instanceRole' - Whether the instance acts as a data node, a dedicated master node, or an
-- UltraWarm node.
--
-- 'instanceType', 'instanceTypeDetails_instanceType' - The instance type.
--
-- 'warmEnabled', 'instanceTypeDetails_warmEnabled' - Whether UltraWarm is supported for the instance type.
newInstanceTypeDetails ::
  InstanceTypeDetails
newInstanceTypeDetails =
  InstanceTypeDetails'
    { advancedSecurityEnabled =
        Prelude.Nothing,
      appLogsEnabled = Prelude.Nothing,
      availabilityZones = Prelude.Nothing,
      cognitoEnabled = Prelude.Nothing,
      encryptionEnabled = Prelude.Nothing,
      instanceRole = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      warmEnabled = Prelude.Nothing
    }

-- | Whether fine-grained access control is supported for the instance type.
instanceTypeDetails_advancedSecurityEnabled :: Lens.Lens' InstanceTypeDetails (Prelude.Maybe Prelude.Bool)
instanceTypeDetails_advancedSecurityEnabled = Lens.lens (\InstanceTypeDetails' {advancedSecurityEnabled} -> advancedSecurityEnabled) (\s@InstanceTypeDetails' {} a -> s {advancedSecurityEnabled = a} :: InstanceTypeDetails)

-- | Whether logging is supported for the instance type.
instanceTypeDetails_appLogsEnabled :: Lens.Lens' InstanceTypeDetails (Prelude.Maybe Prelude.Bool)
instanceTypeDetails_appLogsEnabled = Lens.lens (\InstanceTypeDetails' {appLogsEnabled} -> appLogsEnabled) (\s@InstanceTypeDetails' {} a -> s {appLogsEnabled = a} :: InstanceTypeDetails)

-- | The supported Availability Zones for the instance type.
instanceTypeDetails_availabilityZones :: Lens.Lens' InstanceTypeDetails (Prelude.Maybe [Prelude.Text])
instanceTypeDetails_availabilityZones = Lens.lens (\InstanceTypeDetails' {availabilityZones} -> availabilityZones) (\s@InstanceTypeDetails' {} a -> s {availabilityZones = a} :: InstanceTypeDetails) Prelude.. Lens.mapping Lens.coerced

-- | Whether Amazon Cognito access is supported for the instance type.
instanceTypeDetails_cognitoEnabled :: Lens.Lens' InstanceTypeDetails (Prelude.Maybe Prelude.Bool)
instanceTypeDetails_cognitoEnabled = Lens.lens (\InstanceTypeDetails' {cognitoEnabled} -> cognitoEnabled) (\s@InstanceTypeDetails' {} a -> s {cognitoEnabled = a} :: InstanceTypeDetails)

-- | Whether encryption at rest and node-to-node encryption are supported for
-- the instance type.
instanceTypeDetails_encryptionEnabled :: Lens.Lens' InstanceTypeDetails (Prelude.Maybe Prelude.Bool)
instanceTypeDetails_encryptionEnabled = Lens.lens (\InstanceTypeDetails' {encryptionEnabled} -> encryptionEnabled) (\s@InstanceTypeDetails' {} a -> s {encryptionEnabled = a} :: InstanceTypeDetails)

-- | Whether the instance acts as a data node, a dedicated master node, or an
-- UltraWarm node.
instanceTypeDetails_instanceRole :: Lens.Lens' InstanceTypeDetails (Prelude.Maybe [Prelude.Text])
instanceTypeDetails_instanceRole = Lens.lens (\InstanceTypeDetails' {instanceRole} -> instanceRole) (\s@InstanceTypeDetails' {} a -> s {instanceRole = a} :: InstanceTypeDetails) Prelude.. Lens.mapping Lens.coerced

-- | The instance type.
instanceTypeDetails_instanceType :: Lens.Lens' InstanceTypeDetails (Prelude.Maybe OpenSearchPartitionInstanceType)
instanceTypeDetails_instanceType = Lens.lens (\InstanceTypeDetails' {instanceType} -> instanceType) (\s@InstanceTypeDetails' {} a -> s {instanceType = a} :: InstanceTypeDetails)

-- | Whether UltraWarm is supported for the instance type.
instanceTypeDetails_warmEnabled :: Lens.Lens' InstanceTypeDetails (Prelude.Maybe Prelude.Bool)
instanceTypeDetails_warmEnabled = Lens.lens (\InstanceTypeDetails' {warmEnabled} -> warmEnabled) (\s@InstanceTypeDetails' {} a -> s {warmEnabled = a} :: InstanceTypeDetails)

instance Data.FromJSON InstanceTypeDetails where
  parseJSON =
    Data.withObject
      "InstanceTypeDetails"
      ( \x ->
          InstanceTypeDetails'
            Prelude.<$> (x Data..:? "AdvancedSecurityEnabled")
            Prelude.<*> (x Data..:? "AppLogsEnabled")
            Prelude.<*> ( x
                            Data..:? "AvailabilityZones"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "CognitoEnabled")
            Prelude.<*> (x Data..:? "EncryptionEnabled")
            Prelude.<*> (x Data..:? "InstanceRole" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "InstanceType")
            Prelude.<*> (x Data..:? "WarmEnabled")
      )

instance Prelude.Hashable InstanceTypeDetails where
  hashWithSalt _salt InstanceTypeDetails' {..} =
    _salt
      `Prelude.hashWithSalt` advancedSecurityEnabled
      `Prelude.hashWithSalt` appLogsEnabled
      `Prelude.hashWithSalt` availabilityZones
      `Prelude.hashWithSalt` cognitoEnabled
      `Prelude.hashWithSalt` encryptionEnabled
      `Prelude.hashWithSalt` instanceRole
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` warmEnabled

instance Prelude.NFData InstanceTypeDetails where
  rnf InstanceTypeDetails' {..} =
    Prelude.rnf advancedSecurityEnabled
      `Prelude.seq` Prelude.rnf appLogsEnabled
      `Prelude.seq` Prelude.rnf availabilityZones
      `Prelude.seq` Prelude.rnf cognitoEnabled
      `Prelude.seq` Prelude.rnf encryptionEnabled
      `Prelude.seq` Prelude.rnf instanceRole
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf warmEnabled
