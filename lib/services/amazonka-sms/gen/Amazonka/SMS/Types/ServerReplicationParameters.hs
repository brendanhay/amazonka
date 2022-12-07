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
-- Module      : Amazonka.SMS.Types.ServerReplicationParameters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SMS.Types.ServerReplicationParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SMS.Types.LicenseType

-- | The replication parameters for replicating a server.
--
-- /See:/ 'newServerReplicationParameters' smart constructor.
data ServerReplicationParameters = ServerReplicationParameters'
  { -- | The seed time for creating a replication job for the server.
    seedTime :: Prelude.Maybe Data.POSIX,
    -- | The license type for creating a replication job for the server.
    licenseType :: Prelude.Maybe LicenseType,
    -- | The frequency of creating replication jobs for the server.
    frequency :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether to run the replication job one time.
    runOnce :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether the replication job produces encrypted AMIs.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the KMS key for replication jobs that produce encrypted AMIs.
    -- This value can be any of the following:
    --
    -- -   KMS key ID
    --
    -- -   KMS key alias
    --
    -- -   ARN referring to the KMS key ID
    --
    -- -   ARN referring to the KMS key alias
    --
    -- If encrypted is enabled but a KMS key ID is not specified, the
    -- customer\'s default KMS key for Amazon EBS is used.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The number of recent AMIs to keep when creating a replication job for
    -- this server.
    numberOfRecentAmisToKeep :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServerReplicationParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'seedTime', 'serverReplicationParameters_seedTime' - The seed time for creating a replication job for the server.
--
-- 'licenseType', 'serverReplicationParameters_licenseType' - The license type for creating a replication job for the server.
--
-- 'frequency', 'serverReplicationParameters_frequency' - The frequency of creating replication jobs for the server.
--
-- 'runOnce', 'serverReplicationParameters_runOnce' - Indicates whether to run the replication job one time.
--
-- 'encrypted', 'serverReplicationParameters_encrypted' - Indicates whether the replication job produces encrypted AMIs.
--
-- 'kmsKeyId', 'serverReplicationParameters_kmsKeyId' - The ID of the KMS key for replication jobs that produce encrypted AMIs.
-- This value can be any of the following:
--
-- -   KMS key ID
--
-- -   KMS key alias
--
-- -   ARN referring to the KMS key ID
--
-- -   ARN referring to the KMS key alias
--
-- If encrypted is enabled but a KMS key ID is not specified, the
-- customer\'s default KMS key for Amazon EBS is used.
--
-- 'numberOfRecentAmisToKeep', 'serverReplicationParameters_numberOfRecentAmisToKeep' - The number of recent AMIs to keep when creating a replication job for
-- this server.
newServerReplicationParameters ::
  ServerReplicationParameters
newServerReplicationParameters =
  ServerReplicationParameters'
    { seedTime =
        Prelude.Nothing,
      licenseType = Prelude.Nothing,
      frequency = Prelude.Nothing,
      runOnce = Prelude.Nothing,
      encrypted = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      numberOfRecentAmisToKeep = Prelude.Nothing
    }

-- | The seed time for creating a replication job for the server.
serverReplicationParameters_seedTime :: Lens.Lens' ServerReplicationParameters (Prelude.Maybe Prelude.UTCTime)
serverReplicationParameters_seedTime = Lens.lens (\ServerReplicationParameters' {seedTime} -> seedTime) (\s@ServerReplicationParameters' {} a -> s {seedTime = a} :: ServerReplicationParameters) Prelude.. Lens.mapping Data._Time

-- | The license type for creating a replication job for the server.
serverReplicationParameters_licenseType :: Lens.Lens' ServerReplicationParameters (Prelude.Maybe LicenseType)
serverReplicationParameters_licenseType = Lens.lens (\ServerReplicationParameters' {licenseType} -> licenseType) (\s@ServerReplicationParameters' {} a -> s {licenseType = a} :: ServerReplicationParameters)

-- | The frequency of creating replication jobs for the server.
serverReplicationParameters_frequency :: Lens.Lens' ServerReplicationParameters (Prelude.Maybe Prelude.Int)
serverReplicationParameters_frequency = Lens.lens (\ServerReplicationParameters' {frequency} -> frequency) (\s@ServerReplicationParameters' {} a -> s {frequency = a} :: ServerReplicationParameters)

-- | Indicates whether to run the replication job one time.
serverReplicationParameters_runOnce :: Lens.Lens' ServerReplicationParameters (Prelude.Maybe Prelude.Bool)
serverReplicationParameters_runOnce = Lens.lens (\ServerReplicationParameters' {runOnce} -> runOnce) (\s@ServerReplicationParameters' {} a -> s {runOnce = a} :: ServerReplicationParameters)

-- | Indicates whether the replication job produces encrypted AMIs.
serverReplicationParameters_encrypted :: Lens.Lens' ServerReplicationParameters (Prelude.Maybe Prelude.Bool)
serverReplicationParameters_encrypted = Lens.lens (\ServerReplicationParameters' {encrypted} -> encrypted) (\s@ServerReplicationParameters' {} a -> s {encrypted = a} :: ServerReplicationParameters)

-- | The ID of the KMS key for replication jobs that produce encrypted AMIs.
-- This value can be any of the following:
--
-- -   KMS key ID
--
-- -   KMS key alias
--
-- -   ARN referring to the KMS key ID
--
-- -   ARN referring to the KMS key alias
--
-- If encrypted is enabled but a KMS key ID is not specified, the
-- customer\'s default KMS key for Amazon EBS is used.
serverReplicationParameters_kmsKeyId :: Lens.Lens' ServerReplicationParameters (Prelude.Maybe Prelude.Text)
serverReplicationParameters_kmsKeyId = Lens.lens (\ServerReplicationParameters' {kmsKeyId} -> kmsKeyId) (\s@ServerReplicationParameters' {} a -> s {kmsKeyId = a} :: ServerReplicationParameters)

-- | The number of recent AMIs to keep when creating a replication job for
-- this server.
serverReplicationParameters_numberOfRecentAmisToKeep :: Lens.Lens' ServerReplicationParameters (Prelude.Maybe Prelude.Int)
serverReplicationParameters_numberOfRecentAmisToKeep = Lens.lens (\ServerReplicationParameters' {numberOfRecentAmisToKeep} -> numberOfRecentAmisToKeep) (\s@ServerReplicationParameters' {} a -> s {numberOfRecentAmisToKeep = a} :: ServerReplicationParameters)

instance Data.FromJSON ServerReplicationParameters where
  parseJSON =
    Data.withObject
      "ServerReplicationParameters"
      ( \x ->
          ServerReplicationParameters'
            Prelude.<$> (x Data..:? "seedTime")
            Prelude.<*> (x Data..:? "licenseType")
            Prelude.<*> (x Data..:? "frequency")
            Prelude.<*> (x Data..:? "runOnce")
            Prelude.<*> (x Data..:? "encrypted")
            Prelude.<*> (x Data..:? "kmsKeyId")
            Prelude.<*> (x Data..:? "numberOfRecentAmisToKeep")
      )

instance Prelude.Hashable ServerReplicationParameters where
  hashWithSalt _salt ServerReplicationParameters' {..} =
    _salt `Prelude.hashWithSalt` seedTime
      `Prelude.hashWithSalt` licenseType
      `Prelude.hashWithSalt` frequency
      `Prelude.hashWithSalt` runOnce
      `Prelude.hashWithSalt` encrypted
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` numberOfRecentAmisToKeep

instance Prelude.NFData ServerReplicationParameters where
  rnf ServerReplicationParameters' {..} =
    Prelude.rnf seedTime
      `Prelude.seq` Prelude.rnf licenseType
      `Prelude.seq` Prelude.rnf frequency
      `Prelude.seq` Prelude.rnf runOnce
      `Prelude.seq` Prelude.rnf encrypted
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf numberOfRecentAmisToKeep

instance Data.ToJSON ServerReplicationParameters where
  toJSON ServerReplicationParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("seedTime" Data..=) Prelude.<$> seedTime,
            ("licenseType" Data..=) Prelude.<$> licenseType,
            ("frequency" Data..=) Prelude.<$> frequency,
            ("runOnce" Data..=) Prelude.<$> runOnce,
            ("encrypted" Data..=) Prelude.<$> encrypted,
            ("kmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            ("numberOfRecentAmisToKeep" Data..=)
              Prelude.<$> numberOfRecentAmisToKeep
          ]
      )
