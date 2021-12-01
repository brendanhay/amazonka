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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SMS.Types.ServerReplicationParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SMS.Types.LicenseType

-- | The replication parameters for replicating a server.
--
-- /See:/ 'newServerReplicationParameters' smart constructor.
data ServerReplicationParameters = ServerReplicationParameters'
  { -- | The frequency of creating replication jobs for the server.
    frequency :: Prelude.Maybe Prelude.Int,
    -- | The number of recent AMIs to keep when creating a replication job for
    -- this server.
    numberOfRecentAmisToKeep :: Prelude.Maybe Prelude.Int,
    -- | The seed time for creating a replication job for the server.
    seedTime :: Prelude.Maybe Core.POSIX,
    -- | The license type for creating a replication job for the server.
    licenseType :: Prelude.Maybe LicenseType,
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
    -- | Indicates whether to run the replication job one time.
    runOnce :: Prelude.Maybe Prelude.Bool
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
-- 'frequency', 'serverReplicationParameters_frequency' - The frequency of creating replication jobs for the server.
--
-- 'numberOfRecentAmisToKeep', 'serverReplicationParameters_numberOfRecentAmisToKeep' - The number of recent AMIs to keep when creating a replication job for
-- this server.
--
-- 'seedTime', 'serverReplicationParameters_seedTime' - The seed time for creating a replication job for the server.
--
-- 'licenseType', 'serverReplicationParameters_licenseType' - The license type for creating a replication job for the server.
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
-- 'runOnce', 'serverReplicationParameters_runOnce' - Indicates whether to run the replication job one time.
newServerReplicationParameters ::
  ServerReplicationParameters
newServerReplicationParameters =
  ServerReplicationParameters'
    { frequency =
        Prelude.Nothing,
      numberOfRecentAmisToKeep = Prelude.Nothing,
      seedTime = Prelude.Nothing,
      licenseType = Prelude.Nothing,
      encrypted = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      runOnce = Prelude.Nothing
    }

-- | The frequency of creating replication jobs for the server.
serverReplicationParameters_frequency :: Lens.Lens' ServerReplicationParameters (Prelude.Maybe Prelude.Int)
serverReplicationParameters_frequency = Lens.lens (\ServerReplicationParameters' {frequency} -> frequency) (\s@ServerReplicationParameters' {} a -> s {frequency = a} :: ServerReplicationParameters)

-- | The number of recent AMIs to keep when creating a replication job for
-- this server.
serverReplicationParameters_numberOfRecentAmisToKeep :: Lens.Lens' ServerReplicationParameters (Prelude.Maybe Prelude.Int)
serverReplicationParameters_numberOfRecentAmisToKeep = Lens.lens (\ServerReplicationParameters' {numberOfRecentAmisToKeep} -> numberOfRecentAmisToKeep) (\s@ServerReplicationParameters' {} a -> s {numberOfRecentAmisToKeep = a} :: ServerReplicationParameters)

-- | The seed time for creating a replication job for the server.
serverReplicationParameters_seedTime :: Lens.Lens' ServerReplicationParameters (Prelude.Maybe Prelude.UTCTime)
serverReplicationParameters_seedTime = Lens.lens (\ServerReplicationParameters' {seedTime} -> seedTime) (\s@ServerReplicationParameters' {} a -> s {seedTime = a} :: ServerReplicationParameters) Prelude.. Lens.mapping Core._Time

-- | The license type for creating a replication job for the server.
serverReplicationParameters_licenseType :: Lens.Lens' ServerReplicationParameters (Prelude.Maybe LicenseType)
serverReplicationParameters_licenseType = Lens.lens (\ServerReplicationParameters' {licenseType} -> licenseType) (\s@ServerReplicationParameters' {} a -> s {licenseType = a} :: ServerReplicationParameters)

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

-- | Indicates whether to run the replication job one time.
serverReplicationParameters_runOnce :: Lens.Lens' ServerReplicationParameters (Prelude.Maybe Prelude.Bool)
serverReplicationParameters_runOnce = Lens.lens (\ServerReplicationParameters' {runOnce} -> runOnce) (\s@ServerReplicationParameters' {} a -> s {runOnce = a} :: ServerReplicationParameters)

instance Core.FromJSON ServerReplicationParameters where
  parseJSON =
    Core.withObject
      "ServerReplicationParameters"
      ( \x ->
          ServerReplicationParameters'
            Prelude.<$> (x Core..:? "frequency")
            Prelude.<*> (x Core..:? "numberOfRecentAmisToKeep")
            Prelude.<*> (x Core..:? "seedTime")
            Prelude.<*> (x Core..:? "licenseType")
            Prelude.<*> (x Core..:? "encrypted")
            Prelude.<*> (x Core..:? "kmsKeyId")
            Prelude.<*> (x Core..:? "runOnce")
      )

instance Prelude.Hashable ServerReplicationParameters where
  hashWithSalt salt' ServerReplicationParameters' {..} =
    salt' `Prelude.hashWithSalt` runOnce
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` encrypted
      `Prelude.hashWithSalt` licenseType
      `Prelude.hashWithSalt` seedTime
      `Prelude.hashWithSalt` numberOfRecentAmisToKeep
      `Prelude.hashWithSalt` frequency

instance Prelude.NFData ServerReplicationParameters where
  rnf ServerReplicationParameters' {..} =
    Prelude.rnf frequency
      `Prelude.seq` Prelude.rnf runOnce
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf encrypted
      `Prelude.seq` Prelude.rnf licenseType
      `Prelude.seq` Prelude.rnf seedTime
      `Prelude.seq` Prelude.rnf numberOfRecentAmisToKeep

instance Core.ToJSON ServerReplicationParameters where
  toJSON ServerReplicationParameters' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("frequency" Core..=) Prelude.<$> frequency,
            ("numberOfRecentAmisToKeep" Core..=)
              Prelude.<$> numberOfRecentAmisToKeep,
            ("seedTime" Core..=) Prelude.<$> seedTime,
            ("licenseType" Core..=) Prelude.<$> licenseType,
            ("encrypted" Core..=) Prelude.<$> encrypted,
            ("kmsKeyId" Core..=) Prelude.<$> kmsKeyId,
            ("runOnce" Core..=) Prelude.<$> runOnce
          ]
      )
