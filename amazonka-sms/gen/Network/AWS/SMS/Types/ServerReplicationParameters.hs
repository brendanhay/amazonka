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
-- Module      : Network.AWS.SMS.Types.ServerReplicationParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ServerReplicationParameters where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SMS.Types.LicenseType

-- | The replication parameters for replicating a server.
--
-- /See:/ 'newServerReplicationParameters' smart constructor.
data ServerReplicationParameters = ServerReplicationParameters'
  { -- | The number of recent AMIs to keep when creating a replication job for
    -- this server.
    numberOfRecentAmisToKeep :: Core.Maybe Core.Int,
    -- | Indicates whether the replication job produces encrypted AMIs.
    encrypted :: Core.Maybe Core.Bool,
    -- | The seed time for creating a replication job for the server.
    seedTime :: Core.Maybe Core.POSIX,
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
    kmsKeyId :: Core.Maybe Core.Text,
    -- | The frequency of creating replication jobs for the server.
    frequency :: Core.Maybe Core.Int,
    -- | Indicates whether to run the replication job one time.
    runOnce :: Core.Maybe Core.Bool,
    -- | The license type for creating a replication job for the server.
    licenseType :: Core.Maybe LicenseType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ServerReplicationParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'numberOfRecentAmisToKeep', 'serverReplicationParameters_numberOfRecentAmisToKeep' - The number of recent AMIs to keep when creating a replication job for
-- this server.
--
-- 'encrypted', 'serverReplicationParameters_encrypted' - Indicates whether the replication job produces encrypted AMIs.
--
-- 'seedTime', 'serverReplicationParameters_seedTime' - The seed time for creating a replication job for the server.
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
-- 'frequency', 'serverReplicationParameters_frequency' - The frequency of creating replication jobs for the server.
--
-- 'runOnce', 'serverReplicationParameters_runOnce' - Indicates whether to run the replication job one time.
--
-- 'licenseType', 'serverReplicationParameters_licenseType' - The license type for creating a replication job for the server.
newServerReplicationParameters ::
  ServerReplicationParameters
newServerReplicationParameters =
  ServerReplicationParameters'
    { numberOfRecentAmisToKeep =
        Core.Nothing,
      encrypted = Core.Nothing,
      seedTime = Core.Nothing,
      kmsKeyId = Core.Nothing,
      frequency = Core.Nothing,
      runOnce = Core.Nothing,
      licenseType = Core.Nothing
    }

-- | The number of recent AMIs to keep when creating a replication job for
-- this server.
serverReplicationParameters_numberOfRecentAmisToKeep :: Lens.Lens' ServerReplicationParameters (Core.Maybe Core.Int)
serverReplicationParameters_numberOfRecentAmisToKeep = Lens.lens (\ServerReplicationParameters' {numberOfRecentAmisToKeep} -> numberOfRecentAmisToKeep) (\s@ServerReplicationParameters' {} a -> s {numberOfRecentAmisToKeep = a} :: ServerReplicationParameters)

-- | Indicates whether the replication job produces encrypted AMIs.
serverReplicationParameters_encrypted :: Lens.Lens' ServerReplicationParameters (Core.Maybe Core.Bool)
serverReplicationParameters_encrypted = Lens.lens (\ServerReplicationParameters' {encrypted} -> encrypted) (\s@ServerReplicationParameters' {} a -> s {encrypted = a} :: ServerReplicationParameters)

-- | The seed time for creating a replication job for the server.
serverReplicationParameters_seedTime :: Lens.Lens' ServerReplicationParameters (Core.Maybe Core.UTCTime)
serverReplicationParameters_seedTime = Lens.lens (\ServerReplicationParameters' {seedTime} -> seedTime) (\s@ServerReplicationParameters' {} a -> s {seedTime = a} :: ServerReplicationParameters) Core.. Lens.mapping Core._Time

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
serverReplicationParameters_kmsKeyId :: Lens.Lens' ServerReplicationParameters (Core.Maybe Core.Text)
serverReplicationParameters_kmsKeyId = Lens.lens (\ServerReplicationParameters' {kmsKeyId} -> kmsKeyId) (\s@ServerReplicationParameters' {} a -> s {kmsKeyId = a} :: ServerReplicationParameters)

-- | The frequency of creating replication jobs for the server.
serverReplicationParameters_frequency :: Lens.Lens' ServerReplicationParameters (Core.Maybe Core.Int)
serverReplicationParameters_frequency = Lens.lens (\ServerReplicationParameters' {frequency} -> frequency) (\s@ServerReplicationParameters' {} a -> s {frequency = a} :: ServerReplicationParameters)

-- | Indicates whether to run the replication job one time.
serverReplicationParameters_runOnce :: Lens.Lens' ServerReplicationParameters (Core.Maybe Core.Bool)
serverReplicationParameters_runOnce = Lens.lens (\ServerReplicationParameters' {runOnce} -> runOnce) (\s@ServerReplicationParameters' {} a -> s {runOnce = a} :: ServerReplicationParameters)

-- | The license type for creating a replication job for the server.
serverReplicationParameters_licenseType :: Lens.Lens' ServerReplicationParameters (Core.Maybe LicenseType)
serverReplicationParameters_licenseType = Lens.lens (\ServerReplicationParameters' {licenseType} -> licenseType) (\s@ServerReplicationParameters' {} a -> s {licenseType = a} :: ServerReplicationParameters)

instance Core.FromJSON ServerReplicationParameters where
  parseJSON =
    Core.withObject
      "ServerReplicationParameters"
      ( \x ->
          ServerReplicationParameters'
            Core.<$> (x Core..:? "numberOfRecentAmisToKeep")
            Core.<*> (x Core..:? "encrypted")
            Core.<*> (x Core..:? "seedTime")
            Core.<*> (x Core..:? "kmsKeyId")
            Core.<*> (x Core..:? "frequency")
            Core.<*> (x Core..:? "runOnce")
            Core.<*> (x Core..:? "licenseType")
      )

instance Core.Hashable ServerReplicationParameters

instance Core.NFData ServerReplicationParameters

instance Core.ToJSON ServerReplicationParameters where
  toJSON ServerReplicationParameters' {..} =
    Core.object
      ( Core.catMaybes
          [ ("numberOfRecentAmisToKeep" Core..=)
              Core.<$> numberOfRecentAmisToKeep,
            ("encrypted" Core..=) Core.<$> encrypted,
            ("seedTime" Core..=) Core.<$> seedTime,
            ("kmsKeyId" Core..=) Core.<$> kmsKeyId,
            ("frequency" Core..=) Core.<$> frequency,
            ("runOnce" Core..=) Core.<$> runOnce,
            ("licenseType" Core..=) Core.<$> licenseType
          ]
      )
