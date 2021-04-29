{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SMS.Types.LicenseType

-- | The replication parameters for replicating a server.
--
-- /See:/ 'newServerReplicationParameters' smart constructor.
data ServerReplicationParameters = ServerReplicationParameters'
  { -- | The number of recent AMIs to keep when creating a replication job for
    -- this server.
    numberOfRecentAmisToKeep :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether the replication job produces encrypted AMIs.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | The seed time for creating a replication job for the server.
    seedTime :: Prelude.Maybe Prelude.POSIX,
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
    -- | The frequency of creating replication jobs for the server.
    frequency :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether to run the replication job one time.
    runOnce :: Prelude.Maybe Prelude.Bool,
    -- | The license type for creating a replication job for the server.
    licenseType :: Prelude.Maybe LicenseType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      encrypted = Prelude.Nothing,
      seedTime = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      frequency = Prelude.Nothing,
      runOnce = Prelude.Nothing,
      licenseType = Prelude.Nothing
    }

-- | The number of recent AMIs to keep when creating a replication job for
-- this server.
serverReplicationParameters_numberOfRecentAmisToKeep :: Lens.Lens' ServerReplicationParameters (Prelude.Maybe Prelude.Int)
serverReplicationParameters_numberOfRecentAmisToKeep = Lens.lens (\ServerReplicationParameters' {numberOfRecentAmisToKeep} -> numberOfRecentAmisToKeep) (\s@ServerReplicationParameters' {} a -> s {numberOfRecentAmisToKeep = a} :: ServerReplicationParameters)

-- | Indicates whether the replication job produces encrypted AMIs.
serverReplicationParameters_encrypted :: Lens.Lens' ServerReplicationParameters (Prelude.Maybe Prelude.Bool)
serverReplicationParameters_encrypted = Lens.lens (\ServerReplicationParameters' {encrypted} -> encrypted) (\s@ServerReplicationParameters' {} a -> s {encrypted = a} :: ServerReplicationParameters)

-- | The seed time for creating a replication job for the server.
serverReplicationParameters_seedTime :: Lens.Lens' ServerReplicationParameters (Prelude.Maybe Prelude.UTCTime)
serverReplicationParameters_seedTime = Lens.lens (\ServerReplicationParameters' {seedTime} -> seedTime) (\s@ServerReplicationParameters' {} a -> s {seedTime = a} :: ServerReplicationParameters) Prelude.. Lens.mapping Prelude._Time

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

-- | The frequency of creating replication jobs for the server.
serverReplicationParameters_frequency :: Lens.Lens' ServerReplicationParameters (Prelude.Maybe Prelude.Int)
serverReplicationParameters_frequency = Lens.lens (\ServerReplicationParameters' {frequency} -> frequency) (\s@ServerReplicationParameters' {} a -> s {frequency = a} :: ServerReplicationParameters)

-- | Indicates whether to run the replication job one time.
serverReplicationParameters_runOnce :: Lens.Lens' ServerReplicationParameters (Prelude.Maybe Prelude.Bool)
serverReplicationParameters_runOnce = Lens.lens (\ServerReplicationParameters' {runOnce} -> runOnce) (\s@ServerReplicationParameters' {} a -> s {runOnce = a} :: ServerReplicationParameters)

-- | The license type for creating a replication job for the server.
serverReplicationParameters_licenseType :: Lens.Lens' ServerReplicationParameters (Prelude.Maybe LicenseType)
serverReplicationParameters_licenseType = Lens.lens (\ServerReplicationParameters' {licenseType} -> licenseType) (\s@ServerReplicationParameters' {} a -> s {licenseType = a} :: ServerReplicationParameters)

instance Prelude.FromJSON ServerReplicationParameters where
  parseJSON =
    Prelude.withObject
      "ServerReplicationParameters"
      ( \x ->
          ServerReplicationParameters'
            Prelude.<$> (x Prelude..:? "numberOfRecentAmisToKeep")
            Prelude.<*> (x Prelude..:? "encrypted")
            Prelude.<*> (x Prelude..:? "seedTime")
            Prelude.<*> (x Prelude..:? "kmsKeyId")
            Prelude.<*> (x Prelude..:? "frequency")
            Prelude.<*> (x Prelude..:? "runOnce")
            Prelude.<*> (x Prelude..:? "licenseType")
      )

instance Prelude.Hashable ServerReplicationParameters

instance Prelude.NFData ServerReplicationParameters

instance Prelude.ToJSON ServerReplicationParameters where
  toJSON ServerReplicationParameters' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("numberOfRecentAmisToKeep" Prelude..=)
              Prelude.<$> numberOfRecentAmisToKeep,
            ("encrypted" Prelude..=) Prelude.<$> encrypted,
            ("seedTime" Prelude..=) Prelude.<$> seedTime,
            ("kmsKeyId" Prelude..=) Prelude.<$> kmsKeyId,
            ("frequency" Prelude..=) Prelude.<$> frequency,
            ("runOnce" Prelude..=) Prelude.<$> runOnce,
            ("licenseType" Prelude..=) Prelude.<$> licenseType
          ]
      )
