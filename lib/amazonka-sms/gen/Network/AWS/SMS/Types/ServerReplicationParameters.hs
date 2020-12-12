{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ServerReplicationParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ServerReplicationParameters
  ( ServerReplicationParameters (..),

    -- * Smart constructor
    mkServerReplicationParameters,

    -- * Lenses
    srpFrequency,
    srpNumberOfRecentAMIsToKeep,
    srpSeedTime,
    srpLicenseType,
    srpEncrypted,
    srpKmsKeyId,
    srpRunOnce,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SMS.Types.LicenseType

-- | The replication parameters for replicating a server.
--
-- /See:/ 'mkServerReplicationParameters' smart constructor.
data ServerReplicationParameters = ServerReplicationParameters'
  { frequency ::
      Lude.Maybe Lude.Int,
    numberOfRecentAMIsToKeep ::
      Lude.Maybe Lude.Int,
    seedTime ::
      Lude.Maybe Lude.Timestamp,
    licenseType ::
      Lude.Maybe LicenseType,
    encrypted :: Lude.Maybe Lude.Bool,
    kmsKeyId :: Lude.Maybe Lude.Text,
    runOnce :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ServerReplicationParameters' with the minimum fields required to make a request.
--
-- * 'encrypted' - Indicates whether the replication job produces encrypted AMIs.
-- * 'frequency' - The frequency of creating replication jobs for the server.
-- * 'kmsKeyId' - The ID of the KMS key for replication jobs that produce encrypted AMIs. This value can be any of the following:
--
--
--     * KMS key ID
--
--
--     * KMS key alias
--
--
--     * ARN referring to the KMS key ID
--
--
--     * ARN referring to the KMS key alias
--
--
-- If encrypted is enabled but a KMS key ID is not specified, the customer's default KMS key for Amazon EBS is used.
-- * 'licenseType' - The license type for creating a replication job for the server.
-- * 'numberOfRecentAMIsToKeep' - The number of recent AMIs to keep when creating a replication job for this server.
-- * 'runOnce' - Indicates whether to run the replication job one time.
-- * 'seedTime' - The seed time for creating a replication job for the server.
mkServerReplicationParameters ::
  ServerReplicationParameters
mkServerReplicationParameters =
  ServerReplicationParameters'
    { frequency = Lude.Nothing,
      numberOfRecentAMIsToKeep = Lude.Nothing,
      seedTime = Lude.Nothing,
      licenseType = Lude.Nothing,
      encrypted = Lude.Nothing,
      kmsKeyId = Lude.Nothing,
      runOnce = Lude.Nothing
    }

-- | The frequency of creating replication jobs for the server.
--
-- /Note:/ Consider using 'frequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srpFrequency :: Lens.Lens' ServerReplicationParameters (Lude.Maybe Lude.Int)
srpFrequency = Lens.lens (frequency :: ServerReplicationParameters -> Lude.Maybe Lude.Int) (\s a -> s {frequency = a} :: ServerReplicationParameters)
{-# DEPRECATED srpFrequency "Use generic-lens or generic-optics with 'frequency' instead." #-}

-- | The number of recent AMIs to keep when creating a replication job for this server.
--
-- /Note:/ Consider using 'numberOfRecentAMIsToKeep' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srpNumberOfRecentAMIsToKeep :: Lens.Lens' ServerReplicationParameters (Lude.Maybe Lude.Int)
srpNumberOfRecentAMIsToKeep = Lens.lens (numberOfRecentAMIsToKeep :: ServerReplicationParameters -> Lude.Maybe Lude.Int) (\s a -> s {numberOfRecentAMIsToKeep = a} :: ServerReplicationParameters)
{-# DEPRECATED srpNumberOfRecentAMIsToKeep "Use generic-lens or generic-optics with 'numberOfRecentAMIsToKeep' instead." #-}

-- | The seed time for creating a replication job for the server.
--
-- /Note:/ Consider using 'seedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srpSeedTime :: Lens.Lens' ServerReplicationParameters (Lude.Maybe Lude.Timestamp)
srpSeedTime = Lens.lens (seedTime :: ServerReplicationParameters -> Lude.Maybe Lude.Timestamp) (\s a -> s {seedTime = a} :: ServerReplicationParameters)
{-# DEPRECATED srpSeedTime "Use generic-lens or generic-optics with 'seedTime' instead." #-}

-- | The license type for creating a replication job for the server.
--
-- /Note:/ Consider using 'licenseType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srpLicenseType :: Lens.Lens' ServerReplicationParameters (Lude.Maybe LicenseType)
srpLicenseType = Lens.lens (licenseType :: ServerReplicationParameters -> Lude.Maybe LicenseType) (\s a -> s {licenseType = a} :: ServerReplicationParameters)
{-# DEPRECATED srpLicenseType "Use generic-lens or generic-optics with 'licenseType' instead." #-}

-- | Indicates whether the replication job produces encrypted AMIs.
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srpEncrypted :: Lens.Lens' ServerReplicationParameters (Lude.Maybe Lude.Bool)
srpEncrypted = Lens.lens (encrypted :: ServerReplicationParameters -> Lude.Maybe Lude.Bool) (\s a -> s {encrypted = a} :: ServerReplicationParameters)
{-# DEPRECATED srpEncrypted "Use generic-lens or generic-optics with 'encrypted' instead." #-}

-- | The ID of the KMS key for replication jobs that produce encrypted AMIs. This value can be any of the following:
--
--
--     * KMS key ID
--
--
--     * KMS key alias
--
--
--     * ARN referring to the KMS key ID
--
--
--     * ARN referring to the KMS key alias
--
--
-- If encrypted is enabled but a KMS key ID is not specified, the customer's default KMS key for Amazon EBS is used.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srpKmsKeyId :: Lens.Lens' ServerReplicationParameters (Lude.Maybe Lude.Text)
srpKmsKeyId = Lens.lens (kmsKeyId :: ServerReplicationParameters -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: ServerReplicationParameters)
{-# DEPRECATED srpKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | Indicates whether to run the replication job one time.
--
-- /Note:/ Consider using 'runOnce' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srpRunOnce :: Lens.Lens' ServerReplicationParameters (Lude.Maybe Lude.Bool)
srpRunOnce = Lens.lens (runOnce :: ServerReplicationParameters -> Lude.Maybe Lude.Bool) (\s a -> s {runOnce = a} :: ServerReplicationParameters)
{-# DEPRECATED srpRunOnce "Use generic-lens or generic-optics with 'runOnce' instead." #-}

instance Lude.FromJSON ServerReplicationParameters where
  parseJSON =
    Lude.withObject
      "ServerReplicationParameters"
      ( \x ->
          ServerReplicationParameters'
            Lude.<$> (x Lude..:? "frequency")
            Lude.<*> (x Lude..:? "numberOfRecentAmisToKeep")
            Lude.<*> (x Lude..:? "seedTime")
            Lude.<*> (x Lude..:? "licenseType")
            Lude.<*> (x Lude..:? "encrypted")
            Lude.<*> (x Lude..:? "kmsKeyId")
            Lude.<*> (x Lude..:? "runOnce")
      )

instance Lude.ToJSON ServerReplicationParameters where
  toJSON ServerReplicationParameters' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("frequency" Lude..=) Lude.<$> frequency,
            ("numberOfRecentAmisToKeep" Lude..=)
              Lude.<$> numberOfRecentAMIsToKeep,
            ("seedTime" Lude..=) Lude.<$> seedTime,
            ("licenseType" Lude..=) Lude.<$> licenseType,
            ("encrypted" Lude..=) Lude.<$> encrypted,
            ("kmsKeyId" Lude..=) Lude.<$> kmsKeyId,
            ("runOnce" Lude..=) Lude.<$> runOnce
          ]
      )
