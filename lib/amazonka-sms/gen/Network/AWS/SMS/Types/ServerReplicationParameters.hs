{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ServerReplicationParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SMS.Types.ServerReplicationParameters
  ( ServerReplicationParameters (..)
  -- * Smart constructor
  , mkServerReplicationParameters
  -- * Lenses
  , srpEncrypted
  , srpFrequency
  , srpKmsKeyId
  , srpLicenseType
  , srpNumberOfRecentAmisToKeep
  , srpRunOnce
  , srpSeedTime
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SMS.Types.KmsKeyId as Types
import qualified Network.AWS.SMS.Types.LicenseType as Types

-- | The replication parameters for replicating a server.
--
-- /See:/ 'mkServerReplicationParameters' smart constructor.
data ServerReplicationParameters = ServerReplicationParameters'
  { encrypted :: Core.Maybe Core.Bool
    -- ^ Indicates whether the replication job produces encrypted AMIs.
  , frequency :: Core.Maybe Core.Int
    -- ^ The frequency of creating replication jobs for the server.
  , kmsKeyId :: Core.Maybe Types.KmsKeyId
    -- ^ The ID of the KMS key for replication jobs that produce encrypted AMIs. This value can be any of the following:
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
  , licenseType :: Core.Maybe Types.LicenseType
    -- ^ The license type for creating a replication job for the server.
  , numberOfRecentAmisToKeep :: Core.Maybe Core.Int
    -- ^ The number of recent AMIs to keep when creating a replication job for this server.
  , runOnce :: Core.Maybe Core.Bool
    -- ^ Indicates whether to run the replication job one time.
  , seedTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The seed time for creating a replication job for the server.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ServerReplicationParameters' value with any optional fields omitted.
mkServerReplicationParameters
    :: ServerReplicationParameters
mkServerReplicationParameters
  = ServerReplicationParameters'{encrypted = Core.Nothing,
                                 frequency = Core.Nothing, kmsKeyId = Core.Nothing,
                                 licenseType = Core.Nothing,
                                 numberOfRecentAmisToKeep = Core.Nothing, runOnce = Core.Nothing,
                                 seedTime = Core.Nothing}

-- | Indicates whether the replication job produces encrypted AMIs.
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srpEncrypted :: Lens.Lens' ServerReplicationParameters (Core.Maybe Core.Bool)
srpEncrypted = Lens.field @"encrypted"
{-# INLINEABLE srpEncrypted #-}
{-# DEPRECATED encrypted "Use generic-lens or generic-optics with 'encrypted' instead"  #-}

-- | The frequency of creating replication jobs for the server.
--
-- /Note:/ Consider using 'frequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srpFrequency :: Lens.Lens' ServerReplicationParameters (Core.Maybe Core.Int)
srpFrequency = Lens.field @"frequency"
{-# INLINEABLE srpFrequency #-}
{-# DEPRECATED frequency "Use generic-lens or generic-optics with 'frequency' instead"  #-}

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
srpKmsKeyId :: Lens.Lens' ServerReplicationParameters (Core.Maybe Types.KmsKeyId)
srpKmsKeyId = Lens.field @"kmsKeyId"
{-# INLINEABLE srpKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

-- | The license type for creating a replication job for the server.
--
-- /Note:/ Consider using 'licenseType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srpLicenseType :: Lens.Lens' ServerReplicationParameters (Core.Maybe Types.LicenseType)
srpLicenseType = Lens.field @"licenseType"
{-# INLINEABLE srpLicenseType #-}
{-# DEPRECATED licenseType "Use generic-lens or generic-optics with 'licenseType' instead"  #-}

-- | The number of recent AMIs to keep when creating a replication job for this server.
--
-- /Note:/ Consider using 'numberOfRecentAmisToKeep' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srpNumberOfRecentAmisToKeep :: Lens.Lens' ServerReplicationParameters (Core.Maybe Core.Int)
srpNumberOfRecentAmisToKeep = Lens.field @"numberOfRecentAmisToKeep"
{-# INLINEABLE srpNumberOfRecentAmisToKeep #-}
{-# DEPRECATED numberOfRecentAmisToKeep "Use generic-lens or generic-optics with 'numberOfRecentAmisToKeep' instead"  #-}

-- | Indicates whether to run the replication job one time.
--
-- /Note:/ Consider using 'runOnce' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srpRunOnce :: Lens.Lens' ServerReplicationParameters (Core.Maybe Core.Bool)
srpRunOnce = Lens.field @"runOnce"
{-# INLINEABLE srpRunOnce #-}
{-# DEPRECATED runOnce "Use generic-lens or generic-optics with 'runOnce' instead"  #-}

-- | The seed time for creating a replication job for the server.
--
-- /Note:/ Consider using 'seedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srpSeedTime :: Lens.Lens' ServerReplicationParameters (Core.Maybe Core.NominalDiffTime)
srpSeedTime = Lens.field @"seedTime"
{-# INLINEABLE srpSeedTime #-}
{-# DEPRECATED seedTime "Use generic-lens or generic-optics with 'seedTime' instead"  #-}

instance Core.FromJSON ServerReplicationParameters where
        toJSON ServerReplicationParameters{..}
          = Core.object
              (Core.catMaybes
                 [("encrypted" Core..=) Core.<$> encrypted,
                  ("frequency" Core..=) Core.<$> frequency,
                  ("kmsKeyId" Core..=) Core.<$> kmsKeyId,
                  ("licenseType" Core..=) Core.<$> licenseType,
                  ("numberOfRecentAmisToKeep" Core..=) Core.<$>
                    numberOfRecentAmisToKeep,
                  ("runOnce" Core..=) Core.<$> runOnce,
                  ("seedTime" Core..=) Core.<$> seedTime])

instance Core.FromJSON ServerReplicationParameters where
        parseJSON
          = Core.withObject "ServerReplicationParameters" Core.$
              \ x ->
                ServerReplicationParameters' Core.<$>
                  (x Core..:? "encrypted") Core.<*> x Core..:? "frequency" Core.<*>
                    x Core..:? "kmsKeyId"
                    Core.<*> x Core..:? "licenseType"
                    Core.<*> x Core..:? "numberOfRecentAmisToKeep"
                    Core.<*> x Core..:? "runOnce"
                    Core.<*> x Core..:? "seedTime"
