{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.Types.DestinationBackup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudHSMv2.Types.DestinationBackup
  ( DestinationBackup (..)
  -- * Smart constructor
  , mkDestinationBackup
  -- * Lenses
  , dbCreateTimestamp
  , dbSourceBackup
  , dbSourceCluster
  , dbSourceRegion
  ) where

import qualified Network.AWS.CloudHSMv2.Types.BackupId as Types
import qualified Network.AWS.CloudHSMv2.Types.ClusterId as Types
import qualified Network.AWS.CloudHSMv2.Types.SourceRegion as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the backup that will be copied and created by the 'CopyBackupToRegion' operation.
--
-- /See:/ 'mkDestinationBackup' smart constructor.
data DestinationBackup = DestinationBackup'
  { createTimestamp :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time when both the source backup was created.
  , sourceBackup :: Core.Maybe Types.BackupId
    -- ^ The identifier (ID) of the source backup from which the new backup was copied.
  , sourceCluster :: Core.Maybe Types.ClusterId
    -- ^ The identifier (ID) of the cluster containing the source backup from which the new backup was copied.
  , sourceRegion :: Core.Maybe Types.SourceRegion
    -- ^ The AWS region that contains the source backup from which the new backup was copied.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DestinationBackup' value with any optional fields omitted.
mkDestinationBackup
    :: DestinationBackup
mkDestinationBackup
  = DestinationBackup'{createTimestamp = Core.Nothing,
                       sourceBackup = Core.Nothing, sourceCluster = Core.Nothing,
                       sourceRegion = Core.Nothing}

-- | The date and time when both the source backup was created.
--
-- /Note:/ Consider using 'createTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbCreateTimestamp :: Lens.Lens' DestinationBackup (Core.Maybe Core.NominalDiffTime)
dbCreateTimestamp = Lens.field @"createTimestamp"
{-# INLINEABLE dbCreateTimestamp #-}
{-# DEPRECATED createTimestamp "Use generic-lens or generic-optics with 'createTimestamp' instead"  #-}

-- | The identifier (ID) of the source backup from which the new backup was copied.
--
-- /Note:/ Consider using 'sourceBackup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbSourceBackup :: Lens.Lens' DestinationBackup (Core.Maybe Types.BackupId)
dbSourceBackup = Lens.field @"sourceBackup"
{-# INLINEABLE dbSourceBackup #-}
{-# DEPRECATED sourceBackup "Use generic-lens or generic-optics with 'sourceBackup' instead"  #-}

-- | The identifier (ID) of the cluster containing the source backup from which the new backup was copied.
--
-- /Note:/ Consider using 'sourceCluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbSourceCluster :: Lens.Lens' DestinationBackup (Core.Maybe Types.ClusterId)
dbSourceCluster = Lens.field @"sourceCluster"
{-# INLINEABLE dbSourceCluster #-}
{-# DEPRECATED sourceCluster "Use generic-lens or generic-optics with 'sourceCluster' instead"  #-}

-- | The AWS region that contains the source backup from which the new backup was copied.
--
-- /Note:/ Consider using 'sourceRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbSourceRegion :: Lens.Lens' DestinationBackup (Core.Maybe Types.SourceRegion)
dbSourceRegion = Lens.field @"sourceRegion"
{-# INLINEABLE dbSourceRegion #-}
{-# DEPRECATED sourceRegion "Use generic-lens or generic-optics with 'sourceRegion' instead"  #-}

instance Core.FromJSON DestinationBackup where
        parseJSON
          = Core.withObject "DestinationBackup" Core.$
              \ x ->
                DestinationBackup' Core.<$>
                  (x Core..:? "CreateTimestamp") Core.<*> x Core..:? "SourceBackup"
                    Core.<*> x Core..:? "SourceCluster"
                    Core.<*> x Core..:? "SourceRegion"
