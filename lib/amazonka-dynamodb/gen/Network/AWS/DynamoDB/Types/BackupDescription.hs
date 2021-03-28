{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.BackupDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.BackupDescription
  ( BackupDescription (..)
  -- * Smart constructor
  , mkBackupDescription
  -- * Lenses
  , bdBackupDetails
  , bdSourceTableDetails
  , bdSourceTableFeatureDetails
  ) where

import qualified Network.AWS.DynamoDB.Types.BackupDetails as Types
import qualified Network.AWS.DynamoDB.Types.SourceTableDetails as Types
import qualified Network.AWS.DynamoDB.Types.SourceTableFeatureDetails as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains the description of the backup created for the table.
--
-- /See:/ 'mkBackupDescription' smart constructor.
data BackupDescription = BackupDescription'
  { backupDetails :: Core.Maybe Types.BackupDetails
    -- ^ Contains the details of the backup created for the table. 
  , sourceTableDetails :: Core.Maybe Types.SourceTableDetails
    -- ^ Contains the details of the table when the backup was created. 
  , sourceTableFeatureDetails :: Core.Maybe Types.SourceTableFeatureDetails
    -- ^ Contains the details of the features enabled on the table when the backup was created. For example, LSIs, GSIs, streams, TTL.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'BackupDescription' value with any optional fields omitted.
mkBackupDescription
    :: BackupDescription
mkBackupDescription
  = BackupDescription'{backupDetails = Core.Nothing,
                       sourceTableDetails = Core.Nothing,
                       sourceTableFeatureDetails = Core.Nothing}

-- | Contains the details of the backup created for the table. 
--
-- /Note:/ Consider using 'backupDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdBackupDetails :: Lens.Lens' BackupDescription (Core.Maybe Types.BackupDetails)
bdBackupDetails = Lens.field @"backupDetails"
{-# INLINEABLE bdBackupDetails #-}
{-# DEPRECATED backupDetails "Use generic-lens or generic-optics with 'backupDetails' instead"  #-}

-- | Contains the details of the table when the backup was created. 
--
-- /Note:/ Consider using 'sourceTableDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdSourceTableDetails :: Lens.Lens' BackupDescription (Core.Maybe Types.SourceTableDetails)
bdSourceTableDetails = Lens.field @"sourceTableDetails"
{-# INLINEABLE bdSourceTableDetails #-}
{-# DEPRECATED sourceTableDetails "Use generic-lens or generic-optics with 'sourceTableDetails' instead"  #-}

-- | Contains the details of the features enabled on the table when the backup was created. For example, LSIs, GSIs, streams, TTL.
--
-- /Note:/ Consider using 'sourceTableFeatureDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdSourceTableFeatureDetails :: Lens.Lens' BackupDescription (Core.Maybe Types.SourceTableFeatureDetails)
bdSourceTableFeatureDetails = Lens.field @"sourceTableFeatureDetails"
{-# INLINEABLE bdSourceTableFeatureDetails #-}
{-# DEPRECATED sourceTableFeatureDetails "Use generic-lens or generic-optics with 'sourceTableFeatureDetails' instead"  #-}

instance Core.FromJSON BackupDescription where
        parseJSON
          = Core.withObject "BackupDescription" Core.$
              \ x ->
                BackupDescription' Core.<$>
                  (x Core..:? "BackupDetails") Core.<*>
                    x Core..:? "SourceTableDetails"
                    Core.<*> x Core..:? "SourceTableFeatureDetails"
