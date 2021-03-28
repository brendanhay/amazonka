{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.Types.BackupRetentionPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudHSMv2.Types.BackupRetentionPolicy
  ( BackupRetentionPolicy (..)
  -- * Smart constructor
  , mkBackupRetentionPolicy
  -- * Lenses
  , brpType
  , brpValue
  ) where

import qualified Network.AWS.CloudHSMv2.Types.BackupRetentionType as Types
import qualified Network.AWS.CloudHSMv2.Types.Value as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A policy that defines the number of days to retain backups.
--
-- /See:/ 'mkBackupRetentionPolicy' smart constructor.
data BackupRetentionPolicy = BackupRetentionPolicy'
  { type' :: Core.Maybe Types.BackupRetentionType
    -- ^ The type of backup retention policy. For the @DAYS@ type, the value is the number of days to retain backups.
  , value :: Core.Maybe Types.Value
    -- ^ Use a value between 7 - 379.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BackupRetentionPolicy' value with any optional fields omitted.
mkBackupRetentionPolicy
    :: BackupRetentionPolicy
mkBackupRetentionPolicy
  = BackupRetentionPolicy'{type' = Core.Nothing,
                           value = Core.Nothing}

-- | The type of backup retention policy. For the @DAYS@ type, the value is the number of days to retain backups.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brpType :: Lens.Lens' BackupRetentionPolicy (Core.Maybe Types.BackupRetentionType)
brpType = Lens.field @"type'"
{-# INLINEABLE brpType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | Use a value between 7 - 379.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brpValue :: Lens.Lens' BackupRetentionPolicy (Core.Maybe Types.Value)
brpValue = Lens.field @"value"
{-# INLINEABLE brpValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromJSON BackupRetentionPolicy where
        toJSON BackupRetentionPolicy{..}
          = Core.object
              (Core.catMaybes
                 [("Type" Core..=) Core.<$> type',
                  ("Value" Core..=) Core.<$> value])

instance Core.FromJSON BackupRetentionPolicy where
        parseJSON
          = Core.withObject "BackupRetentionPolicy" Core.$
              \ x ->
                BackupRetentionPolicy' Core.<$>
                  (x Core..:? "Type") Core.<*> x Core..:? "Value"
