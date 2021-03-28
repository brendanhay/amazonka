{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.OptionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudSearch.Types.OptionStatus
  ( OptionStatus (..)
  -- * Smart constructor
  , mkOptionStatus
  -- * Lenses
  , osCreationDate
  , osUpdateDate
  , osState
  , osPendingDeletion
  , osUpdateVersion
  ) where

import qualified Network.AWS.CloudSearch.Types.OptionState as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The status of domain configuration option.
--
-- /See:/ 'mkOptionStatus' smart constructor.
data OptionStatus = OptionStatus'
  { creationDate :: Core.UTCTime
    -- ^ A timestamp for when this option was created.
  , updateDate :: Core.UTCTime
    -- ^ A timestamp for when this option was last updated.
  , state :: Types.OptionState
    -- ^ The state of processing a change to an option. Possible values:
--
--
--     * @RequiresIndexDocuments@ : the option's latest value will not be deployed until 'IndexDocuments' has been called and indexing is complete.
--
--     * @Processing@ : the option's latest value is in the process of being activated. 
--
--     * @Active@ : the option's latest value is completely deployed.
--
--     * @FailedToValidate@ : the option value is not compatible with the domain's data and cannot be used to index the data. You must either modify the option value or update or remove the incompatible documents.
--
  , pendingDeletion :: Core.Maybe Core.Bool
    -- ^ Indicates that the option will be deleted once processing is complete.
  , updateVersion :: Core.Maybe Core.Natural
    -- ^ A unique integer that indicates when this option was last updated.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'OptionStatus' value with any optional fields omitted.
mkOptionStatus
    :: Core.UTCTime -- ^ 'creationDate'
    -> Core.UTCTime -- ^ 'updateDate'
    -> Types.OptionState -- ^ 'state'
    -> OptionStatus
mkOptionStatus creationDate updateDate state
  = OptionStatus'{creationDate, updateDate, state,
                  pendingDeletion = Core.Nothing, updateVersion = Core.Nothing}

-- | A timestamp for when this option was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osCreationDate :: Lens.Lens' OptionStatus Core.UTCTime
osCreationDate = Lens.field @"creationDate"
{-# INLINEABLE osCreationDate #-}
{-# DEPRECATED creationDate "Use generic-lens or generic-optics with 'creationDate' instead"  #-}

-- | A timestamp for when this option was last updated.
--
-- /Note:/ Consider using 'updateDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osUpdateDate :: Lens.Lens' OptionStatus Core.UTCTime
osUpdateDate = Lens.field @"updateDate"
{-# INLINEABLE osUpdateDate #-}
{-# DEPRECATED updateDate "Use generic-lens or generic-optics with 'updateDate' instead"  #-}

-- | The state of processing a change to an option. Possible values:
--
--
--     * @RequiresIndexDocuments@ : the option's latest value will not be deployed until 'IndexDocuments' has been called and indexing is complete.
--
--     * @Processing@ : the option's latest value is in the process of being activated. 
--
--     * @Active@ : the option's latest value is completely deployed.
--
--     * @FailedToValidate@ : the option value is not compatible with the domain's data and cannot be used to index the data. You must either modify the option value or update or remove the incompatible documents.
--
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osState :: Lens.Lens' OptionStatus Types.OptionState
osState = Lens.field @"state"
{-# INLINEABLE osState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | Indicates that the option will be deleted once processing is complete.
--
-- /Note:/ Consider using 'pendingDeletion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osPendingDeletion :: Lens.Lens' OptionStatus (Core.Maybe Core.Bool)
osPendingDeletion = Lens.field @"pendingDeletion"
{-# INLINEABLE osPendingDeletion #-}
{-# DEPRECATED pendingDeletion "Use generic-lens or generic-optics with 'pendingDeletion' instead"  #-}

-- | A unique integer that indicates when this option was last updated.
--
-- /Note:/ Consider using 'updateVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osUpdateVersion :: Lens.Lens' OptionStatus (Core.Maybe Core.Natural)
osUpdateVersion = Lens.field @"updateVersion"
{-# INLINEABLE osUpdateVersion #-}
{-# DEPRECATED updateVersion "Use generic-lens or generic-optics with 'updateVersion' instead"  #-}

instance Core.FromXML OptionStatus where
        parseXML x
          = OptionStatus' Core.<$>
              (x Core..@ "CreationDate") Core.<*> x Core..@ "UpdateDate" Core.<*>
                x Core..@ "State"
                Core.<*> x Core..@? "PendingDeletion"
                Core.<*> x Core..@? "UpdateVersion"
