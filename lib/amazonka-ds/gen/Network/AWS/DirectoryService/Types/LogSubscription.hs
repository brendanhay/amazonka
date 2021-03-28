{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.LogSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectoryService.Types.LogSubscription
  ( LogSubscription (..)
  -- * Smart constructor
  , mkLogSubscription
  -- * Lenses
  , lsDirectoryId
  , lsLogGroupName
  , lsSubscriptionCreatedDateTime
  ) where

import qualified Network.AWS.DirectoryService.Types.DirectoryId as Types
import qualified Network.AWS.DirectoryService.Types.LogGroupName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a log subscription, which tracks real-time data from a chosen log group to a specified destination.
--
-- /See:/ 'mkLogSubscription' smart constructor.
data LogSubscription = LogSubscription'
  { directoryId :: Core.Maybe Types.DirectoryId
    -- ^ Identifier (ID) of the directory that you want to associate with the log subscription.
  , logGroupName :: Core.Maybe Types.LogGroupName
    -- ^ The name of the log group.
  , subscriptionCreatedDateTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time that the log subscription was created.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'LogSubscription' value with any optional fields omitted.
mkLogSubscription
    :: LogSubscription
mkLogSubscription
  = LogSubscription'{directoryId = Core.Nothing,
                     logGroupName = Core.Nothing,
                     subscriptionCreatedDateTime = Core.Nothing}

-- | Identifier (ID) of the directory that you want to associate with the log subscription.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsDirectoryId :: Lens.Lens' LogSubscription (Core.Maybe Types.DirectoryId)
lsDirectoryId = Lens.field @"directoryId"
{-# INLINEABLE lsDirectoryId #-}
{-# DEPRECATED directoryId "Use generic-lens or generic-optics with 'directoryId' instead"  #-}

-- | The name of the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsLogGroupName :: Lens.Lens' LogSubscription (Core.Maybe Types.LogGroupName)
lsLogGroupName = Lens.field @"logGroupName"
{-# INLINEABLE lsLogGroupName #-}
{-# DEPRECATED logGroupName "Use generic-lens or generic-optics with 'logGroupName' instead"  #-}

-- | The date and time that the log subscription was created.
--
-- /Note:/ Consider using 'subscriptionCreatedDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsSubscriptionCreatedDateTime :: Lens.Lens' LogSubscription (Core.Maybe Core.NominalDiffTime)
lsSubscriptionCreatedDateTime = Lens.field @"subscriptionCreatedDateTime"
{-# INLINEABLE lsSubscriptionCreatedDateTime #-}
{-# DEPRECATED subscriptionCreatedDateTime "Use generic-lens or generic-optics with 'subscriptionCreatedDateTime' instead"  #-}

instance Core.FromJSON LogSubscription where
        parseJSON
          = Core.withObject "LogSubscription" Core.$
              \ x ->
                LogSubscription' Core.<$>
                  (x Core..:? "DirectoryId") Core.<*> x Core..:? "LogGroupName"
                    Core.<*> x Core..:? "SubscriptionCreatedDateTime"
