{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Transition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.Transition
  ( Transition (..)
  -- * Smart constructor
  , mkTransition
  -- * Lenses
  , tfDate
  , tfDays
  , tfStorageClass
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.TransitionStorageClass as Types

-- | Specifies when an object transitions to a specified storage class. For more information about Amazon S3 lifecycle configuration rules, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/lifecycle-transition-general-considerations.html Transitioning Objects Using Amazon S3 Lifecycle> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /See:/ 'mkTransition' smart constructor.
data Transition = Transition'
  { date :: Core.Maybe Core.UTCTime
    -- ^ Indicates when objects are transitioned to the specified storage class. The date value must be in ISO 8601 format. The time is always midnight UTC.
  , days :: Core.Maybe Core.Int
    -- ^ Indicates the number of days after creation when objects are transitioned to the specified storage class. The value must be a positive integer.
  , storageClass :: Core.Maybe Types.TransitionStorageClass
    -- ^ The storage class to which you want the object to transition.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Transition' value with any optional fields omitted.
mkTransition
    :: Transition
mkTransition
  = Transition'{date = Core.Nothing, days = Core.Nothing,
                storageClass = Core.Nothing}

-- | Indicates when objects are transitioned to the specified storage class. The date value must be in ISO 8601 format. The time is always midnight UTC.
--
-- /Note:/ Consider using 'date' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfDate :: Lens.Lens' Transition (Core.Maybe Core.UTCTime)
tfDate = Lens.field @"date"
{-# INLINEABLE tfDate #-}
{-# DEPRECATED date "Use generic-lens or generic-optics with 'date' instead"  #-}

-- | Indicates the number of days after creation when objects are transitioned to the specified storage class. The value must be a positive integer.
--
-- /Note:/ Consider using 'days' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfDays :: Lens.Lens' Transition (Core.Maybe Core.Int)
tfDays = Lens.field @"days"
{-# INLINEABLE tfDays #-}
{-# DEPRECATED days "Use generic-lens or generic-optics with 'days' instead"  #-}

-- | The storage class to which you want the object to transition.
--
-- /Note:/ Consider using 'storageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfStorageClass :: Lens.Lens' Transition (Core.Maybe Types.TransitionStorageClass)
tfStorageClass = Lens.field @"storageClass"
{-# INLINEABLE tfStorageClass #-}
{-# DEPRECATED storageClass "Use generic-lens or generic-optics with 'storageClass' instead"  #-}

instance Core.ToXML Transition where
        toXML Transition{..}
          = Core.maybe Core.mempty (Core.toXMLElement "Date") date Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "Days") days
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "StorageClass")
                storageClass

instance Core.FromXML Transition where
        parseXML x
          = Transition' Core.<$>
              (x Core..@? "Date") Core.<*> x Core..@? "Days" Core.<*>
                x Core..@? "StorageClass"
