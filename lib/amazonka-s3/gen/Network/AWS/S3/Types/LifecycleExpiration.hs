{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.LifecycleExpiration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.LifecycleExpiration
  ( LifecycleExpiration (..)
  -- * Smart constructor
  , mkLifecycleExpiration
  -- * Lenses
  , leDate
  , leDays
  , leExpiredObjectDeleteMarker
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types

-- | Container for the expiration for the lifecycle of the object.
--
-- /See:/ 'mkLifecycleExpiration' smart constructor.
data LifecycleExpiration = LifecycleExpiration'
  { date :: Core.Maybe Core.UTCTime
    -- ^ Indicates at what date the object is to be moved or deleted. Should be in GMT ISO 8601 Format.
  , days :: Core.Maybe Core.Int
    -- ^ Indicates the lifetime, in days, of the objects that are subject to the rule. The value must be a non-zero positive integer.
  , expiredObjectDeleteMarker :: Core.Maybe Core.Bool
    -- ^ Indicates whether Amazon S3 will remove a delete marker with no noncurrent versions. If set to true, the delete marker will be expired; if set to false the policy takes no action. This cannot be specified with Days or Date in a Lifecycle Expiration Policy.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'LifecycleExpiration' value with any optional fields omitted.
mkLifecycleExpiration
    :: LifecycleExpiration
mkLifecycleExpiration
  = LifecycleExpiration'{date = Core.Nothing, days = Core.Nothing,
                         expiredObjectDeleteMarker = Core.Nothing}

-- | Indicates at what date the object is to be moved or deleted. Should be in GMT ISO 8601 Format.
--
-- /Note:/ Consider using 'date' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leDate :: Lens.Lens' LifecycleExpiration (Core.Maybe Core.UTCTime)
leDate = Lens.field @"date"
{-# INLINEABLE leDate #-}
{-# DEPRECATED date "Use generic-lens or generic-optics with 'date' instead"  #-}

-- | Indicates the lifetime, in days, of the objects that are subject to the rule. The value must be a non-zero positive integer.
--
-- /Note:/ Consider using 'days' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leDays :: Lens.Lens' LifecycleExpiration (Core.Maybe Core.Int)
leDays = Lens.field @"days"
{-# INLINEABLE leDays #-}
{-# DEPRECATED days "Use generic-lens or generic-optics with 'days' instead"  #-}

-- | Indicates whether Amazon S3 will remove a delete marker with no noncurrent versions. If set to true, the delete marker will be expired; if set to false the policy takes no action. This cannot be specified with Days or Date in a Lifecycle Expiration Policy.
--
-- /Note:/ Consider using 'expiredObjectDeleteMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leExpiredObjectDeleteMarker :: Lens.Lens' LifecycleExpiration (Core.Maybe Core.Bool)
leExpiredObjectDeleteMarker = Lens.field @"expiredObjectDeleteMarker"
{-# INLINEABLE leExpiredObjectDeleteMarker #-}
{-# DEPRECATED expiredObjectDeleteMarker "Use generic-lens or generic-optics with 'expiredObjectDeleteMarker' instead"  #-}

instance Core.ToXML LifecycleExpiration where
        toXML LifecycleExpiration{..}
          = Core.maybe Core.mempty (Core.toXMLElement "Date") date Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "Days") days
              Core.<>
              Core.maybe Core.mempty
                (Core.toXMLElement "ExpiredObjectDeleteMarker")
                expiredObjectDeleteMarker

instance Core.FromXML LifecycleExpiration where
        parseXML x
          = LifecycleExpiration' Core.<$>
              (x Core..@? "Date") Core.<*> x Core..@? "Days" Core.<*>
                x Core..@? "ExpiredObjectDeleteMarker"
