{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ObjectLockRetention
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.ObjectLockRetention
  ( ObjectLockRetention (..)
  -- * Smart constructor
  , mkObjectLockRetention
  -- * Lenses
  , olrMode
  , olrRetainUntilDate
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.ObjectLockRetentionMode as Types

-- | A Retention configuration for an object.
--
-- /See:/ 'mkObjectLockRetention' smart constructor.
data ObjectLockRetention = ObjectLockRetention'
  { mode :: Core.Maybe Types.ObjectLockRetentionMode
    -- ^ Indicates the Retention mode for the specified object.
  , retainUntilDate :: Core.Maybe Core.UTCTime
    -- ^ The date on which this Object Lock Retention will expire.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ObjectLockRetention' value with any optional fields omitted.
mkObjectLockRetention
    :: ObjectLockRetention
mkObjectLockRetention
  = ObjectLockRetention'{mode = Core.Nothing,
                         retainUntilDate = Core.Nothing}

-- | Indicates the Retention mode for the specified object.
--
-- /Note:/ Consider using 'mode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
olrMode :: Lens.Lens' ObjectLockRetention (Core.Maybe Types.ObjectLockRetentionMode)
olrMode = Lens.field @"mode"
{-# INLINEABLE olrMode #-}
{-# DEPRECATED mode "Use generic-lens or generic-optics with 'mode' instead"  #-}

-- | The date on which this Object Lock Retention will expire.
--
-- /Note:/ Consider using 'retainUntilDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
olrRetainUntilDate :: Lens.Lens' ObjectLockRetention (Core.Maybe Core.UTCTime)
olrRetainUntilDate = Lens.field @"retainUntilDate"
{-# INLINEABLE olrRetainUntilDate #-}
{-# DEPRECATED retainUntilDate "Use generic-lens or generic-optics with 'retainUntilDate' instead"  #-}

instance Core.ToXML ObjectLockRetention where
        toXML ObjectLockRetention{..}
          = Core.maybe Core.mempty (Core.toXMLElement "Mode") mode Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "RetainUntilDate")
                retainUntilDate

instance Core.FromXML ObjectLockRetention where
        parseXML x
          = ObjectLockRetention' Core.<$>
              (x Core..@? "Mode") Core.<*> x Core..@? "RetainUntilDate"
