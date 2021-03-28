{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ObjectLockLegalHold
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.ObjectLockLegalHold
  ( ObjectLockLegalHold (..)
  -- * Smart constructor
  , mkObjectLockLegalHold
  -- * Lenses
  , ollhStatus
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.ObjectLockLegalHoldStatus as Types

-- | A Legal Hold configuration for an object.
--
-- /See:/ 'mkObjectLockLegalHold' smart constructor.
newtype ObjectLockLegalHold = ObjectLockLegalHold'
  { status :: Core.Maybe Types.ObjectLockLegalHoldStatus
    -- ^ Indicates whether the specified object has a Legal Hold in place.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ObjectLockLegalHold' value with any optional fields omitted.
mkObjectLockLegalHold
    :: ObjectLockLegalHold
mkObjectLockLegalHold = ObjectLockLegalHold'{status = Core.Nothing}

-- | Indicates whether the specified object has a Legal Hold in place.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ollhStatus :: Lens.Lens' ObjectLockLegalHold (Core.Maybe Types.ObjectLockLegalHoldStatus)
ollhStatus = Lens.field @"status"
{-# INLINEABLE ollhStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.ToXML ObjectLockLegalHold where
        toXML ObjectLockLegalHold{..}
          = Core.maybe Core.mempty (Core.toXMLElement "Status") status

instance Core.FromXML ObjectLockLegalHold where
        parseXML x = ObjectLockLegalHold' Core.<$> (x Core..@? "Status")
