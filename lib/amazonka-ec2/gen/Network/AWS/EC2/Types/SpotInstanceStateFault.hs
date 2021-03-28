{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotInstanceStateFault
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.SpotInstanceStateFault
  ( SpotInstanceStateFault (..)
  -- * Smart constructor
  , mkSpotInstanceStateFault
  -- * Lenses
  , sisfCode
  , sisfMessage
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a Spot Instance state change.
--
-- /See:/ 'mkSpotInstanceStateFault' smart constructor.
data SpotInstanceStateFault = SpotInstanceStateFault'
  { code :: Core.Maybe Core.Text
    -- ^ The reason code for the Spot Instance state change.
  , message :: Core.Maybe Core.Text
    -- ^ The message for the Spot Instance state change.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SpotInstanceStateFault' value with any optional fields omitted.
mkSpotInstanceStateFault
    :: SpotInstanceStateFault
mkSpotInstanceStateFault
  = SpotInstanceStateFault'{code = Core.Nothing,
                            message = Core.Nothing}

-- | The reason code for the Spot Instance state change.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sisfCode :: Lens.Lens' SpotInstanceStateFault (Core.Maybe Core.Text)
sisfCode = Lens.field @"code"
{-# INLINEABLE sisfCode #-}
{-# DEPRECATED code "Use generic-lens or generic-optics with 'code' instead"  #-}

-- | The message for the Spot Instance state change.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sisfMessage :: Lens.Lens' SpotInstanceStateFault (Core.Maybe Core.Text)
sisfMessage = Lens.field @"message"
{-# INLINEABLE sisfMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

instance Core.FromXML SpotInstanceStateFault where
        parseXML x
          = SpotInstanceStateFault' Core.<$>
              (x Core..@? "code") Core.<*> x Core..@? "message"
