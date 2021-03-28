{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PeeringAttachmentStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.PeeringAttachmentStatus
  ( PeeringAttachmentStatus (..)
  -- * Smart constructor
  , mkPeeringAttachmentStatus
  -- * Lenses
  , pasCode
  , pasMessage
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The status of the transit gateway peering attachment.
--
-- /See:/ 'mkPeeringAttachmentStatus' smart constructor.
data PeeringAttachmentStatus = PeeringAttachmentStatus'
  { code :: Core.Maybe Core.Text
    -- ^ The status code.
  , message :: Core.Maybe Core.Text
    -- ^ The status message, if applicable.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PeeringAttachmentStatus' value with any optional fields omitted.
mkPeeringAttachmentStatus
    :: PeeringAttachmentStatus
mkPeeringAttachmentStatus
  = PeeringAttachmentStatus'{code = Core.Nothing,
                             message = Core.Nothing}

-- | The status code.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pasCode :: Lens.Lens' PeeringAttachmentStatus (Core.Maybe Core.Text)
pasCode = Lens.field @"code"
{-# INLINEABLE pasCode #-}
{-# DEPRECATED code "Use generic-lens or generic-optics with 'code' instead"  #-}

-- | The status message, if applicable.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pasMessage :: Lens.Lens' PeeringAttachmentStatus (Core.Maybe Core.Text)
pasMessage = Lens.field @"message"
{-# INLINEABLE pasMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

instance Core.FromXML PeeringAttachmentStatus where
        parseXML x
          = PeeringAttachmentStatus' Core.<$>
              (x Core..@? "code") Core.<*> x Core..@? "message"
