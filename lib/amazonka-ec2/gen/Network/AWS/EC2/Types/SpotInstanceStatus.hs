{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotInstanceStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.SpotInstanceStatus
  ( SpotInstanceStatus (..)
  -- * Smart constructor
  , mkSpotInstanceStatus
  -- * Lenses
  , sisCode
  , sisMessage
  , sisUpdateTime
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the status of a Spot Instance request.
--
-- /See:/ 'mkSpotInstanceStatus' smart constructor.
data SpotInstanceStatus = SpotInstanceStatus'
  { code :: Core.Maybe Core.Text
    -- ^ The status code. For a list of status codes, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-bid-status.html#spot-instance-bid-status-understand Spot status codes> in the /Amazon EC2 User Guide for Linux Instances/ .
  , message :: Core.Maybe Core.Text
    -- ^ The description for the status code.
  , updateTime :: Core.Maybe Core.UTCTime
    -- ^ The date and time of the most recent status update, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'SpotInstanceStatus' value with any optional fields omitted.
mkSpotInstanceStatus
    :: SpotInstanceStatus
mkSpotInstanceStatus
  = SpotInstanceStatus'{code = Core.Nothing, message = Core.Nothing,
                        updateTime = Core.Nothing}

-- | The status code. For a list of status codes, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-bid-status.html#spot-instance-bid-status-understand Spot status codes> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sisCode :: Lens.Lens' SpotInstanceStatus (Core.Maybe Core.Text)
sisCode = Lens.field @"code"
{-# INLINEABLE sisCode #-}
{-# DEPRECATED code "Use generic-lens or generic-optics with 'code' instead"  #-}

-- | The description for the status code.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sisMessage :: Lens.Lens' SpotInstanceStatus (Core.Maybe Core.Text)
sisMessage = Lens.field @"message"
{-# INLINEABLE sisMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

-- | The date and time of the most recent status update, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
--
-- /Note:/ Consider using 'updateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sisUpdateTime :: Lens.Lens' SpotInstanceStatus (Core.Maybe Core.UTCTime)
sisUpdateTime = Lens.field @"updateTime"
{-# INLINEABLE sisUpdateTime #-}
{-# DEPRECATED updateTime "Use generic-lens or generic-optics with 'updateTime' instead"  #-}

instance Core.FromXML SpotInstanceStatus where
        parseXML x
          = SpotInstanceStatus' Core.<$>
              (x Core..@? "code") Core.<*> x Core..@? "message" Core.<*>
                x Core..@? "updateTime"
