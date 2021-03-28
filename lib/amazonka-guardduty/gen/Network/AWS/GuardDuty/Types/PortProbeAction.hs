{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.PortProbeAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.PortProbeAction
  ( PortProbeAction (..)
  -- * Smart constructor
  , mkPortProbeAction
  -- * Lenses
  , ppaBlocked
  , ppaPortProbeDetails
  ) where

import qualified Network.AWS.GuardDuty.Types.PortProbeDetail as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the PORT_PROBE action described in the finding.
--
-- /See:/ 'mkPortProbeAction' smart constructor.
data PortProbeAction = PortProbeAction'
  { blocked :: Core.Maybe Core.Bool
    -- ^ Indicates whether EC2 blocked the port probe to the instance, such as with an ACL.
  , portProbeDetails :: Core.Maybe [Types.PortProbeDetail]
    -- ^ A list of objects related to port probe details.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PortProbeAction' value with any optional fields omitted.
mkPortProbeAction
    :: PortProbeAction
mkPortProbeAction
  = PortProbeAction'{blocked = Core.Nothing,
                     portProbeDetails = Core.Nothing}

-- | Indicates whether EC2 blocked the port probe to the instance, such as with an ACL.
--
-- /Note:/ Consider using 'blocked' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppaBlocked :: Lens.Lens' PortProbeAction (Core.Maybe Core.Bool)
ppaBlocked = Lens.field @"blocked"
{-# INLINEABLE ppaBlocked #-}
{-# DEPRECATED blocked "Use generic-lens or generic-optics with 'blocked' instead"  #-}

-- | A list of objects related to port probe details.
--
-- /Note:/ Consider using 'portProbeDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppaPortProbeDetails :: Lens.Lens' PortProbeAction (Core.Maybe [Types.PortProbeDetail])
ppaPortProbeDetails = Lens.field @"portProbeDetails"
{-# INLINEABLE ppaPortProbeDetails #-}
{-# DEPRECATED portProbeDetails "Use generic-lens or generic-optics with 'portProbeDetails' instead"  #-}

instance Core.FromJSON PortProbeAction where
        parseJSON
          = Core.withObject "PortProbeAction" Core.$
              \ x ->
                PortProbeAction' Core.<$>
                  (x Core..:? "blocked") Core.<*> x Core..:? "portProbeDetails"
