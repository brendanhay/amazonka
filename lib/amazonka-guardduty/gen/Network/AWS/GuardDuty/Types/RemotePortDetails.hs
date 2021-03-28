{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.RemotePortDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.RemotePortDetails
  ( RemotePortDetails (..)
  -- * Smart constructor
  , mkRemotePortDetails
  -- * Lenses
  , rpdPort
  , rpdPortName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the remote port.
--
-- /See:/ 'mkRemotePortDetails' smart constructor.
data RemotePortDetails = RemotePortDetails'
  { port :: Core.Maybe Core.Int
    -- ^ The port number of the remote connection.
  , portName :: Core.Maybe Core.Text
    -- ^ The port name of the remote connection.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemotePortDetails' value with any optional fields omitted.
mkRemotePortDetails
    :: RemotePortDetails
mkRemotePortDetails
  = RemotePortDetails'{port = Core.Nothing, portName = Core.Nothing}

-- | The port number of the remote connection.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpdPort :: Lens.Lens' RemotePortDetails (Core.Maybe Core.Int)
rpdPort = Lens.field @"port"
{-# INLINEABLE rpdPort #-}
{-# DEPRECATED port "Use generic-lens or generic-optics with 'port' instead"  #-}

-- | The port name of the remote connection.
--
-- /Note:/ Consider using 'portName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpdPortName :: Lens.Lens' RemotePortDetails (Core.Maybe Core.Text)
rpdPortName = Lens.field @"portName"
{-# INLINEABLE rpdPortName #-}
{-# DEPRECATED portName "Use generic-lens or generic-optics with 'portName' instead"  #-}

instance Core.FromJSON RemotePortDetails where
        parseJSON
          = Core.withObject "RemotePortDetails" Core.$
              \ x ->
                RemotePortDetails' Core.<$>
                  (x Core..:? "port") Core.<*> x Core..:? "portName"
