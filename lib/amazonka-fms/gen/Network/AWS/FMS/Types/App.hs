{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.App
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.FMS.Types.App
  ( App (..)
  -- * Smart constructor
  , mkApp
  -- * Lenses
  , aAppName
  , aProtocol
  , aPort
  ) where

import qualified Network.AWS.FMS.Types.Protocol as Types
import qualified Network.AWS.FMS.Types.ResourceName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An individual AWS Firewall Manager application.
--
-- /See:/ 'mkApp' smart constructor.
data App = App'
  { appName :: Types.ResourceName
    -- ^ The application's name.
  , protocol :: Types.Protocol
    -- ^ The IP protocol name or number. The name can be one of @tcp@ , @udp@ , or @icmp@ . For information on possible numbers, see <https://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers> .
  , port :: Core.Natural
    -- ^ The application's port number, for example @80@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'App' value with any optional fields omitted.
mkApp
    :: Types.ResourceName -- ^ 'appName'
    -> Types.Protocol -- ^ 'protocol'
    -> Core.Natural -- ^ 'port'
    -> App
mkApp appName protocol port = App'{appName, protocol, port}

-- | The application's name.
--
-- /Note:/ Consider using 'appName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAppName :: Lens.Lens' App Types.ResourceName
aAppName = Lens.field @"appName"
{-# INLINEABLE aAppName #-}
{-# DEPRECATED appName "Use generic-lens or generic-optics with 'appName' instead"  #-}

-- | The IP protocol name or number. The name can be one of @tcp@ , @udp@ , or @icmp@ . For information on possible numbers, see <https://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers> .
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aProtocol :: Lens.Lens' App Types.Protocol
aProtocol = Lens.field @"protocol"
{-# INLINEABLE aProtocol #-}
{-# DEPRECATED protocol "Use generic-lens or generic-optics with 'protocol' instead"  #-}

-- | The application's port number, for example @80@ .
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aPort :: Lens.Lens' App Core.Natural
aPort = Lens.field @"port"
{-# INLINEABLE aPort #-}
{-# DEPRECATED port "Use generic-lens or generic-optics with 'port' instead"  #-}

instance Core.FromJSON App where
        toJSON App{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AppName" Core..= appName),
                  Core.Just ("Protocol" Core..= protocol),
                  Core.Just ("Port" Core..= port)])

instance Core.FromJSON App where
        parseJSON
          = Core.withObject "App" Core.$
              \ x ->
                App' Core.<$>
                  (x Core..: "AppName") Core.<*> x Core..: "Protocol" Core.<*>
                    x Core..: "Port"
