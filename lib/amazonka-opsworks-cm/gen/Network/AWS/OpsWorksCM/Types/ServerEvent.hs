{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.Types.ServerEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorksCM.Types.ServerEvent
  ( ServerEvent (..)
  -- * Smart constructor
  , mkServerEvent
  -- * Lenses
  , seCreatedAt
  , seLogUrl
  , seMessage
  , seServerName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An event that is related to the server, such as the start of maintenance or backup. 
--
-- /See:/ 'mkServerEvent' smart constructor.
data ServerEvent = ServerEvent'
  { createdAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The time when the event occurred. 
  , logUrl :: Core.Maybe Core.Text
    -- ^ The Amazon S3 URL of the event's log file.
  , message :: Core.Maybe Core.Text
    -- ^ A human-readable informational or status message.
  , serverName :: Core.Maybe Core.Text
    -- ^ The name of the server on or for which the event occurred. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ServerEvent' value with any optional fields omitted.
mkServerEvent
    :: ServerEvent
mkServerEvent
  = ServerEvent'{createdAt = Core.Nothing, logUrl = Core.Nothing,
                 message = Core.Nothing, serverName = Core.Nothing}

-- | The time when the event occurred. 
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seCreatedAt :: Lens.Lens' ServerEvent (Core.Maybe Core.NominalDiffTime)
seCreatedAt = Lens.field @"createdAt"
{-# INLINEABLE seCreatedAt #-}
{-# DEPRECATED createdAt "Use generic-lens or generic-optics with 'createdAt' instead"  #-}

-- | The Amazon S3 URL of the event's log file.
--
-- /Note:/ Consider using 'logUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seLogUrl :: Lens.Lens' ServerEvent (Core.Maybe Core.Text)
seLogUrl = Lens.field @"logUrl"
{-# INLINEABLE seLogUrl #-}
{-# DEPRECATED logUrl "Use generic-lens or generic-optics with 'logUrl' instead"  #-}

-- | A human-readable informational or status message.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seMessage :: Lens.Lens' ServerEvent (Core.Maybe Core.Text)
seMessage = Lens.field @"message"
{-# INLINEABLE seMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

-- | The name of the server on or for which the event occurred. 
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seServerName :: Lens.Lens' ServerEvent (Core.Maybe Core.Text)
seServerName = Lens.field @"serverName"
{-# INLINEABLE seServerName #-}
{-# DEPRECATED serverName "Use generic-lens or generic-optics with 'serverName' instead"  #-}

instance Core.FromJSON ServerEvent where
        parseJSON
          = Core.withObject "ServerEvent" Core.$
              \ x ->
                ServerEvent' Core.<$>
                  (x Core..:? "CreatedAt") Core.<*> x Core..:? "LogUrl" Core.<*>
                    x Core..:? "Message"
                    Core.<*> x Core..:? "ServerName"
