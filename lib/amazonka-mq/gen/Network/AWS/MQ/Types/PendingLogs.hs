{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.PendingLogs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MQ.Types.PendingLogs
  ( PendingLogs (..)
  -- * Smart constructor
  , mkPendingLogs
  -- * Lenses
  , plAudit
  , plGeneral
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The list of information about logs to be enabled for the specified broker.
--
-- /See:/ 'mkPendingLogs' smart constructor.
data PendingLogs = PendingLogs'
  { audit :: Core.Maybe Core.Bool
    -- ^ Enables audit logging. Every user management action made using JMX or the ActiveMQ Web Console is logged.
  , general :: Core.Maybe Core.Bool
    -- ^ Enables general logging.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PendingLogs' value with any optional fields omitted.
mkPendingLogs
    :: PendingLogs
mkPendingLogs
  = PendingLogs'{audit = Core.Nothing, general = Core.Nothing}

-- | Enables audit logging. Every user management action made using JMX or the ActiveMQ Web Console is logged.
--
-- /Note:/ Consider using 'audit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plAudit :: Lens.Lens' PendingLogs (Core.Maybe Core.Bool)
plAudit = Lens.field @"audit"
{-# INLINEABLE plAudit #-}
{-# DEPRECATED audit "Use generic-lens or generic-optics with 'audit' instead"  #-}

-- | Enables general logging.
--
-- /Note:/ Consider using 'general' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plGeneral :: Lens.Lens' PendingLogs (Core.Maybe Core.Bool)
plGeneral = Lens.field @"general"
{-# INLINEABLE plGeneral #-}
{-# DEPRECATED general "Use generic-lens or generic-optics with 'general' instead"  #-}

instance Core.FromJSON PendingLogs where
        parseJSON
          = Core.withObject "PendingLogs" Core.$
              \ x ->
                PendingLogs' Core.<$>
                  (x Core..:? "audit") Core.<*> x Core..:? "general"
