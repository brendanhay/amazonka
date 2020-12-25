{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.Logs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.Logs
  ( Logs (..),

    -- * Smart constructor
    mkLogs,

    -- * Lenses
    lAudit,
    lGeneral,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The list of information about logs to be enabled for the specified broker.
--
-- /See:/ 'mkLogs' smart constructor.
data Logs = Logs'
  { -- | Enables audit logging. Every user management action made using JMX or the ActiveMQ Web Console is logged. Does not apply to RabbitMQ brokers.
    audit :: Core.Maybe Core.Bool,
    -- | Enables general logging.
    general :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Logs' value with any optional fields omitted.
mkLogs ::
  Logs
mkLogs = Logs' {audit = Core.Nothing, general = Core.Nothing}

-- | Enables audit logging. Every user management action made using JMX or the ActiveMQ Web Console is logged. Does not apply to RabbitMQ brokers.
--
-- /Note:/ Consider using 'audit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lAudit :: Lens.Lens' Logs (Core.Maybe Core.Bool)
lAudit = Lens.field @"audit"
{-# DEPRECATED lAudit "Use generic-lens or generic-optics with 'audit' instead." #-}

-- | Enables general logging.
--
-- /Note:/ Consider using 'general' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lGeneral :: Lens.Lens' Logs (Core.Maybe Core.Bool)
lGeneral = Lens.field @"general"
{-# DEPRECATED lGeneral "Use generic-lens or generic-optics with 'general' instead." #-}

instance Core.FromJSON Logs where
  toJSON Logs {..} =
    Core.object
      ( Core.catMaybes
          [ ("audit" Core..=) Core.<$> audit,
            ("general" Core..=) Core.<$> general
          ]
      )

instance Core.FromJSON Logs where
  parseJSON =
    Core.withObject "Logs" Core.$
      \x ->
        Logs' Core.<$> (x Core..:? "audit") Core.<*> (x Core..:? "general")
