{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.LogsSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.LogsSummary
  ( LogsSummary (..),

    -- * Smart constructor
    mkLogsSummary,

    -- * Lenses
    lsAudit,
    lsAuditLogGroup,
    lsGeneral,
    lsGeneralLogGroup,
    lsPending,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MQ.Types.PendingLogs as Types
import qualified Network.AWS.Prelude as Core

-- | The list of information about logs currently enabled and pending to be deployed for the specified broker.
--
-- /See:/ 'mkLogsSummary' smart constructor.
data LogsSummary = LogsSummary'
  { -- | Enables audit logging. Every user management action made using JMX or the ActiveMQ Web Console is logged.
    audit :: Core.Maybe Core.Bool,
    -- | The location of the CloudWatch Logs log group where audit logs are sent.
    auditLogGroup :: Core.Maybe Core.Text,
    -- | Enables general logging.
    general :: Core.Maybe Core.Bool,
    -- | The location of the CloudWatch Logs log group where general logs are sent.
    generalLogGroup :: Core.Maybe Core.Text,
    -- | The list of information about logs pending to be deployed for the specified broker.
    pending :: Core.Maybe Types.PendingLogs
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LogsSummary' value with any optional fields omitted.
mkLogsSummary ::
  LogsSummary
mkLogsSummary =
  LogsSummary'
    { audit = Core.Nothing,
      auditLogGroup = Core.Nothing,
      general = Core.Nothing,
      generalLogGroup = Core.Nothing,
      pending = Core.Nothing
    }

-- | Enables audit logging. Every user management action made using JMX or the ActiveMQ Web Console is logged.
--
-- /Note:/ Consider using 'audit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsAudit :: Lens.Lens' LogsSummary (Core.Maybe Core.Bool)
lsAudit = Lens.field @"audit"
{-# DEPRECATED lsAudit "Use generic-lens or generic-optics with 'audit' instead." #-}

-- | The location of the CloudWatch Logs log group where audit logs are sent.
--
-- /Note:/ Consider using 'auditLogGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsAuditLogGroup :: Lens.Lens' LogsSummary (Core.Maybe Core.Text)
lsAuditLogGroup = Lens.field @"auditLogGroup"
{-# DEPRECATED lsAuditLogGroup "Use generic-lens or generic-optics with 'auditLogGroup' instead." #-}

-- | Enables general logging.
--
-- /Note:/ Consider using 'general' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsGeneral :: Lens.Lens' LogsSummary (Core.Maybe Core.Bool)
lsGeneral = Lens.field @"general"
{-# DEPRECATED lsGeneral "Use generic-lens or generic-optics with 'general' instead." #-}

-- | The location of the CloudWatch Logs log group where general logs are sent.
--
-- /Note:/ Consider using 'generalLogGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsGeneralLogGroup :: Lens.Lens' LogsSummary (Core.Maybe Core.Text)
lsGeneralLogGroup = Lens.field @"generalLogGroup"
{-# DEPRECATED lsGeneralLogGroup "Use generic-lens or generic-optics with 'generalLogGroup' instead." #-}

-- | The list of information about logs pending to be deployed for the specified broker.
--
-- /Note:/ Consider using 'pending' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsPending :: Lens.Lens' LogsSummary (Core.Maybe Types.PendingLogs)
lsPending = Lens.field @"pending"
{-# DEPRECATED lsPending "Use generic-lens or generic-optics with 'pending' instead." #-}

instance Core.FromJSON LogsSummary where
  parseJSON =
    Core.withObject "LogsSummary" Core.$
      \x ->
        LogsSummary'
          Core.<$> (x Core..:? "audit")
          Core.<*> (x Core..:? "auditLogGroup")
          Core.<*> (x Core..:? "general")
          Core.<*> (x Core..:? "generalLogGroup")
          Core.<*> (x Core..:? "pending")
