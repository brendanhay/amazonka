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
    lsPending,
    lsAudit,
    lsGeneral,
    lsGeneralLogGroup,
    lsAuditLogGroup,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types.PendingLogs
import qualified Network.AWS.Prelude as Lude

-- | The list of information about logs currently enabled and pending to be deployed for the specified broker.
--
-- /See:/ 'mkLogsSummary' smart constructor.
data LogsSummary = LogsSummary'
  { pending :: Lude.Maybe PendingLogs,
    audit :: Lude.Maybe Lude.Bool,
    general :: Lude.Maybe Lude.Bool,
    generalLogGroup :: Lude.Maybe Lude.Text,
    auditLogGroup :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LogsSummary' with the minimum fields required to make a request.
--
-- * 'audit' - Enables audit logging. Every user management action made using JMX or the ActiveMQ Web Console is logged.
-- * 'auditLogGroup' - The location of the CloudWatch Logs log group where audit logs are sent.
-- * 'general' - Enables general logging.
-- * 'generalLogGroup' - The location of the CloudWatch Logs log group where general logs are sent.
-- * 'pending' - The list of information about logs pending to be deployed for the specified broker.
mkLogsSummary ::
  LogsSummary
mkLogsSummary =
  LogsSummary'
    { pending = Lude.Nothing,
      audit = Lude.Nothing,
      general = Lude.Nothing,
      generalLogGroup = Lude.Nothing,
      auditLogGroup = Lude.Nothing
    }

-- | The list of information about logs pending to be deployed for the specified broker.
--
-- /Note:/ Consider using 'pending' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsPending :: Lens.Lens' LogsSummary (Lude.Maybe PendingLogs)
lsPending = Lens.lens (pending :: LogsSummary -> Lude.Maybe PendingLogs) (\s a -> s {pending = a} :: LogsSummary)
{-# DEPRECATED lsPending "Use generic-lens or generic-optics with 'pending' instead." #-}

-- | Enables audit logging. Every user management action made using JMX or the ActiveMQ Web Console is logged.
--
-- /Note:/ Consider using 'audit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsAudit :: Lens.Lens' LogsSummary (Lude.Maybe Lude.Bool)
lsAudit = Lens.lens (audit :: LogsSummary -> Lude.Maybe Lude.Bool) (\s a -> s {audit = a} :: LogsSummary)
{-# DEPRECATED lsAudit "Use generic-lens or generic-optics with 'audit' instead." #-}

-- | Enables general logging.
--
-- /Note:/ Consider using 'general' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsGeneral :: Lens.Lens' LogsSummary (Lude.Maybe Lude.Bool)
lsGeneral = Lens.lens (general :: LogsSummary -> Lude.Maybe Lude.Bool) (\s a -> s {general = a} :: LogsSummary)
{-# DEPRECATED lsGeneral "Use generic-lens or generic-optics with 'general' instead." #-}

-- | The location of the CloudWatch Logs log group where general logs are sent.
--
-- /Note:/ Consider using 'generalLogGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsGeneralLogGroup :: Lens.Lens' LogsSummary (Lude.Maybe Lude.Text)
lsGeneralLogGroup = Lens.lens (generalLogGroup :: LogsSummary -> Lude.Maybe Lude.Text) (\s a -> s {generalLogGroup = a} :: LogsSummary)
{-# DEPRECATED lsGeneralLogGroup "Use generic-lens or generic-optics with 'generalLogGroup' instead." #-}

-- | The location of the CloudWatch Logs log group where audit logs are sent.
--
-- /Note:/ Consider using 'auditLogGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsAuditLogGroup :: Lens.Lens' LogsSummary (Lude.Maybe Lude.Text)
lsAuditLogGroup = Lens.lens (auditLogGroup :: LogsSummary -> Lude.Maybe Lude.Text) (\s a -> s {auditLogGroup = a} :: LogsSummary)
{-# DEPRECATED lsAuditLogGroup "Use generic-lens or generic-optics with 'auditLogGroup' instead." #-}

instance Lude.FromJSON LogsSummary where
  parseJSON =
    Lude.withObject
      "LogsSummary"
      ( \x ->
          LogsSummary'
            Lude.<$> (x Lude..:? "pending")
            Lude.<*> (x Lude..:? "audit")
            Lude.<*> (x Lude..:? "general")
            Lude.<*> (x Lude..:? "generalLogGroup")
            Lude.<*> (x Lude..:? "auditLogGroup")
      )
