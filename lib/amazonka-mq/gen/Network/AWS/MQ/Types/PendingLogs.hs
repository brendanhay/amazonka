{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.PendingLogs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.PendingLogs
  ( PendingLogs (..),

    -- * Smart constructor
    mkPendingLogs,

    -- * Lenses
    plAudit,
    plGeneral,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The list of information about logs to be enabled for the specified broker.
--
-- /See:/ 'mkPendingLogs' smart constructor.
data PendingLogs = PendingLogs'
  { -- | Enables audit logging. Every user management action made using JMX or the ActiveMQ Web Console is logged.
    audit :: Lude.Maybe Lude.Bool,
    -- | Enables general logging.
    general :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PendingLogs' with the minimum fields required to make a request.
--
-- * 'audit' - Enables audit logging. Every user management action made using JMX or the ActiveMQ Web Console is logged.
-- * 'general' - Enables general logging.
mkPendingLogs ::
  PendingLogs
mkPendingLogs =
  PendingLogs' {audit = Lude.Nothing, general = Lude.Nothing}

-- | Enables audit logging. Every user management action made using JMX or the ActiveMQ Web Console is logged.
--
-- /Note:/ Consider using 'audit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plAudit :: Lens.Lens' PendingLogs (Lude.Maybe Lude.Bool)
plAudit = Lens.lens (audit :: PendingLogs -> Lude.Maybe Lude.Bool) (\s a -> s {audit = a} :: PendingLogs)
{-# DEPRECATED plAudit "Use generic-lens or generic-optics with 'audit' instead." #-}

-- | Enables general logging.
--
-- /Note:/ Consider using 'general' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plGeneral :: Lens.Lens' PendingLogs (Lude.Maybe Lude.Bool)
plGeneral = Lens.lens (general :: PendingLogs -> Lude.Maybe Lude.Bool) (\s a -> s {general = a} :: PendingLogs)
{-# DEPRECATED plGeneral "Use generic-lens or generic-optics with 'general' instead." #-}

instance Lude.FromJSON PendingLogs where
  parseJSON =
    Lude.withObject
      "PendingLogs"
      ( \x ->
          PendingLogs'
            Lude.<$> (x Lude..:? "audit") Lude.<*> (x Lude..:? "general")
      )
