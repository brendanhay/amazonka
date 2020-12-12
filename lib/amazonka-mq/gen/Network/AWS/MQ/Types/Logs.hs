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
import qualified Network.AWS.Prelude as Lude

-- | The list of information about logs to be enabled for the specified broker.
--
-- /See:/ 'mkLogs' smart constructor.
data Logs = Logs'
  { audit :: Lude.Maybe Lude.Bool,
    general :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Logs' with the minimum fields required to make a request.
--
-- * 'audit' - Enables audit logging. Every user management action made using JMX or the ActiveMQ Web Console is logged. Does not apply to RabbitMQ brokers.
-- * 'general' - Enables general logging.
mkLogs ::
  Logs
mkLogs = Logs' {audit = Lude.Nothing, general = Lude.Nothing}

-- | Enables audit logging. Every user management action made using JMX or the ActiveMQ Web Console is logged. Does not apply to RabbitMQ brokers.
--
-- /Note:/ Consider using 'audit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lAudit :: Lens.Lens' Logs (Lude.Maybe Lude.Bool)
lAudit = Lens.lens (audit :: Logs -> Lude.Maybe Lude.Bool) (\s a -> s {audit = a} :: Logs)
{-# DEPRECATED lAudit "Use generic-lens or generic-optics with 'audit' instead." #-}

-- | Enables general logging.
--
-- /Note:/ Consider using 'general' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lGeneral :: Lens.Lens' Logs (Lude.Maybe Lude.Bool)
lGeneral = Lens.lens (general :: Logs -> Lude.Maybe Lude.Bool) (\s a -> s {general = a} :: Logs)
{-# DEPRECATED lGeneral "Use generic-lens or generic-optics with 'general' instead." #-}

instance Lude.FromJSON Logs where
  parseJSON =
    Lude.withObject
      "Logs"
      ( \x ->
          Logs'
            Lude.<$> (x Lude..:? "audit") Lude.<*> (x Lude..:? "general")
      )

instance Lude.ToJSON Logs where
  toJSON Logs' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("audit" Lude..=) Lude.<$> audit,
            ("general" Lude..=) Lude.<$> general
          ]
      )
