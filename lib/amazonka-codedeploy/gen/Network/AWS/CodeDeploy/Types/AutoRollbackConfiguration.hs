{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.AutoRollbackConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.AutoRollbackConfiguration
  ( AutoRollbackConfiguration (..),

    -- * Smart constructor
    mkAutoRollbackConfiguration,

    -- * Lenses
    arcEnabled,
    arcEvents,
  )
where

import Network.AWS.CodeDeploy.Types.AutoRollbackEvent
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a configuration for automatically rolling back to a previous version of an application revision when a deployment is not completed successfully.
--
-- /See:/ 'mkAutoRollbackConfiguration' smart constructor.
data AutoRollbackConfiguration = AutoRollbackConfiguration'
  { enabled ::
      Lude.Maybe Lude.Bool,
    events ::
      Lude.Maybe [AutoRollbackEvent]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AutoRollbackConfiguration' with the minimum fields required to make a request.
--
-- * 'enabled' - Indicates whether a defined automatic rollback configuration is currently enabled.
-- * 'events' - The event type or types that trigger a rollback.
mkAutoRollbackConfiguration ::
  AutoRollbackConfiguration
mkAutoRollbackConfiguration =
  AutoRollbackConfiguration'
    { enabled = Lude.Nothing,
      events = Lude.Nothing
    }

-- | Indicates whether a defined automatic rollback configuration is currently enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arcEnabled :: Lens.Lens' AutoRollbackConfiguration (Lude.Maybe Lude.Bool)
arcEnabled = Lens.lens (enabled :: AutoRollbackConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: AutoRollbackConfiguration)
{-# DEPRECATED arcEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The event type or types that trigger a rollback.
--
-- /Note:/ Consider using 'events' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arcEvents :: Lens.Lens' AutoRollbackConfiguration (Lude.Maybe [AutoRollbackEvent])
arcEvents = Lens.lens (events :: AutoRollbackConfiguration -> Lude.Maybe [AutoRollbackEvent]) (\s a -> s {events = a} :: AutoRollbackConfiguration)
{-# DEPRECATED arcEvents "Use generic-lens or generic-optics with 'events' instead." #-}

instance Lude.FromJSON AutoRollbackConfiguration where
  parseJSON =
    Lude.withObject
      "AutoRollbackConfiguration"
      ( \x ->
          AutoRollbackConfiguration'
            Lude.<$> (x Lude..:? "enabled")
            Lude.<*> (x Lude..:? "events" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON AutoRollbackConfiguration where
  toJSON AutoRollbackConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("enabled" Lude..=) Lude.<$> enabled,
            ("events" Lude..=) Lude.<$> events
          ]
      )
