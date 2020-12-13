{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.VaultNotificationConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.VaultNotificationConfig
  ( VaultNotificationConfig (..),

    -- * Smart constructor
    mkVaultNotificationConfig,

    -- * Lenses
    vncSNSTopic,
    vncEvents,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a vault's notification configuration.
--
-- /See:/ 'mkVaultNotificationConfig' smart constructor.
data VaultNotificationConfig = VaultNotificationConfig'
  { -- | The Amazon Simple Notification Service (Amazon SNS) topic Amazon Resource Name (ARN).
    snsTopic :: Lude.Maybe Lude.Text,
    -- | A list of one or more events for which Amazon S3 Glacier will send a notification to the specified Amazon SNS topic.
    events :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VaultNotificationConfig' with the minimum fields required to make a request.
--
-- * 'snsTopic' - The Amazon Simple Notification Service (Amazon SNS) topic Amazon Resource Name (ARN).
-- * 'events' - A list of one or more events for which Amazon S3 Glacier will send a notification to the specified Amazon SNS topic.
mkVaultNotificationConfig ::
  VaultNotificationConfig
mkVaultNotificationConfig =
  VaultNotificationConfig'
    { snsTopic = Lude.Nothing,
      events = Lude.Nothing
    }

-- | The Amazon Simple Notification Service (Amazon SNS) topic Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'snsTopic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vncSNSTopic :: Lens.Lens' VaultNotificationConfig (Lude.Maybe Lude.Text)
vncSNSTopic = Lens.lens (snsTopic :: VaultNotificationConfig -> Lude.Maybe Lude.Text) (\s a -> s {snsTopic = a} :: VaultNotificationConfig)
{-# DEPRECATED vncSNSTopic "Use generic-lens or generic-optics with 'snsTopic' instead." #-}

-- | A list of one or more events for which Amazon S3 Glacier will send a notification to the specified Amazon SNS topic.
--
-- /Note:/ Consider using 'events' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vncEvents :: Lens.Lens' VaultNotificationConfig (Lude.Maybe [Lude.Text])
vncEvents = Lens.lens (events :: VaultNotificationConfig -> Lude.Maybe [Lude.Text]) (\s a -> s {events = a} :: VaultNotificationConfig)
{-# DEPRECATED vncEvents "Use generic-lens or generic-optics with 'events' instead." #-}

instance Lude.FromJSON VaultNotificationConfig where
  parseJSON =
    Lude.withObject
      "VaultNotificationConfig"
      ( \x ->
          VaultNotificationConfig'
            Lude.<$> (x Lude..:? "SNSTopic")
            Lude.<*> (x Lude..:? "Events" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON VaultNotificationConfig where
  toJSON VaultNotificationConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SNSTopic" Lude..=) Lude.<$> snsTopic,
            ("Events" Lude..=) Lude.<$> events
          ]
      )
