{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.TriggerConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.TriggerConfig
  ( TriggerConfig (..),

    -- * Smart constructor
    mkTriggerConfig,

    -- * Lenses
    tcTriggerName,
    tcTriggerEvents,
    tcTriggerTargetARN,
  )
where

import Network.AWS.CodeDeploy.Types.TriggerEventType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about notification triggers for the deployment group.
--
-- /See:/ 'mkTriggerConfig' smart constructor.
data TriggerConfig = TriggerConfig'
  { triggerName ::
      Lude.Maybe Lude.Text,
    triggerEvents :: Lude.Maybe [TriggerEventType],
    triggerTargetARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TriggerConfig' with the minimum fields required to make a request.
--
-- * 'triggerEvents' - The event type or types for which notifications are triggered.
-- * 'triggerName' - The name of the notification trigger.
-- * 'triggerTargetARN' - The Amazon Resource Name (ARN) of the Amazon Simple Notification Service topic through which notifications about deployment or instance events are sent.
mkTriggerConfig ::
  TriggerConfig
mkTriggerConfig =
  TriggerConfig'
    { triggerName = Lude.Nothing,
      triggerEvents = Lude.Nothing,
      triggerTargetARN = Lude.Nothing
    }

-- | The name of the notification trigger.
--
-- /Note:/ Consider using 'triggerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcTriggerName :: Lens.Lens' TriggerConfig (Lude.Maybe Lude.Text)
tcTriggerName = Lens.lens (triggerName :: TriggerConfig -> Lude.Maybe Lude.Text) (\s a -> s {triggerName = a} :: TriggerConfig)
{-# DEPRECATED tcTriggerName "Use generic-lens or generic-optics with 'triggerName' instead." #-}

-- | The event type or types for which notifications are triggered.
--
-- /Note:/ Consider using 'triggerEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcTriggerEvents :: Lens.Lens' TriggerConfig (Lude.Maybe [TriggerEventType])
tcTriggerEvents = Lens.lens (triggerEvents :: TriggerConfig -> Lude.Maybe [TriggerEventType]) (\s a -> s {triggerEvents = a} :: TriggerConfig)
{-# DEPRECATED tcTriggerEvents "Use generic-lens or generic-optics with 'triggerEvents' instead." #-}

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service topic through which notifications about deployment or instance events are sent.
--
-- /Note:/ Consider using 'triggerTargetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcTriggerTargetARN :: Lens.Lens' TriggerConfig (Lude.Maybe Lude.Text)
tcTriggerTargetARN = Lens.lens (triggerTargetARN :: TriggerConfig -> Lude.Maybe Lude.Text) (\s a -> s {triggerTargetARN = a} :: TriggerConfig)
{-# DEPRECATED tcTriggerTargetARN "Use generic-lens or generic-optics with 'triggerTargetARN' instead." #-}

instance Lude.FromJSON TriggerConfig where
  parseJSON =
    Lude.withObject
      "TriggerConfig"
      ( \x ->
          TriggerConfig'
            Lude.<$> (x Lude..:? "triggerName")
            Lude.<*> (x Lude..:? "triggerEvents" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "triggerTargetArn")
      )

instance Lude.ToJSON TriggerConfig where
  toJSON TriggerConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("triggerName" Lude..=) Lude.<$> triggerName,
            ("triggerEvents" Lude..=) Lude.<$> triggerEvents,
            ("triggerTargetArn" Lude..=) Lude.<$> triggerTargetARN
          ]
      )
