{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.TriggerConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.TriggerConfig where

import Network.AWS.CodeDeploy.Types.TriggerEventType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about notification triggers for the deployment group.
--
-- /See:/ 'newTriggerConfig' smart constructor.
data TriggerConfig = TriggerConfig'
  { -- | The event type or types for which notifications are triggered.
    triggerEvents :: Core.Maybe [TriggerEventType],
    -- | The name of the notification trigger.
    triggerName :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
    -- topic through which notifications about deployment or instance events
    -- are sent.
    triggerTargetArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TriggerConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'triggerEvents', 'triggerConfig_triggerEvents' - The event type or types for which notifications are triggered.
--
-- 'triggerName', 'triggerConfig_triggerName' - The name of the notification trigger.
--
-- 'triggerTargetArn', 'triggerConfig_triggerTargetArn' - The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
-- topic through which notifications about deployment or instance events
-- are sent.
newTriggerConfig ::
  TriggerConfig
newTriggerConfig =
  TriggerConfig'
    { triggerEvents = Core.Nothing,
      triggerName = Core.Nothing,
      triggerTargetArn = Core.Nothing
    }

-- | The event type or types for which notifications are triggered.
triggerConfig_triggerEvents :: Lens.Lens' TriggerConfig (Core.Maybe [TriggerEventType])
triggerConfig_triggerEvents = Lens.lens (\TriggerConfig' {triggerEvents} -> triggerEvents) (\s@TriggerConfig' {} a -> s {triggerEvents = a} :: TriggerConfig) Core.. Lens.mapping Lens._Coerce

-- | The name of the notification trigger.
triggerConfig_triggerName :: Lens.Lens' TriggerConfig (Core.Maybe Core.Text)
triggerConfig_triggerName = Lens.lens (\TriggerConfig' {triggerName} -> triggerName) (\s@TriggerConfig' {} a -> s {triggerName = a} :: TriggerConfig)

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
-- topic through which notifications about deployment or instance events
-- are sent.
triggerConfig_triggerTargetArn :: Lens.Lens' TriggerConfig (Core.Maybe Core.Text)
triggerConfig_triggerTargetArn = Lens.lens (\TriggerConfig' {triggerTargetArn} -> triggerTargetArn) (\s@TriggerConfig' {} a -> s {triggerTargetArn = a} :: TriggerConfig)

instance Core.FromJSON TriggerConfig where
  parseJSON =
    Core.withObject
      "TriggerConfig"
      ( \x ->
          TriggerConfig'
            Core.<$> (x Core..:? "triggerEvents" Core..!= Core.mempty)
            Core.<*> (x Core..:? "triggerName")
            Core.<*> (x Core..:? "triggerTargetArn")
      )

instance Core.Hashable TriggerConfig

instance Core.NFData TriggerConfig

instance Core.ToJSON TriggerConfig where
  toJSON TriggerConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("triggerEvents" Core..=) Core.<$> triggerEvents,
            ("triggerName" Core..=) Core.<$> triggerName,
            ("triggerTargetArn" Core..=)
              Core.<$> triggerTargetArn
          ]
      )
