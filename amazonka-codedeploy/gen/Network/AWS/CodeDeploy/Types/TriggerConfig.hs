{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about notification triggers for the deployment group.
--
-- /See:/ 'newTriggerConfig' smart constructor.
data TriggerConfig = TriggerConfig'
  { -- | The event type or types for which notifications are triggered.
    triggerEvents :: Prelude.Maybe [TriggerEventType],
    -- | The name of the notification trigger.
    triggerName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
    -- topic through which notifications about deployment or instance events
    -- are sent.
    triggerTargetArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { triggerEvents = Prelude.Nothing,
      triggerName = Prelude.Nothing,
      triggerTargetArn = Prelude.Nothing
    }

-- | The event type or types for which notifications are triggered.
triggerConfig_triggerEvents :: Lens.Lens' TriggerConfig (Prelude.Maybe [TriggerEventType])
triggerConfig_triggerEvents = Lens.lens (\TriggerConfig' {triggerEvents} -> triggerEvents) (\s@TriggerConfig' {} a -> s {triggerEvents = a} :: TriggerConfig) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the notification trigger.
triggerConfig_triggerName :: Lens.Lens' TriggerConfig (Prelude.Maybe Prelude.Text)
triggerConfig_triggerName = Lens.lens (\TriggerConfig' {triggerName} -> triggerName) (\s@TriggerConfig' {} a -> s {triggerName = a} :: TriggerConfig)

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
-- topic through which notifications about deployment or instance events
-- are sent.
triggerConfig_triggerTargetArn :: Lens.Lens' TriggerConfig (Prelude.Maybe Prelude.Text)
triggerConfig_triggerTargetArn = Lens.lens (\TriggerConfig' {triggerTargetArn} -> triggerTargetArn) (\s@TriggerConfig' {} a -> s {triggerTargetArn = a} :: TriggerConfig)

instance Prelude.FromJSON TriggerConfig where
  parseJSON =
    Prelude.withObject
      "TriggerConfig"
      ( \x ->
          TriggerConfig'
            Prelude.<$> ( x Prelude..:? "triggerEvents"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "triggerName")
            Prelude.<*> (x Prelude..:? "triggerTargetArn")
      )

instance Prelude.Hashable TriggerConfig

instance Prelude.NFData TriggerConfig

instance Prelude.ToJSON TriggerConfig where
  toJSON TriggerConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("triggerEvents" Prelude..=)
              Prelude.<$> triggerEvents,
            ("triggerName" Prelude..=) Prelude.<$> triggerName,
            ("triggerTargetArn" Prelude..=)
              Prelude.<$> triggerTargetArn
          ]
      )
