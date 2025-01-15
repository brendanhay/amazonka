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
-- Module      : Amazonka.CodeDeploy.Types.TriggerConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.TriggerConfig where

import Amazonka.CodeDeploy.Types.TriggerEventType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
triggerConfig_triggerEvents = Lens.lens (\TriggerConfig' {triggerEvents} -> triggerEvents) (\s@TriggerConfig' {} a -> s {triggerEvents = a} :: TriggerConfig) Prelude.. Lens.mapping Lens.coerced

-- | The name of the notification trigger.
triggerConfig_triggerName :: Lens.Lens' TriggerConfig (Prelude.Maybe Prelude.Text)
triggerConfig_triggerName = Lens.lens (\TriggerConfig' {triggerName} -> triggerName) (\s@TriggerConfig' {} a -> s {triggerName = a} :: TriggerConfig)

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
-- topic through which notifications about deployment or instance events
-- are sent.
triggerConfig_triggerTargetArn :: Lens.Lens' TriggerConfig (Prelude.Maybe Prelude.Text)
triggerConfig_triggerTargetArn = Lens.lens (\TriggerConfig' {triggerTargetArn} -> triggerTargetArn) (\s@TriggerConfig' {} a -> s {triggerTargetArn = a} :: TriggerConfig)

instance Data.FromJSON TriggerConfig where
  parseJSON =
    Data.withObject
      "TriggerConfig"
      ( \x ->
          TriggerConfig'
            Prelude.<$> (x Data..:? "triggerEvents" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "triggerName")
            Prelude.<*> (x Data..:? "triggerTargetArn")
      )

instance Prelude.Hashable TriggerConfig where
  hashWithSalt _salt TriggerConfig' {..} =
    _salt
      `Prelude.hashWithSalt` triggerEvents
      `Prelude.hashWithSalt` triggerName
      `Prelude.hashWithSalt` triggerTargetArn

instance Prelude.NFData TriggerConfig where
  rnf TriggerConfig' {..} =
    Prelude.rnf triggerEvents `Prelude.seq`
      Prelude.rnf triggerName `Prelude.seq`
        Prelude.rnf triggerTargetArn

instance Data.ToJSON TriggerConfig where
  toJSON TriggerConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("triggerEvents" Data..=) Prelude.<$> triggerEvents,
            ("triggerName" Data..=) Prelude.<$> triggerName,
            ("triggerTargetArn" Data..=)
              Prelude.<$> triggerTargetArn
          ]
      )
