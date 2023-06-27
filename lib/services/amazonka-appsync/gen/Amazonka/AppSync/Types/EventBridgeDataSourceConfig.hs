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
-- Module      : Amazonka.AppSync.Types.EventBridgeDataSourceConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppSync.Types.EventBridgeDataSourceConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an Amazon EventBridge bus data source configuration.
--
-- /See:/ 'newEventBridgeDataSourceConfig' smart constructor.
data EventBridgeDataSourceConfig = EventBridgeDataSourceConfig'
  { -- | The ARN of the event bus. For more information about event buses, see
    -- <https://docs.aws.amazon.com/eventbridge/latest/userguide/eb-event-bus.html Amazon EventBridge event buses>.
    eventBusArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EventBridgeDataSourceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventBusArn', 'eventBridgeDataSourceConfig_eventBusArn' - The ARN of the event bus. For more information about event buses, see
-- <https://docs.aws.amazon.com/eventbridge/latest/userguide/eb-event-bus.html Amazon EventBridge event buses>.
newEventBridgeDataSourceConfig ::
  -- | 'eventBusArn'
  Prelude.Text ->
  EventBridgeDataSourceConfig
newEventBridgeDataSourceConfig pEventBusArn_ =
  EventBridgeDataSourceConfig'
    { eventBusArn =
        pEventBusArn_
    }

-- | The ARN of the event bus. For more information about event buses, see
-- <https://docs.aws.amazon.com/eventbridge/latest/userguide/eb-event-bus.html Amazon EventBridge event buses>.
eventBridgeDataSourceConfig_eventBusArn :: Lens.Lens' EventBridgeDataSourceConfig Prelude.Text
eventBridgeDataSourceConfig_eventBusArn = Lens.lens (\EventBridgeDataSourceConfig' {eventBusArn} -> eventBusArn) (\s@EventBridgeDataSourceConfig' {} a -> s {eventBusArn = a} :: EventBridgeDataSourceConfig)

instance Data.FromJSON EventBridgeDataSourceConfig where
  parseJSON =
    Data.withObject
      "EventBridgeDataSourceConfig"
      ( \x ->
          EventBridgeDataSourceConfig'
            Prelude.<$> (x Data..: "eventBusArn")
      )

instance Prelude.Hashable EventBridgeDataSourceConfig where
  hashWithSalt _salt EventBridgeDataSourceConfig' {..} =
    _salt `Prelude.hashWithSalt` eventBusArn

instance Prelude.NFData EventBridgeDataSourceConfig where
  rnf EventBridgeDataSourceConfig' {..} =
    Prelude.rnf eventBusArn

instance Data.ToJSON EventBridgeDataSourceConfig where
  toJSON EventBridgeDataSourceConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("eventBusArn" Data..= eventBusArn)]
      )
