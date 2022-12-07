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
-- Module      : Amazonka.AppFlow.Types.EventBridgeDestinationProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.EventBridgeDestinationProperties where

import Amazonka.AppFlow.Types.ErrorHandlingConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The properties that are applied when Amazon EventBridge is being used as
-- a destination.
--
-- /See:/ 'newEventBridgeDestinationProperties' smart constructor.
data EventBridgeDestinationProperties = EventBridgeDestinationProperties'
  { errorHandlingConfig :: Prelude.Maybe ErrorHandlingConfig,
    -- | The object specified in the Amazon EventBridge flow destination.
    object' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EventBridgeDestinationProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorHandlingConfig', 'eventBridgeDestinationProperties_errorHandlingConfig' - Undocumented member.
--
-- 'object'', 'eventBridgeDestinationProperties_object' - The object specified in the Amazon EventBridge flow destination.
newEventBridgeDestinationProperties ::
  -- | 'object''
  Prelude.Text ->
  EventBridgeDestinationProperties
newEventBridgeDestinationProperties pObject_ =
  EventBridgeDestinationProperties'
    { errorHandlingConfig =
        Prelude.Nothing,
      object' = pObject_
    }

-- | Undocumented member.
eventBridgeDestinationProperties_errorHandlingConfig :: Lens.Lens' EventBridgeDestinationProperties (Prelude.Maybe ErrorHandlingConfig)
eventBridgeDestinationProperties_errorHandlingConfig = Lens.lens (\EventBridgeDestinationProperties' {errorHandlingConfig} -> errorHandlingConfig) (\s@EventBridgeDestinationProperties' {} a -> s {errorHandlingConfig = a} :: EventBridgeDestinationProperties)

-- | The object specified in the Amazon EventBridge flow destination.
eventBridgeDestinationProperties_object :: Lens.Lens' EventBridgeDestinationProperties Prelude.Text
eventBridgeDestinationProperties_object = Lens.lens (\EventBridgeDestinationProperties' {object'} -> object') (\s@EventBridgeDestinationProperties' {} a -> s {object' = a} :: EventBridgeDestinationProperties)

instance
  Data.FromJSON
    EventBridgeDestinationProperties
  where
  parseJSON =
    Data.withObject
      "EventBridgeDestinationProperties"
      ( \x ->
          EventBridgeDestinationProperties'
            Prelude.<$> (x Data..:? "errorHandlingConfig")
            Prelude.<*> (x Data..: "object")
      )

instance
  Prelude.Hashable
    EventBridgeDestinationProperties
  where
  hashWithSalt
    _salt
    EventBridgeDestinationProperties' {..} =
      _salt `Prelude.hashWithSalt` errorHandlingConfig
        `Prelude.hashWithSalt` object'

instance
  Prelude.NFData
    EventBridgeDestinationProperties
  where
  rnf EventBridgeDestinationProperties' {..} =
    Prelude.rnf errorHandlingConfig
      `Prelude.seq` Prelude.rnf object'

instance Data.ToJSON EventBridgeDestinationProperties where
  toJSON EventBridgeDestinationProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("errorHandlingConfig" Data..=)
              Prelude.<$> errorHandlingConfig,
            Prelude.Just ("object" Data..= object')
          ]
      )
