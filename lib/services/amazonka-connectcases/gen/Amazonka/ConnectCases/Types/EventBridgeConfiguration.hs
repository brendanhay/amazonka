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
-- Module      : Amazonka.ConnectCases.Types.EventBridgeConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCases.Types.EventBridgeConfiguration where

import Amazonka.ConnectCases.Types.EventIncludedData
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configuration to enable EventBridge case event delivery and determine
-- what data is delivered.
--
-- /See:/ 'newEventBridgeConfiguration' smart constructor.
data EventBridgeConfiguration = EventBridgeConfiguration'
  { -- | Details of what case and related item data is published through the case
    -- event stream.
    includedData :: Prelude.Maybe EventIncludedData,
    -- | Indicates whether the to broadcast case event data to the customer.
    enabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EventBridgeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includedData', 'eventBridgeConfiguration_includedData' - Details of what case and related item data is published through the case
-- event stream.
--
-- 'enabled', 'eventBridgeConfiguration_enabled' - Indicates whether the to broadcast case event data to the customer.
newEventBridgeConfiguration ::
  -- | 'enabled'
  Prelude.Bool ->
  EventBridgeConfiguration
newEventBridgeConfiguration pEnabled_ =
  EventBridgeConfiguration'
    { includedData =
        Prelude.Nothing,
      enabled = pEnabled_
    }

-- | Details of what case and related item data is published through the case
-- event stream.
eventBridgeConfiguration_includedData :: Lens.Lens' EventBridgeConfiguration (Prelude.Maybe EventIncludedData)
eventBridgeConfiguration_includedData = Lens.lens (\EventBridgeConfiguration' {includedData} -> includedData) (\s@EventBridgeConfiguration' {} a -> s {includedData = a} :: EventBridgeConfiguration)

-- | Indicates whether the to broadcast case event data to the customer.
eventBridgeConfiguration_enabled :: Lens.Lens' EventBridgeConfiguration Prelude.Bool
eventBridgeConfiguration_enabled = Lens.lens (\EventBridgeConfiguration' {enabled} -> enabled) (\s@EventBridgeConfiguration' {} a -> s {enabled = a} :: EventBridgeConfiguration)

instance Data.FromJSON EventBridgeConfiguration where
  parseJSON =
    Data.withObject
      "EventBridgeConfiguration"
      ( \x ->
          EventBridgeConfiguration'
            Prelude.<$> (x Data..:? "includedData")
            Prelude.<*> (x Data..: "enabled")
      )

instance Prelude.Hashable EventBridgeConfiguration where
  hashWithSalt _salt EventBridgeConfiguration' {..} =
    _salt `Prelude.hashWithSalt` includedData
      `Prelude.hashWithSalt` enabled

instance Prelude.NFData EventBridgeConfiguration where
  rnf EventBridgeConfiguration' {..} =
    Prelude.rnf includedData
      `Prelude.seq` Prelude.rnf enabled

instance Data.ToJSON EventBridgeConfiguration where
  toJSON EventBridgeConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("includedData" Data..=) Prelude.<$> includedData,
            Prelude.Just ("enabled" Data..= enabled)
          ]
      )
