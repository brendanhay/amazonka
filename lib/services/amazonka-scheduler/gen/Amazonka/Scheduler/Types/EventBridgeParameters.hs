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
-- Module      : Amazonka.Scheduler.Types.EventBridgeParameters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Scheduler.Types.EventBridgeParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The templated target type for the EventBridge
-- <https://docs.aws.amazon.com/eventbridge/latest/APIReference/API_PutEvents.html PutEvents>
-- API operation.
--
-- /See:/ 'newEventBridgeParameters' smart constructor.
data EventBridgeParameters = EventBridgeParameters'
  { -- | A free-form string, with a maximum of 128 characters, used to decide
    -- what fields to expect in the event detail.
    detailType :: Prelude.Text,
    -- | The source of the event.
    source :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EventBridgeParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detailType', 'eventBridgeParameters_detailType' - A free-form string, with a maximum of 128 characters, used to decide
-- what fields to expect in the event detail.
--
-- 'source', 'eventBridgeParameters_source' - The source of the event.
newEventBridgeParameters ::
  -- | 'detailType'
  Prelude.Text ->
  -- | 'source'
  Prelude.Text ->
  EventBridgeParameters
newEventBridgeParameters pDetailType_ pSource_ =
  EventBridgeParameters'
    { detailType = pDetailType_,
      source = pSource_
    }

-- | A free-form string, with a maximum of 128 characters, used to decide
-- what fields to expect in the event detail.
eventBridgeParameters_detailType :: Lens.Lens' EventBridgeParameters Prelude.Text
eventBridgeParameters_detailType = Lens.lens (\EventBridgeParameters' {detailType} -> detailType) (\s@EventBridgeParameters' {} a -> s {detailType = a} :: EventBridgeParameters)

-- | The source of the event.
eventBridgeParameters_source :: Lens.Lens' EventBridgeParameters Prelude.Text
eventBridgeParameters_source = Lens.lens (\EventBridgeParameters' {source} -> source) (\s@EventBridgeParameters' {} a -> s {source = a} :: EventBridgeParameters)

instance Core.FromJSON EventBridgeParameters where
  parseJSON =
    Core.withObject
      "EventBridgeParameters"
      ( \x ->
          EventBridgeParameters'
            Prelude.<$> (x Core..: "DetailType")
            Prelude.<*> (x Core..: "Source")
      )

instance Prelude.Hashable EventBridgeParameters where
  hashWithSalt _salt EventBridgeParameters' {..} =
    _salt `Prelude.hashWithSalt` detailType
      `Prelude.hashWithSalt` source

instance Prelude.NFData EventBridgeParameters where
  rnf EventBridgeParameters' {..} =
    Prelude.rnf detailType
      `Prelude.seq` Prelude.rnf source

instance Core.ToJSON EventBridgeParameters where
  toJSON EventBridgeParameters' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DetailType" Core..= detailType),
            Prelude.Just ("Source" Core..= source)
          ]
      )
