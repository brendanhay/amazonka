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
-- Module      : Amazonka.Glue.Types.EventBatchingCondition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.EventBatchingCondition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Batch condition that must be met (specified number of events received or
-- batch time window expired) before EventBridge event trigger fires.
--
-- /See:/ 'newEventBatchingCondition' smart constructor.
data EventBatchingCondition = EventBatchingCondition'
  { -- | Window of time in seconds after which EventBridge event trigger fires.
    -- Window starts when first event is received.
    batchWindow :: Prelude.Maybe Prelude.Natural,
    -- | Number of events that must be received from Amazon EventBridge before
    -- EventBridge event trigger fires.
    batchSize :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EventBatchingCondition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchWindow', 'eventBatchingCondition_batchWindow' - Window of time in seconds after which EventBridge event trigger fires.
-- Window starts when first event is received.
--
-- 'batchSize', 'eventBatchingCondition_batchSize' - Number of events that must be received from Amazon EventBridge before
-- EventBridge event trigger fires.
newEventBatchingCondition ::
  -- | 'batchSize'
  Prelude.Natural ->
  EventBatchingCondition
newEventBatchingCondition pBatchSize_ =
  EventBatchingCondition'
    { batchWindow =
        Prelude.Nothing,
      batchSize = pBatchSize_
    }

-- | Window of time in seconds after which EventBridge event trigger fires.
-- Window starts when first event is received.
eventBatchingCondition_batchWindow :: Lens.Lens' EventBatchingCondition (Prelude.Maybe Prelude.Natural)
eventBatchingCondition_batchWindow = Lens.lens (\EventBatchingCondition' {batchWindow} -> batchWindow) (\s@EventBatchingCondition' {} a -> s {batchWindow = a} :: EventBatchingCondition)

-- | Number of events that must be received from Amazon EventBridge before
-- EventBridge event trigger fires.
eventBatchingCondition_batchSize :: Lens.Lens' EventBatchingCondition Prelude.Natural
eventBatchingCondition_batchSize = Lens.lens (\EventBatchingCondition' {batchSize} -> batchSize) (\s@EventBatchingCondition' {} a -> s {batchSize = a} :: EventBatchingCondition)

instance Data.FromJSON EventBatchingCondition where
  parseJSON =
    Data.withObject
      "EventBatchingCondition"
      ( \x ->
          EventBatchingCondition'
            Prelude.<$> (x Data..:? "BatchWindow")
            Prelude.<*> (x Data..: "BatchSize")
      )

instance Prelude.Hashable EventBatchingCondition where
  hashWithSalt _salt EventBatchingCondition' {..} =
    _salt
      `Prelude.hashWithSalt` batchWindow
      `Prelude.hashWithSalt` batchSize

instance Prelude.NFData EventBatchingCondition where
  rnf EventBatchingCondition' {..} =
    Prelude.rnf batchWindow `Prelude.seq`
      Prelude.rnf batchSize

instance Data.ToJSON EventBatchingCondition where
  toJSON EventBatchingCondition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BatchWindow" Data..=) Prelude.<$> batchWindow,
            Prelude.Just ("BatchSize" Data..= batchSize)
          ]
      )
