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
-- Module      : Amazonka.Pinpoint.Types.EventsRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.EventsRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.EventsBatch
import qualified Amazonka.Prelude as Prelude

-- | Specifies a batch of events to process.
--
-- /See:/ 'newEventsRequest' smart constructor.
data EventsRequest = EventsRequest'
  { -- | The batch of events to process. For each item in a batch, the endpoint
    -- ID acts as a key that has an EventsBatch object as its value.
    batchItem :: Prelude.HashMap Prelude.Text EventsBatch
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EventsRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchItem', 'eventsRequest_batchItem' - The batch of events to process. For each item in a batch, the endpoint
-- ID acts as a key that has an EventsBatch object as its value.
newEventsRequest ::
  EventsRequest
newEventsRequest =
  EventsRequest' {batchItem = Prelude.mempty}

-- | The batch of events to process. For each item in a batch, the endpoint
-- ID acts as a key that has an EventsBatch object as its value.
eventsRequest_batchItem :: Lens.Lens' EventsRequest (Prelude.HashMap Prelude.Text EventsBatch)
eventsRequest_batchItem = Lens.lens (\EventsRequest' {batchItem} -> batchItem) (\s@EventsRequest' {} a -> s {batchItem = a} :: EventsRequest) Prelude.. Lens.coerced

instance Prelude.Hashable EventsRequest where
  hashWithSalt _salt EventsRequest' {..} =
    _salt `Prelude.hashWithSalt` batchItem

instance Prelude.NFData EventsRequest where
  rnf EventsRequest' {..} = Prelude.rnf batchItem

instance Data.ToJSON EventsRequest where
  toJSON EventsRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("BatchItem" Data..= batchItem)]
      )
