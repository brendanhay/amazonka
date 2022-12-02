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
-- Module      : Amazonka.AWSHealth.Types.EventAggregate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AWSHealth.Types.EventAggregate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The number of events of each issue type. Returned by the
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventAggregates.html DescribeEventAggregates>
-- operation.
--
-- /See:/ 'newEventAggregate' smart constructor.
data EventAggregate = EventAggregate'
  { -- | The issue type for the associated count.
    aggregateValue :: Prelude.Maybe Prelude.Text,
    -- | The number of events of the associated issue type.
    count :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EventAggregate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aggregateValue', 'eventAggregate_aggregateValue' - The issue type for the associated count.
--
-- 'count', 'eventAggregate_count' - The number of events of the associated issue type.
newEventAggregate ::
  EventAggregate
newEventAggregate =
  EventAggregate'
    { aggregateValue = Prelude.Nothing,
      count = Prelude.Nothing
    }

-- | The issue type for the associated count.
eventAggregate_aggregateValue :: Lens.Lens' EventAggregate (Prelude.Maybe Prelude.Text)
eventAggregate_aggregateValue = Lens.lens (\EventAggregate' {aggregateValue} -> aggregateValue) (\s@EventAggregate' {} a -> s {aggregateValue = a} :: EventAggregate)

-- | The number of events of the associated issue type.
eventAggregate_count :: Lens.Lens' EventAggregate (Prelude.Maybe Prelude.Int)
eventAggregate_count = Lens.lens (\EventAggregate' {count} -> count) (\s@EventAggregate' {} a -> s {count = a} :: EventAggregate)

instance Data.FromJSON EventAggregate where
  parseJSON =
    Data.withObject
      "EventAggregate"
      ( \x ->
          EventAggregate'
            Prelude.<$> (x Data..:? "aggregateValue")
            Prelude.<*> (x Data..:? "count")
      )

instance Prelude.Hashable EventAggregate where
  hashWithSalt _salt EventAggregate' {..} =
    _salt `Prelude.hashWithSalt` aggregateValue
      `Prelude.hashWithSalt` count

instance Prelude.NFData EventAggregate where
  rnf EventAggregate' {..} =
    Prelude.rnf aggregateValue
      `Prelude.seq` Prelude.rnf count
