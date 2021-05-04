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
-- Module      : Network.AWS.AWSHealth.Types.EventAggregate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.EventAggregate where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The number of events of each issue type. Returned by the
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventAggregates.html DescribeEventAggregates>
-- operation.
--
-- /See:/ 'newEventAggregate' smart constructor.
data EventAggregate = EventAggregate'
  { -- | The number of events of the associated issue type.
    count :: Prelude.Maybe Prelude.Int,
    -- | The issue type for the associated count.
    aggregateValue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EventAggregate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'count', 'eventAggregate_count' - The number of events of the associated issue type.
--
-- 'aggregateValue', 'eventAggregate_aggregateValue' - The issue type for the associated count.
newEventAggregate ::
  EventAggregate
newEventAggregate =
  EventAggregate'
    { count = Prelude.Nothing,
      aggregateValue = Prelude.Nothing
    }

-- | The number of events of the associated issue type.
eventAggregate_count :: Lens.Lens' EventAggregate (Prelude.Maybe Prelude.Int)
eventAggregate_count = Lens.lens (\EventAggregate' {count} -> count) (\s@EventAggregate' {} a -> s {count = a} :: EventAggregate)

-- | The issue type for the associated count.
eventAggregate_aggregateValue :: Lens.Lens' EventAggregate (Prelude.Maybe Prelude.Text)
eventAggregate_aggregateValue = Lens.lens (\EventAggregate' {aggregateValue} -> aggregateValue) (\s@EventAggregate' {} a -> s {aggregateValue = a} :: EventAggregate)

instance Prelude.FromJSON EventAggregate where
  parseJSON =
    Prelude.withObject
      "EventAggregate"
      ( \x ->
          EventAggregate'
            Prelude.<$> (x Prelude..:? "count")
            Prelude.<*> (x Prelude..:? "aggregateValue")
      )

instance Prelude.Hashable EventAggregate

instance Prelude.NFData EventAggregate
