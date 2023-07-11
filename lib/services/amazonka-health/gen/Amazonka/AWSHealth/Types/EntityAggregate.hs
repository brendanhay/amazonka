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
-- Module      : Amazonka.AWSHealth.Types.EntityAggregate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AWSHealth.Types.EntityAggregate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The number of entities that are affected by one or more events. Returned
-- by the
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEntityAggregates.html DescribeEntityAggregates>
-- operation.
--
-- /See:/ 'newEntityAggregate' smart constructor.
data EntityAggregate = EntityAggregate'
  { -- | The number of entities that match the criteria for the specified events.
    count :: Prelude.Maybe Prelude.Int,
    -- | The unique identifier for the event. The event ARN has the
    -- @arn:aws:health:@/@event-region@/@::event\/@/@SERVICE@/@\/@/@EVENT_TYPE_CODE@/@\/@/@EVENT_TYPE_PLUS_ID@/@ @
    -- format.
    --
    -- For example, an event ARN might look like the following:
    --
    -- @arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
    eventArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EntityAggregate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'count', 'entityAggregate_count' - The number of entities that match the criteria for the specified events.
--
-- 'eventArn', 'entityAggregate_eventArn' - The unique identifier for the event. The event ARN has the
-- @arn:aws:health:@/@event-region@/@::event\/@/@SERVICE@/@\/@/@EVENT_TYPE_CODE@/@\/@/@EVENT_TYPE_PLUS_ID@/@ @
-- format.
--
-- For example, an event ARN might look like the following:
--
-- @arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
newEntityAggregate ::
  EntityAggregate
newEntityAggregate =
  EntityAggregate'
    { count = Prelude.Nothing,
      eventArn = Prelude.Nothing
    }

-- | The number of entities that match the criteria for the specified events.
entityAggregate_count :: Lens.Lens' EntityAggregate (Prelude.Maybe Prelude.Int)
entityAggregate_count = Lens.lens (\EntityAggregate' {count} -> count) (\s@EntityAggregate' {} a -> s {count = a} :: EntityAggregate)

-- | The unique identifier for the event. The event ARN has the
-- @arn:aws:health:@/@event-region@/@::event\/@/@SERVICE@/@\/@/@EVENT_TYPE_CODE@/@\/@/@EVENT_TYPE_PLUS_ID@/@ @
-- format.
--
-- For example, an event ARN might look like the following:
--
-- @arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
entityAggregate_eventArn :: Lens.Lens' EntityAggregate (Prelude.Maybe Prelude.Text)
entityAggregate_eventArn = Lens.lens (\EntityAggregate' {eventArn} -> eventArn) (\s@EntityAggregate' {} a -> s {eventArn = a} :: EntityAggregate)

instance Data.FromJSON EntityAggregate where
  parseJSON =
    Data.withObject
      "EntityAggregate"
      ( \x ->
          EntityAggregate'
            Prelude.<$> (x Data..:? "count")
            Prelude.<*> (x Data..:? "eventArn")
      )

instance Prelude.Hashable EntityAggregate where
  hashWithSalt _salt EntityAggregate' {..} =
    _salt
      `Prelude.hashWithSalt` count
      `Prelude.hashWithSalt` eventArn

instance Prelude.NFData EntityAggregate where
  rnf EntityAggregate' {..} =
    Prelude.rnf count
      `Prelude.seq` Prelude.rnf eventArn
