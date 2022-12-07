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
-- Module      : Amazonka.Connect.Types.RoutingProfileQueueConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.RoutingProfileQueueConfig where

import Amazonka.Connect.Types.RoutingProfileQueueReference
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the queue and channel for which priority and
-- delay can be set.
--
-- /See:/ 'newRoutingProfileQueueConfig' smart constructor.
data RoutingProfileQueueConfig = RoutingProfileQueueConfig'
  { -- | Contains information about a queue resource.
    queueReference :: RoutingProfileQueueReference,
    -- | The order in which contacts are to be handled for the queue. For more
    -- information, see
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-routing-profiles-priority.html Queues: priority and delay>.
    priority :: Prelude.Natural,
    -- | The delay, in seconds, a contact should be in the queue before they are
    -- routed to an available agent. For more information, see
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-routing-profiles-priority.html Queues: priority and delay>
    -- in the /Amazon Connect Administrator Guide/.
    delay :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RoutingProfileQueueConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queueReference', 'routingProfileQueueConfig_queueReference' - Contains information about a queue resource.
--
-- 'priority', 'routingProfileQueueConfig_priority' - The order in which contacts are to be handled for the queue. For more
-- information, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-routing-profiles-priority.html Queues: priority and delay>.
--
-- 'delay', 'routingProfileQueueConfig_delay' - The delay, in seconds, a contact should be in the queue before they are
-- routed to an available agent. For more information, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-routing-profiles-priority.html Queues: priority and delay>
-- in the /Amazon Connect Administrator Guide/.
newRoutingProfileQueueConfig ::
  -- | 'queueReference'
  RoutingProfileQueueReference ->
  -- | 'priority'
  Prelude.Natural ->
  -- | 'delay'
  Prelude.Natural ->
  RoutingProfileQueueConfig
newRoutingProfileQueueConfig
  pQueueReference_
  pPriority_
  pDelay_ =
    RoutingProfileQueueConfig'
      { queueReference =
          pQueueReference_,
        priority = pPriority_,
        delay = pDelay_
      }

-- | Contains information about a queue resource.
routingProfileQueueConfig_queueReference :: Lens.Lens' RoutingProfileQueueConfig RoutingProfileQueueReference
routingProfileQueueConfig_queueReference = Lens.lens (\RoutingProfileQueueConfig' {queueReference} -> queueReference) (\s@RoutingProfileQueueConfig' {} a -> s {queueReference = a} :: RoutingProfileQueueConfig)

-- | The order in which contacts are to be handled for the queue. For more
-- information, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-routing-profiles-priority.html Queues: priority and delay>.
routingProfileQueueConfig_priority :: Lens.Lens' RoutingProfileQueueConfig Prelude.Natural
routingProfileQueueConfig_priority = Lens.lens (\RoutingProfileQueueConfig' {priority} -> priority) (\s@RoutingProfileQueueConfig' {} a -> s {priority = a} :: RoutingProfileQueueConfig)

-- | The delay, in seconds, a contact should be in the queue before they are
-- routed to an available agent. For more information, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-routing-profiles-priority.html Queues: priority and delay>
-- in the /Amazon Connect Administrator Guide/.
routingProfileQueueConfig_delay :: Lens.Lens' RoutingProfileQueueConfig Prelude.Natural
routingProfileQueueConfig_delay = Lens.lens (\RoutingProfileQueueConfig' {delay} -> delay) (\s@RoutingProfileQueueConfig' {} a -> s {delay = a} :: RoutingProfileQueueConfig)

instance Prelude.Hashable RoutingProfileQueueConfig where
  hashWithSalt _salt RoutingProfileQueueConfig' {..} =
    _salt `Prelude.hashWithSalt` queueReference
      `Prelude.hashWithSalt` priority
      `Prelude.hashWithSalt` delay

instance Prelude.NFData RoutingProfileQueueConfig where
  rnf RoutingProfileQueueConfig' {..} =
    Prelude.rnf queueReference
      `Prelude.seq` Prelude.rnf priority
      `Prelude.seq` Prelude.rnf delay

instance Data.ToJSON RoutingProfileQueueConfig where
  toJSON RoutingProfileQueueConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("QueueReference" Data..= queueReference),
            Prelude.Just ("Priority" Data..= priority),
            Prelude.Just ("Delay" Data..= delay)
          ]
      )
