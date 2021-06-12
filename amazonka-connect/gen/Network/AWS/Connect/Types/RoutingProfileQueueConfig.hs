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
-- Module      : Network.AWS.Connect.Types.RoutingProfileQueueConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.RoutingProfileQueueConfig where

import Network.AWS.Connect.Types.RoutingProfileQueueReference
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

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
    priority :: Core.Natural,
    -- | The delay, in seconds, a contact should be in the queue before they are
    -- routed to an available agent. For more information, see
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-routing-profiles-priority.html Queues: priority and delay>
    -- in the /Amazon Connect Administrator Guide/.
    delay :: Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Natural ->
  -- | 'delay'
  Core.Natural ->
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
routingProfileQueueConfig_priority :: Lens.Lens' RoutingProfileQueueConfig Core.Natural
routingProfileQueueConfig_priority = Lens.lens (\RoutingProfileQueueConfig' {priority} -> priority) (\s@RoutingProfileQueueConfig' {} a -> s {priority = a} :: RoutingProfileQueueConfig)

-- | The delay, in seconds, a contact should be in the queue before they are
-- routed to an available agent. For more information, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-routing-profiles-priority.html Queues: priority and delay>
-- in the /Amazon Connect Administrator Guide/.
routingProfileQueueConfig_delay :: Lens.Lens' RoutingProfileQueueConfig Core.Natural
routingProfileQueueConfig_delay = Lens.lens (\RoutingProfileQueueConfig' {delay} -> delay) (\s@RoutingProfileQueueConfig' {} a -> s {delay = a} :: RoutingProfileQueueConfig)

instance Core.Hashable RoutingProfileQueueConfig

instance Core.NFData RoutingProfileQueueConfig

instance Core.ToJSON RoutingProfileQueueConfig where
  toJSON RoutingProfileQueueConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("QueueReference" Core..= queueReference),
            Core.Just ("Priority" Core..= priority),
            Core.Just ("Delay" Core..= delay)
          ]
      )
