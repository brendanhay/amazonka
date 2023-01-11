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
-- Module      : Amazonka.Connect.Types.RoutingProfileQueueConfigSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.RoutingProfileQueueConfigSummary where

import Amazonka.Connect.Types.Channel
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains summary information about a routing profile queue.
--
-- /See:/ 'newRoutingProfileQueueConfigSummary' smart constructor.
data RoutingProfileQueueConfigSummary = RoutingProfileQueueConfigSummary'
  { -- | The identifier for the queue.
    queueId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the queue.
    queueArn :: Prelude.Text,
    -- | The name of the queue.
    queueName :: Prelude.Text,
    -- | The order in which contacts are to be handled for the queue. For more
    -- information, see
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-routing-profiles-priority.html Queues: priority and delay>.
    priority :: Prelude.Natural,
    -- | The delay, in seconds, that a contact should be in the queue before they
    -- are routed to an available agent. For more information, see
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-routing-profiles-priority.html Queues: priority and delay>
    -- in the /Amazon Connect Administrator Guide/.
    delay :: Prelude.Natural,
    -- | The channels this queue supports.
    channel :: Channel
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RoutingProfileQueueConfigSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queueId', 'routingProfileQueueConfigSummary_queueId' - The identifier for the queue.
--
-- 'queueArn', 'routingProfileQueueConfigSummary_queueArn' - The Amazon Resource Name (ARN) of the queue.
--
-- 'queueName', 'routingProfileQueueConfigSummary_queueName' - The name of the queue.
--
-- 'priority', 'routingProfileQueueConfigSummary_priority' - The order in which contacts are to be handled for the queue. For more
-- information, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-routing-profiles-priority.html Queues: priority and delay>.
--
-- 'delay', 'routingProfileQueueConfigSummary_delay' - The delay, in seconds, that a contact should be in the queue before they
-- are routed to an available agent. For more information, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-routing-profiles-priority.html Queues: priority and delay>
-- in the /Amazon Connect Administrator Guide/.
--
-- 'channel', 'routingProfileQueueConfigSummary_channel' - The channels this queue supports.
newRoutingProfileQueueConfigSummary ::
  -- | 'queueId'
  Prelude.Text ->
  -- | 'queueArn'
  Prelude.Text ->
  -- | 'queueName'
  Prelude.Text ->
  -- | 'priority'
  Prelude.Natural ->
  -- | 'delay'
  Prelude.Natural ->
  -- | 'channel'
  Channel ->
  RoutingProfileQueueConfigSummary
newRoutingProfileQueueConfigSummary
  pQueueId_
  pQueueArn_
  pQueueName_
  pPriority_
  pDelay_
  pChannel_ =
    RoutingProfileQueueConfigSummary'
      { queueId =
          pQueueId_,
        queueArn = pQueueArn_,
        queueName = pQueueName_,
        priority = pPriority_,
        delay = pDelay_,
        channel = pChannel_
      }

-- | The identifier for the queue.
routingProfileQueueConfigSummary_queueId :: Lens.Lens' RoutingProfileQueueConfigSummary Prelude.Text
routingProfileQueueConfigSummary_queueId = Lens.lens (\RoutingProfileQueueConfigSummary' {queueId} -> queueId) (\s@RoutingProfileQueueConfigSummary' {} a -> s {queueId = a} :: RoutingProfileQueueConfigSummary)

-- | The Amazon Resource Name (ARN) of the queue.
routingProfileQueueConfigSummary_queueArn :: Lens.Lens' RoutingProfileQueueConfigSummary Prelude.Text
routingProfileQueueConfigSummary_queueArn = Lens.lens (\RoutingProfileQueueConfigSummary' {queueArn} -> queueArn) (\s@RoutingProfileQueueConfigSummary' {} a -> s {queueArn = a} :: RoutingProfileQueueConfigSummary)

-- | The name of the queue.
routingProfileQueueConfigSummary_queueName :: Lens.Lens' RoutingProfileQueueConfigSummary Prelude.Text
routingProfileQueueConfigSummary_queueName = Lens.lens (\RoutingProfileQueueConfigSummary' {queueName} -> queueName) (\s@RoutingProfileQueueConfigSummary' {} a -> s {queueName = a} :: RoutingProfileQueueConfigSummary)

-- | The order in which contacts are to be handled for the queue. For more
-- information, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-routing-profiles-priority.html Queues: priority and delay>.
routingProfileQueueConfigSummary_priority :: Lens.Lens' RoutingProfileQueueConfigSummary Prelude.Natural
routingProfileQueueConfigSummary_priority = Lens.lens (\RoutingProfileQueueConfigSummary' {priority} -> priority) (\s@RoutingProfileQueueConfigSummary' {} a -> s {priority = a} :: RoutingProfileQueueConfigSummary)

-- | The delay, in seconds, that a contact should be in the queue before they
-- are routed to an available agent. For more information, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-routing-profiles-priority.html Queues: priority and delay>
-- in the /Amazon Connect Administrator Guide/.
routingProfileQueueConfigSummary_delay :: Lens.Lens' RoutingProfileQueueConfigSummary Prelude.Natural
routingProfileQueueConfigSummary_delay = Lens.lens (\RoutingProfileQueueConfigSummary' {delay} -> delay) (\s@RoutingProfileQueueConfigSummary' {} a -> s {delay = a} :: RoutingProfileQueueConfigSummary)

-- | The channels this queue supports.
routingProfileQueueConfigSummary_channel :: Lens.Lens' RoutingProfileQueueConfigSummary Channel
routingProfileQueueConfigSummary_channel = Lens.lens (\RoutingProfileQueueConfigSummary' {channel} -> channel) (\s@RoutingProfileQueueConfigSummary' {} a -> s {channel = a} :: RoutingProfileQueueConfigSummary)

instance
  Data.FromJSON
    RoutingProfileQueueConfigSummary
  where
  parseJSON =
    Data.withObject
      "RoutingProfileQueueConfigSummary"
      ( \x ->
          RoutingProfileQueueConfigSummary'
            Prelude.<$> (x Data..: "QueueId")
            Prelude.<*> (x Data..: "QueueArn")
            Prelude.<*> (x Data..: "QueueName")
            Prelude.<*> (x Data..: "Priority")
            Prelude.<*> (x Data..: "Delay")
            Prelude.<*> (x Data..: "Channel")
      )

instance
  Prelude.Hashable
    RoutingProfileQueueConfigSummary
  where
  hashWithSalt
    _salt
    RoutingProfileQueueConfigSummary' {..} =
      _salt `Prelude.hashWithSalt` queueId
        `Prelude.hashWithSalt` queueArn
        `Prelude.hashWithSalt` queueName
        `Prelude.hashWithSalt` priority
        `Prelude.hashWithSalt` delay
        `Prelude.hashWithSalt` channel

instance
  Prelude.NFData
    RoutingProfileQueueConfigSummary
  where
  rnf RoutingProfileQueueConfigSummary' {..} =
    Prelude.rnf queueId
      `Prelude.seq` Prelude.rnf queueArn
      `Prelude.seq` Prelude.rnf queueName
      `Prelude.seq` Prelude.rnf priority
      `Prelude.seq` Prelude.rnf delay
      `Prelude.seq` Prelude.rnf channel
