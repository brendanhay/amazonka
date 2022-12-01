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
-- Module      : Amazonka.Connect.Types.Queue
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.Queue where

import Amazonka.Connect.Types.OutboundCallerConfig
import Amazonka.Connect.Types.QueueStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a queue.
--
-- /See:/ 'newQueue' smart constructor.
data Queue = Queue'
  { -- | The tags used to organize, track, or control access for this resource.
    -- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the queue.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the queue.
    queueArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the queue.
    status :: Prelude.Maybe QueueStatus,
    -- | The description of the queue.
    description :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the queue.
    queueId :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the hours of operation.
    hoursOfOperationId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of contacts that can be in the queue before it is
    -- considered full.
    maxContacts :: Prelude.Maybe Prelude.Natural,
    -- | The outbound caller ID name, number, and outbound whisper flow.
    outboundCallerConfig :: Prelude.Maybe OutboundCallerConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Queue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'queue_tags' - The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
--
-- 'name', 'queue_name' - The name of the queue.
--
-- 'queueArn', 'queue_queueArn' - The Amazon Resource Name (ARN) for the queue.
--
-- 'status', 'queue_status' - The status of the queue.
--
-- 'description', 'queue_description' - The description of the queue.
--
-- 'queueId', 'queue_queueId' - The identifier for the queue.
--
-- 'hoursOfOperationId', 'queue_hoursOfOperationId' - The identifier for the hours of operation.
--
-- 'maxContacts', 'queue_maxContacts' - The maximum number of contacts that can be in the queue before it is
-- considered full.
--
-- 'outboundCallerConfig', 'queue_outboundCallerConfig' - The outbound caller ID name, number, and outbound whisper flow.
newQueue ::
  Queue
newQueue =
  Queue'
    { tags = Prelude.Nothing,
      name = Prelude.Nothing,
      queueArn = Prelude.Nothing,
      status = Prelude.Nothing,
      description = Prelude.Nothing,
      queueId = Prelude.Nothing,
      hoursOfOperationId = Prelude.Nothing,
      maxContacts = Prelude.Nothing,
      outboundCallerConfig = Prelude.Nothing
    }

-- | The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
queue_tags :: Lens.Lens' Queue (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
queue_tags = Lens.lens (\Queue' {tags} -> tags) (\s@Queue' {} a -> s {tags = a} :: Queue) Prelude.. Lens.mapping Lens.coerced

-- | The name of the queue.
queue_name :: Lens.Lens' Queue (Prelude.Maybe Prelude.Text)
queue_name = Lens.lens (\Queue' {name} -> name) (\s@Queue' {} a -> s {name = a} :: Queue)

-- | The Amazon Resource Name (ARN) for the queue.
queue_queueArn :: Lens.Lens' Queue (Prelude.Maybe Prelude.Text)
queue_queueArn = Lens.lens (\Queue' {queueArn} -> queueArn) (\s@Queue' {} a -> s {queueArn = a} :: Queue)

-- | The status of the queue.
queue_status :: Lens.Lens' Queue (Prelude.Maybe QueueStatus)
queue_status = Lens.lens (\Queue' {status} -> status) (\s@Queue' {} a -> s {status = a} :: Queue)

-- | The description of the queue.
queue_description :: Lens.Lens' Queue (Prelude.Maybe Prelude.Text)
queue_description = Lens.lens (\Queue' {description} -> description) (\s@Queue' {} a -> s {description = a} :: Queue)

-- | The identifier for the queue.
queue_queueId :: Lens.Lens' Queue (Prelude.Maybe Prelude.Text)
queue_queueId = Lens.lens (\Queue' {queueId} -> queueId) (\s@Queue' {} a -> s {queueId = a} :: Queue)

-- | The identifier for the hours of operation.
queue_hoursOfOperationId :: Lens.Lens' Queue (Prelude.Maybe Prelude.Text)
queue_hoursOfOperationId = Lens.lens (\Queue' {hoursOfOperationId} -> hoursOfOperationId) (\s@Queue' {} a -> s {hoursOfOperationId = a} :: Queue)

-- | The maximum number of contacts that can be in the queue before it is
-- considered full.
queue_maxContacts :: Lens.Lens' Queue (Prelude.Maybe Prelude.Natural)
queue_maxContacts = Lens.lens (\Queue' {maxContacts} -> maxContacts) (\s@Queue' {} a -> s {maxContacts = a} :: Queue)

-- | The outbound caller ID name, number, and outbound whisper flow.
queue_outboundCallerConfig :: Lens.Lens' Queue (Prelude.Maybe OutboundCallerConfig)
queue_outboundCallerConfig = Lens.lens (\Queue' {outboundCallerConfig} -> outboundCallerConfig) (\s@Queue' {} a -> s {outboundCallerConfig = a} :: Queue)

instance Core.FromJSON Queue where
  parseJSON =
    Core.withObject
      "Queue"
      ( \x ->
          Queue'
            Prelude.<$> (x Core..:? "Tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "QueueArn")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "QueueId")
            Prelude.<*> (x Core..:? "HoursOfOperationId")
            Prelude.<*> (x Core..:? "MaxContacts")
            Prelude.<*> (x Core..:? "OutboundCallerConfig")
      )

instance Prelude.Hashable Queue where
  hashWithSalt _salt Queue' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` queueArn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` queueId
      `Prelude.hashWithSalt` hoursOfOperationId
      `Prelude.hashWithSalt` maxContacts
      `Prelude.hashWithSalt` outboundCallerConfig

instance Prelude.NFData Queue where
  rnf Queue' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf queueArn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf queueId
      `Prelude.seq` Prelude.rnf hoursOfOperationId
      `Prelude.seq` Prelude.rnf maxContacts
      `Prelude.seq` Prelude.rnf outboundCallerConfig
