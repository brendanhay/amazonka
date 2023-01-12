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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.Queue where

import Amazonka.Connect.Types.OutboundCallerConfig
import Amazonka.Connect.Types.QueueStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a queue.
--
-- /See:/ 'newQueue' smart constructor.
data Queue = Queue'
  { -- | The description of the queue.
    description :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the hours of operation.
    hoursOfOperationId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of contacts that can be in the queue before it is
    -- considered full.
    maxContacts :: Prelude.Maybe Prelude.Natural,
    -- | The name of the queue.
    name :: Prelude.Maybe Prelude.Text,
    -- | The outbound caller ID name, number, and outbound whisper flow.
    outboundCallerConfig :: Prelude.Maybe OutboundCallerConfig,
    -- | The Amazon Resource Name (ARN) for the queue.
    queueArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the queue.
    queueId :: Prelude.Maybe Prelude.Text,
    -- | The status of the queue.
    status :: Prelude.Maybe QueueStatus,
    -- | The tags used to organize, track, or control access for this resource.
    -- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
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
-- 'description', 'queue_description' - The description of the queue.
--
-- 'hoursOfOperationId', 'queue_hoursOfOperationId' - The identifier for the hours of operation.
--
-- 'maxContacts', 'queue_maxContacts' - The maximum number of contacts that can be in the queue before it is
-- considered full.
--
-- 'name', 'queue_name' - The name of the queue.
--
-- 'outboundCallerConfig', 'queue_outboundCallerConfig' - The outbound caller ID name, number, and outbound whisper flow.
--
-- 'queueArn', 'queue_queueArn' - The Amazon Resource Name (ARN) for the queue.
--
-- 'queueId', 'queue_queueId' - The identifier for the queue.
--
-- 'status', 'queue_status' - The status of the queue.
--
-- 'tags', 'queue_tags' - The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
newQueue ::
  Queue
newQueue =
  Queue'
    { description = Prelude.Nothing,
      hoursOfOperationId = Prelude.Nothing,
      maxContacts = Prelude.Nothing,
      name = Prelude.Nothing,
      outboundCallerConfig = Prelude.Nothing,
      queueArn = Prelude.Nothing,
      queueId = Prelude.Nothing,
      status = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The description of the queue.
queue_description :: Lens.Lens' Queue (Prelude.Maybe Prelude.Text)
queue_description = Lens.lens (\Queue' {description} -> description) (\s@Queue' {} a -> s {description = a} :: Queue)

-- | The identifier for the hours of operation.
queue_hoursOfOperationId :: Lens.Lens' Queue (Prelude.Maybe Prelude.Text)
queue_hoursOfOperationId = Lens.lens (\Queue' {hoursOfOperationId} -> hoursOfOperationId) (\s@Queue' {} a -> s {hoursOfOperationId = a} :: Queue)

-- | The maximum number of contacts that can be in the queue before it is
-- considered full.
queue_maxContacts :: Lens.Lens' Queue (Prelude.Maybe Prelude.Natural)
queue_maxContacts = Lens.lens (\Queue' {maxContacts} -> maxContacts) (\s@Queue' {} a -> s {maxContacts = a} :: Queue)

-- | The name of the queue.
queue_name :: Lens.Lens' Queue (Prelude.Maybe Prelude.Text)
queue_name = Lens.lens (\Queue' {name} -> name) (\s@Queue' {} a -> s {name = a} :: Queue)

-- | The outbound caller ID name, number, and outbound whisper flow.
queue_outboundCallerConfig :: Lens.Lens' Queue (Prelude.Maybe OutboundCallerConfig)
queue_outboundCallerConfig = Lens.lens (\Queue' {outboundCallerConfig} -> outboundCallerConfig) (\s@Queue' {} a -> s {outboundCallerConfig = a} :: Queue)

-- | The Amazon Resource Name (ARN) for the queue.
queue_queueArn :: Lens.Lens' Queue (Prelude.Maybe Prelude.Text)
queue_queueArn = Lens.lens (\Queue' {queueArn} -> queueArn) (\s@Queue' {} a -> s {queueArn = a} :: Queue)

-- | The identifier for the queue.
queue_queueId :: Lens.Lens' Queue (Prelude.Maybe Prelude.Text)
queue_queueId = Lens.lens (\Queue' {queueId} -> queueId) (\s@Queue' {} a -> s {queueId = a} :: Queue)

-- | The status of the queue.
queue_status :: Lens.Lens' Queue (Prelude.Maybe QueueStatus)
queue_status = Lens.lens (\Queue' {status} -> status) (\s@Queue' {} a -> s {status = a} :: Queue)

-- | The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
queue_tags :: Lens.Lens' Queue (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
queue_tags = Lens.lens (\Queue' {tags} -> tags) (\s@Queue' {} a -> s {tags = a} :: Queue) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Queue where
  parseJSON =
    Data.withObject
      "Queue"
      ( \x ->
          Queue'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "HoursOfOperationId")
            Prelude.<*> (x Data..:? "MaxContacts")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "OutboundCallerConfig")
            Prelude.<*> (x Data..:? "QueueArn")
            Prelude.<*> (x Data..:? "QueueId")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Queue where
  hashWithSalt _salt Queue' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` hoursOfOperationId
      `Prelude.hashWithSalt` maxContacts
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` outboundCallerConfig
      `Prelude.hashWithSalt` queueArn
      `Prelude.hashWithSalt` queueId
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` tags

instance Prelude.NFData Queue where
  rnf Queue' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf hoursOfOperationId
      `Prelude.seq` Prelude.rnf maxContacts
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf outboundCallerConfig
      `Prelude.seq` Prelude.rnf queueArn
      `Prelude.seq` Prelude.rnf queueId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf tags
