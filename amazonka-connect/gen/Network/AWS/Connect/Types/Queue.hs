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
-- Module      : Network.AWS.Connect.Types.Queue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.Queue where

import Network.AWS.Connect.Types.OutboundCallerConfig
import Network.AWS.Connect.Types.QueueStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about a queue.
--
-- /See:/ 'newQueue' smart constructor.
data Queue = Queue'
  { -- | The maximum number of contacts that can be in the queue before it is
    -- considered full.
    maxContacts :: Prelude.Maybe Prelude.Natural,
    -- | The status of the queue.
    status :: Prelude.Maybe QueueStatus,
    -- | The identifier for the queue.
    queueId :: Prelude.Maybe Prelude.Text,
    -- | The name of the queue.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the queue.
    queueArn :: Prelude.Maybe Prelude.Text,
    -- | One or more tags.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The description of the queue.
    description :: Prelude.Maybe Prelude.Text,
    -- | The outbound caller ID name, number, and outbound whisper flow.
    outboundCallerConfig :: Prelude.Maybe OutboundCallerConfig,
    -- | The identifier for the hours of operation.
    hoursOfOperationId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Queue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxContacts', 'queue_maxContacts' - The maximum number of contacts that can be in the queue before it is
-- considered full.
--
-- 'status', 'queue_status' - The status of the queue.
--
-- 'queueId', 'queue_queueId' - The identifier for the queue.
--
-- 'name', 'queue_name' - The name of the queue.
--
-- 'queueArn', 'queue_queueArn' - The Amazon Resource Name (ARN) for the queue.
--
-- 'tags', 'queue_tags' - One or more tags.
--
-- 'description', 'queue_description' - The description of the queue.
--
-- 'outboundCallerConfig', 'queue_outboundCallerConfig' - The outbound caller ID name, number, and outbound whisper flow.
--
-- 'hoursOfOperationId', 'queue_hoursOfOperationId' - The identifier for the hours of operation.
newQueue ::
  Queue
newQueue =
  Queue'
    { maxContacts = Prelude.Nothing,
      status = Prelude.Nothing,
      queueId = Prelude.Nothing,
      name = Prelude.Nothing,
      queueArn = Prelude.Nothing,
      tags = Prelude.Nothing,
      description = Prelude.Nothing,
      outboundCallerConfig = Prelude.Nothing,
      hoursOfOperationId = Prelude.Nothing
    }

-- | The maximum number of contacts that can be in the queue before it is
-- considered full.
queue_maxContacts :: Lens.Lens' Queue (Prelude.Maybe Prelude.Natural)
queue_maxContacts = Lens.lens (\Queue' {maxContacts} -> maxContacts) (\s@Queue' {} a -> s {maxContacts = a} :: Queue)

-- | The status of the queue.
queue_status :: Lens.Lens' Queue (Prelude.Maybe QueueStatus)
queue_status = Lens.lens (\Queue' {status} -> status) (\s@Queue' {} a -> s {status = a} :: Queue)

-- | The identifier for the queue.
queue_queueId :: Lens.Lens' Queue (Prelude.Maybe Prelude.Text)
queue_queueId = Lens.lens (\Queue' {queueId} -> queueId) (\s@Queue' {} a -> s {queueId = a} :: Queue)

-- | The name of the queue.
queue_name :: Lens.Lens' Queue (Prelude.Maybe Prelude.Text)
queue_name = Lens.lens (\Queue' {name} -> name) (\s@Queue' {} a -> s {name = a} :: Queue)

-- | The Amazon Resource Name (ARN) for the queue.
queue_queueArn :: Lens.Lens' Queue (Prelude.Maybe Prelude.Text)
queue_queueArn = Lens.lens (\Queue' {queueArn} -> queueArn) (\s@Queue' {} a -> s {queueArn = a} :: Queue)

-- | One or more tags.
queue_tags :: Lens.Lens' Queue (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
queue_tags = Lens.lens (\Queue' {tags} -> tags) (\s@Queue' {} a -> s {tags = a} :: Queue) Prelude.. Lens.mapping Prelude._Coerce

-- | The description of the queue.
queue_description :: Lens.Lens' Queue (Prelude.Maybe Prelude.Text)
queue_description = Lens.lens (\Queue' {description} -> description) (\s@Queue' {} a -> s {description = a} :: Queue)

-- | The outbound caller ID name, number, and outbound whisper flow.
queue_outboundCallerConfig :: Lens.Lens' Queue (Prelude.Maybe OutboundCallerConfig)
queue_outboundCallerConfig = Lens.lens (\Queue' {outboundCallerConfig} -> outboundCallerConfig) (\s@Queue' {} a -> s {outboundCallerConfig = a} :: Queue)

-- | The identifier for the hours of operation.
queue_hoursOfOperationId :: Lens.Lens' Queue (Prelude.Maybe Prelude.Text)
queue_hoursOfOperationId = Lens.lens (\Queue' {hoursOfOperationId} -> hoursOfOperationId) (\s@Queue' {} a -> s {hoursOfOperationId = a} :: Queue)

instance Prelude.FromJSON Queue where
  parseJSON =
    Prelude.withObject
      "Queue"
      ( \x ->
          Queue'
            Prelude.<$> (x Prelude..:? "MaxContacts")
            Prelude.<*> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "QueueId")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "QueueArn")
            Prelude.<*> (x Prelude..:? "Tags" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "Description")
            Prelude.<*> (x Prelude..:? "OutboundCallerConfig")
            Prelude.<*> (x Prelude..:? "HoursOfOperationId")
      )

instance Prelude.Hashable Queue

instance Prelude.NFData Queue
