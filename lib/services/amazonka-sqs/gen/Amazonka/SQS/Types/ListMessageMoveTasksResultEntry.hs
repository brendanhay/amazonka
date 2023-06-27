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
-- Module      : Amazonka.SQS.Types.ListMessageMoveTasksResultEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SQS.Types.ListMessageMoveTasksResultEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the details of a message movement task.
--
-- /See:/ 'newListMessageMoveTasksResultEntry' smart constructor.
data ListMessageMoveTasksResultEntry = ListMessageMoveTasksResultEntry'
  { -- | The approximate number of messages already moved to the destination
    -- queue.
    approximateNumberOfMessagesMoved :: Prelude.Maybe Prelude.Integer,
    -- | The number of messages to be moved from the source queue. This number is
    -- obtained at the time of starting the message movement task.
    approximateNumberOfMessagesToMove :: Prelude.Maybe Prelude.Integer,
    -- | The ARN of the destination queue if it has been specified in the
    -- @StartMessageMoveTask@ request. If a @DestinationArn@ has not been
    -- specified in the @StartMessageMoveTask@ request, this field value will
    -- be NULL.
    destinationArn :: Prelude.Maybe Prelude.Text,
    -- | The task failure reason (only included if the task status is FAILED).
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The number of messages to be moved per second (the message movement
    -- rate), if it has been specified in the @StartMessageMoveTask@ request.
    -- If a @MaxNumberOfMessagesPerSecond@ has not been specified in the
    -- @StartMessageMoveTask@ request, this field value will be NULL.
    maxNumberOfMessagesPerSecond :: Prelude.Maybe Prelude.Int,
    -- | The ARN of the queue that contains the messages to be moved to another
    -- queue.
    sourceArn :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of starting the message movement task.
    startedTimestamp :: Prelude.Maybe Prelude.Integer,
    -- | The status of the message movement task. Possible values are: RUNNING,
    -- COMPLETED, CANCELLING, CANCELLED, and FAILED.
    status :: Prelude.Maybe Prelude.Text,
    -- | An identifier associated with a message movement task. When this field
    -- is returned in the response of the @ListMessageMoveTasks@ action, it is
    -- only populated for tasks that are in RUNNING status.
    taskHandle :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMessageMoveTasksResultEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'approximateNumberOfMessagesMoved', 'listMessageMoveTasksResultEntry_approximateNumberOfMessagesMoved' - The approximate number of messages already moved to the destination
-- queue.
--
-- 'approximateNumberOfMessagesToMove', 'listMessageMoveTasksResultEntry_approximateNumberOfMessagesToMove' - The number of messages to be moved from the source queue. This number is
-- obtained at the time of starting the message movement task.
--
-- 'destinationArn', 'listMessageMoveTasksResultEntry_destinationArn' - The ARN of the destination queue if it has been specified in the
-- @StartMessageMoveTask@ request. If a @DestinationArn@ has not been
-- specified in the @StartMessageMoveTask@ request, this field value will
-- be NULL.
--
-- 'failureReason', 'listMessageMoveTasksResultEntry_failureReason' - The task failure reason (only included if the task status is FAILED).
--
-- 'maxNumberOfMessagesPerSecond', 'listMessageMoveTasksResultEntry_maxNumberOfMessagesPerSecond' - The number of messages to be moved per second (the message movement
-- rate), if it has been specified in the @StartMessageMoveTask@ request.
-- If a @MaxNumberOfMessagesPerSecond@ has not been specified in the
-- @StartMessageMoveTask@ request, this field value will be NULL.
--
-- 'sourceArn', 'listMessageMoveTasksResultEntry_sourceArn' - The ARN of the queue that contains the messages to be moved to another
-- queue.
--
-- 'startedTimestamp', 'listMessageMoveTasksResultEntry_startedTimestamp' - The timestamp of starting the message movement task.
--
-- 'status', 'listMessageMoveTasksResultEntry_status' - The status of the message movement task. Possible values are: RUNNING,
-- COMPLETED, CANCELLING, CANCELLED, and FAILED.
--
-- 'taskHandle', 'listMessageMoveTasksResultEntry_taskHandle' - An identifier associated with a message movement task. When this field
-- is returned in the response of the @ListMessageMoveTasks@ action, it is
-- only populated for tasks that are in RUNNING status.
newListMessageMoveTasksResultEntry ::
  ListMessageMoveTasksResultEntry
newListMessageMoveTasksResultEntry =
  ListMessageMoveTasksResultEntry'
    { approximateNumberOfMessagesMoved =
        Prelude.Nothing,
      approximateNumberOfMessagesToMove =
        Prelude.Nothing,
      destinationArn = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      maxNumberOfMessagesPerSecond =
        Prelude.Nothing,
      sourceArn = Prelude.Nothing,
      startedTimestamp = Prelude.Nothing,
      status = Prelude.Nothing,
      taskHandle = Prelude.Nothing
    }

-- | The approximate number of messages already moved to the destination
-- queue.
listMessageMoveTasksResultEntry_approximateNumberOfMessagesMoved :: Lens.Lens' ListMessageMoveTasksResultEntry (Prelude.Maybe Prelude.Integer)
listMessageMoveTasksResultEntry_approximateNumberOfMessagesMoved = Lens.lens (\ListMessageMoveTasksResultEntry' {approximateNumberOfMessagesMoved} -> approximateNumberOfMessagesMoved) (\s@ListMessageMoveTasksResultEntry' {} a -> s {approximateNumberOfMessagesMoved = a} :: ListMessageMoveTasksResultEntry)

-- | The number of messages to be moved from the source queue. This number is
-- obtained at the time of starting the message movement task.
listMessageMoveTasksResultEntry_approximateNumberOfMessagesToMove :: Lens.Lens' ListMessageMoveTasksResultEntry (Prelude.Maybe Prelude.Integer)
listMessageMoveTasksResultEntry_approximateNumberOfMessagesToMove = Lens.lens (\ListMessageMoveTasksResultEntry' {approximateNumberOfMessagesToMove} -> approximateNumberOfMessagesToMove) (\s@ListMessageMoveTasksResultEntry' {} a -> s {approximateNumberOfMessagesToMove = a} :: ListMessageMoveTasksResultEntry)

-- | The ARN of the destination queue if it has been specified in the
-- @StartMessageMoveTask@ request. If a @DestinationArn@ has not been
-- specified in the @StartMessageMoveTask@ request, this field value will
-- be NULL.
listMessageMoveTasksResultEntry_destinationArn :: Lens.Lens' ListMessageMoveTasksResultEntry (Prelude.Maybe Prelude.Text)
listMessageMoveTasksResultEntry_destinationArn = Lens.lens (\ListMessageMoveTasksResultEntry' {destinationArn} -> destinationArn) (\s@ListMessageMoveTasksResultEntry' {} a -> s {destinationArn = a} :: ListMessageMoveTasksResultEntry)

-- | The task failure reason (only included if the task status is FAILED).
listMessageMoveTasksResultEntry_failureReason :: Lens.Lens' ListMessageMoveTasksResultEntry (Prelude.Maybe Prelude.Text)
listMessageMoveTasksResultEntry_failureReason = Lens.lens (\ListMessageMoveTasksResultEntry' {failureReason} -> failureReason) (\s@ListMessageMoveTasksResultEntry' {} a -> s {failureReason = a} :: ListMessageMoveTasksResultEntry)

-- | The number of messages to be moved per second (the message movement
-- rate), if it has been specified in the @StartMessageMoveTask@ request.
-- If a @MaxNumberOfMessagesPerSecond@ has not been specified in the
-- @StartMessageMoveTask@ request, this field value will be NULL.
listMessageMoveTasksResultEntry_maxNumberOfMessagesPerSecond :: Lens.Lens' ListMessageMoveTasksResultEntry (Prelude.Maybe Prelude.Int)
listMessageMoveTasksResultEntry_maxNumberOfMessagesPerSecond = Lens.lens (\ListMessageMoveTasksResultEntry' {maxNumberOfMessagesPerSecond} -> maxNumberOfMessagesPerSecond) (\s@ListMessageMoveTasksResultEntry' {} a -> s {maxNumberOfMessagesPerSecond = a} :: ListMessageMoveTasksResultEntry)

-- | The ARN of the queue that contains the messages to be moved to another
-- queue.
listMessageMoveTasksResultEntry_sourceArn :: Lens.Lens' ListMessageMoveTasksResultEntry (Prelude.Maybe Prelude.Text)
listMessageMoveTasksResultEntry_sourceArn = Lens.lens (\ListMessageMoveTasksResultEntry' {sourceArn} -> sourceArn) (\s@ListMessageMoveTasksResultEntry' {} a -> s {sourceArn = a} :: ListMessageMoveTasksResultEntry)

-- | The timestamp of starting the message movement task.
listMessageMoveTasksResultEntry_startedTimestamp :: Lens.Lens' ListMessageMoveTasksResultEntry (Prelude.Maybe Prelude.Integer)
listMessageMoveTasksResultEntry_startedTimestamp = Lens.lens (\ListMessageMoveTasksResultEntry' {startedTimestamp} -> startedTimestamp) (\s@ListMessageMoveTasksResultEntry' {} a -> s {startedTimestamp = a} :: ListMessageMoveTasksResultEntry)

-- | The status of the message movement task. Possible values are: RUNNING,
-- COMPLETED, CANCELLING, CANCELLED, and FAILED.
listMessageMoveTasksResultEntry_status :: Lens.Lens' ListMessageMoveTasksResultEntry (Prelude.Maybe Prelude.Text)
listMessageMoveTasksResultEntry_status = Lens.lens (\ListMessageMoveTasksResultEntry' {status} -> status) (\s@ListMessageMoveTasksResultEntry' {} a -> s {status = a} :: ListMessageMoveTasksResultEntry)

-- | An identifier associated with a message movement task. When this field
-- is returned in the response of the @ListMessageMoveTasks@ action, it is
-- only populated for tasks that are in RUNNING status.
listMessageMoveTasksResultEntry_taskHandle :: Lens.Lens' ListMessageMoveTasksResultEntry (Prelude.Maybe Prelude.Text)
listMessageMoveTasksResultEntry_taskHandle = Lens.lens (\ListMessageMoveTasksResultEntry' {taskHandle} -> taskHandle) (\s@ListMessageMoveTasksResultEntry' {} a -> s {taskHandle = a} :: ListMessageMoveTasksResultEntry)

instance Data.FromXML ListMessageMoveTasksResultEntry where
  parseXML x =
    ListMessageMoveTasksResultEntry'
      Prelude.<$> (x Data..@? "ApproximateNumberOfMessagesMoved")
      Prelude.<*> (x Data..@? "ApproximateNumberOfMessagesToMove")
      Prelude.<*> (x Data..@? "DestinationArn")
      Prelude.<*> (x Data..@? "FailureReason")
      Prelude.<*> (x Data..@? "MaxNumberOfMessagesPerSecond")
      Prelude.<*> (x Data..@? "SourceArn")
      Prelude.<*> (x Data..@? "StartedTimestamp")
      Prelude.<*> (x Data..@? "Status")
      Prelude.<*> (x Data..@? "TaskHandle")

instance
  Prelude.Hashable
    ListMessageMoveTasksResultEntry
  where
  hashWithSalt
    _salt
    ListMessageMoveTasksResultEntry' {..} =
      _salt
        `Prelude.hashWithSalt` approximateNumberOfMessagesMoved
        `Prelude.hashWithSalt` approximateNumberOfMessagesToMove
        `Prelude.hashWithSalt` destinationArn
        `Prelude.hashWithSalt` failureReason
        `Prelude.hashWithSalt` maxNumberOfMessagesPerSecond
        `Prelude.hashWithSalt` sourceArn
        `Prelude.hashWithSalt` startedTimestamp
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` taskHandle

instance
  Prelude.NFData
    ListMessageMoveTasksResultEntry
  where
  rnf ListMessageMoveTasksResultEntry' {..} =
    Prelude.rnf approximateNumberOfMessagesMoved
      `Prelude.seq` Prelude.rnf approximateNumberOfMessagesToMove
      `Prelude.seq` Prelude.rnf destinationArn
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf maxNumberOfMessagesPerSecond
      `Prelude.seq` Prelude.rnf sourceArn
      `Prelude.seq` Prelude.rnf startedTimestamp
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf taskHandle
