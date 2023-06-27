{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SQS.StartMessageMoveTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an asynchronous task to move messages from a specified source
-- queue to a specified destination queue.
--
-- -   This action is currently limited to supporting message redrive from
--     dead-letter queues (DLQs) only. In this context, the source queue is
--     the dead-letter queue (DLQ), while the destination queue can be the
--     original source queue (from which the messages were driven to the
--     dead-letter-queue), or a custom destination queue.
--
-- -   Currently, only standard queues are supported.
--
-- -   Only one active message movement task is supported per queue at any
--     given time.
module Amazonka.SQS.StartMessageMoveTask
  ( -- * Creating a Request
    StartMessageMoveTask (..),
    newStartMessageMoveTask,

    -- * Request Lenses
    startMessageMoveTask_destinationArn,
    startMessageMoveTask_maxNumberOfMessagesPerSecond,
    startMessageMoveTask_sourceArn,

    -- * Destructuring the Response
    StartMessageMoveTaskResponse (..),
    newStartMessageMoveTaskResponse,

    -- * Response Lenses
    startMessageMoveTaskResponse_taskHandle,
    startMessageMoveTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SQS.Types

-- | /See:/ 'newStartMessageMoveTask' smart constructor.
data StartMessageMoveTask = StartMessageMoveTask'
  { -- | The ARN of the queue that receives the moved messages. You can use this
    -- field to specify the destination queue where you would like to redrive
    -- messages. If this field is left blank, the messages will be redriven
    -- back to their respective original source queues.
    destinationArn :: Prelude.Maybe Prelude.Text,
    -- | The number of messages to be moved per second (the message movement
    -- rate). You can use this field to define a fixed message movement rate.
    -- The maximum value for messages per second is 500. If this field is left
    -- blank, the system will optimize the rate based on the queue message
    -- backlog size, which may vary throughout the duration of the message
    -- movement task.
    maxNumberOfMessagesPerSecond :: Prelude.Maybe Prelude.Int,
    -- | The ARN of the queue that contains the messages to be moved to another
    -- queue. Currently, only dead-letter queue (DLQ) ARNs are accepted.
    sourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartMessageMoveTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationArn', 'startMessageMoveTask_destinationArn' - The ARN of the queue that receives the moved messages. You can use this
-- field to specify the destination queue where you would like to redrive
-- messages. If this field is left blank, the messages will be redriven
-- back to their respective original source queues.
--
-- 'maxNumberOfMessagesPerSecond', 'startMessageMoveTask_maxNumberOfMessagesPerSecond' - The number of messages to be moved per second (the message movement
-- rate). You can use this field to define a fixed message movement rate.
-- The maximum value for messages per second is 500. If this field is left
-- blank, the system will optimize the rate based on the queue message
-- backlog size, which may vary throughout the duration of the message
-- movement task.
--
-- 'sourceArn', 'startMessageMoveTask_sourceArn' - The ARN of the queue that contains the messages to be moved to another
-- queue. Currently, only dead-letter queue (DLQ) ARNs are accepted.
newStartMessageMoveTask ::
  -- | 'sourceArn'
  Prelude.Text ->
  StartMessageMoveTask
newStartMessageMoveTask pSourceArn_ =
  StartMessageMoveTask'
    { destinationArn =
        Prelude.Nothing,
      maxNumberOfMessagesPerSecond = Prelude.Nothing,
      sourceArn = pSourceArn_
    }

-- | The ARN of the queue that receives the moved messages. You can use this
-- field to specify the destination queue where you would like to redrive
-- messages. If this field is left blank, the messages will be redriven
-- back to their respective original source queues.
startMessageMoveTask_destinationArn :: Lens.Lens' StartMessageMoveTask (Prelude.Maybe Prelude.Text)
startMessageMoveTask_destinationArn = Lens.lens (\StartMessageMoveTask' {destinationArn} -> destinationArn) (\s@StartMessageMoveTask' {} a -> s {destinationArn = a} :: StartMessageMoveTask)

-- | The number of messages to be moved per second (the message movement
-- rate). You can use this field to define a fixed message movement rate.
-- The maximum value for messages per second is 500. If this field is left
-- blank, the system will optimize the rate based on the queue message
-- backlog size, which may vary throughout the duration of the message
-- movement task.
startMessageMoveTask_maxNumberOfMessagesPerSecond :: Lens.Lens' StartMessageMoveTask (Prelude.Maybe Prelude.Int)
startMessageMoveTask_maxNumberOfMessagesPerSecond = Lens.lens (\StartMessageMoveTask' {maxNumberOfMessagesPerSecond} -> maxNumberOfMessagesPerSecond) (\s@StartMessageMoveTask' {} a -> s {maxNumberOfMessagesPerSecond = a} :: StartMessageMoveTask)

-- | The ARN of the queue that contains the messages to be moved to another
-- queue. Currently, only dead-letter queue (DLQ) ARNs are accepted.
startMessageMoveTask_sourceArn :: Lens.Lens' StartMessageMoveTask Prelude.Text
startMessageMoveTask_sourceArn = Lens.lens (\StartMessageMoveTask' {sourceArn} -> sourceArn) (\s@StartMessageMoveTask' {} a -> s {sourceArn = a} :: StartMessageMoveTask)

instance Core.AWSRequest StartMessageMoveTask where
  type
    AWSResponse StartMessageMoveTask =
      StartMessageMoveTaskResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "StartMessageMoveTaskResult"
      ( \s h x ->
          StartMessageMoveTaskResponse'
            Prelude.<$> (x Data..@? "TaskHandle")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartMessageMoveTask where
  hashWithSalt _salt StartMessageMoveTask' {..} =
    _salt
      `Prelude.hashWithSalt` destinationArn
      `Prelude.hashWithSalt` maxNumberOfMessagesPerSecond
      `Prelude.hashWithSalt` sourceArn

instance Prelude.NFData StartMessageMoveTask where
  rnf StartMessageMoveTask' {..} =
    Prelude.rnf destinationArn
      `Prelude.seq` Prelude.rnf maxNumberOfMessagesPerSecond
      `Prelude.seq` Prelude.rnf sourceArn

instance Data.ToHeaders StartMessageMoveTask where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath StartMessageMoveTask where
  toPath = Prelude.const "/"

instance Data.ToQuery StartMessageMoveTask where
  toQuery StartMessageMoveTask' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("StartMessageMoveTask" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-11-05" :: Prelude.ByteString),
        "DestinationArn" Data.=: destinationArn,
        "MaxNumberOfMessagesPerSecond"
          Data.=: maxNumberOfMessagesPerSecond,
        "SourceArn" Data.=: sourceArn
      ]

-- | /See:/ 'newStartMessageMoveTaskResponse' smart constructor.
data StartMessageMoveTaskResponse = StartMessageMoveTaskResponse'
  { -- | An identifier associated with a message movement task. You can use this
    -- identifier to cancel a specified message movement task using the
    -- @CancelMessageMoveTask@ action.
    taskHandle :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartMessageMoveTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskHandle', 'startMessageMoveTaskResponse_taskHandle' - An identifier associated with a message movement task. You can use this
-- identifier to cancel a specified message movement task using the
-- @CancelMessageMoveTask@ action.
--
-- 'httpStatus', 'startMessageMoveTaskResponse_httpStatus' - The response's http status code.
newStartMessageMoveTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartMessageMoveTaskResponse
newStartMessageMoveTaskResponse pHttpStatus_ =
  StartMessageMoveTaskResponse'
    { taskHandle =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An identifier associated with a message movement task. You can use this
-- identifier to cancel a specified message movement task using the
-- @CancelMessageMoveTask@ action.
startMessageMoveTaskResponse_taskHandle :: Lens.Lens' StartMessageMoveTaskResponse (Prelude.Maybe Prelude.Text)
startMessageMoveTaskResponse_taskHandle = Lens.lens (\StartMessageMoveTaskResponse' {taskHandle} -> taskHandle) (\s@StartMessageMoveTaskResponse' {} a -> s {taskHandle = a} :: StartMessageMoveTaskResponse)

-- | The response's http status code.
startMessageMoveTaskResponse_httpStatus :: Lens.Lens' StartMessageMoveTaskResponse Prelude.Int
startMessageMoveTaskResponse_httpStatus = Lens.lens (\StartMessageMoveTaskResponse' {httpStatus} -> httpStatus) (\s@StartMessageMoveTaskResponse' {} a -> s {httpStatus = a} :: StartMessageMoveTaskResponse)

instance Prelude.NFData StartMessageMoveTaskResponse where
  rnf StartMessageMoveTaskResponse' {..} =
    Prelude.rnf taskHandle
      `Prelude.seq` Prelude.rnf httpStatus
