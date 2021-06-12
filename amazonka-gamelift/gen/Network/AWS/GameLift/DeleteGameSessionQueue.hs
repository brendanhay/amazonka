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
-- Module      : Network.AWS.GameLift.DeleteGameSessionQueue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a game session queue. Once a queue is successfully deleted,
-- unfulfilled StartGameSessionPlacement requests that reference the queue
-- will fail. To delete a queue, specify the queue name.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/queues-intro.html Using Multi-Region Queues>
--
-- __Related operations__
--
-- -   CreateGameSessionQueue
--
-- -   DescribeGameSessionQueues
--
-- -   UpdateGameSessionQueue
--
-- -   DeleteGameSessionQueue
module Network.AWS.GameLift.DeleteGameSessionQueue
  ( -- * Creating a Request
    DeleteGameSessionQueue (..),
    newDeleteGameSessionQueue,

    -- * Request Lenses
    deleteGameSessionQueue_name,

    -- * Destructuring the Response
    DeleteGameSessionQueueResponse (..),
    newDeleteGameSessionQueueResponse,

    -- * Response Lenses
    deleteGameSessionQueueResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newDeleteGameSessionQueue' smart constructor.
data DeleteGameSessionQueue = DeleteGameSessionQueue'
  { -- | A descriptive label that is associated with game session queue. Queue
    -- names must be unique within each Region. You can use either the queue ID
    -- or ARN value.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteGameSessionQueue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteGameSessionQueue_name' - A descriptive label that is associated with game session queue. Queue
-- names must be unique within each Region. You can use either the queue ID
-- or ARN value.
newDeleteGameSessionQueue ::
  -- | 'name'
  Core.Text ->
  DeleteGameSessionQueue
newDeleteGameSessionQueue pName_ =
  DeleteGameSessionQueue' {name = pName_}

-- | A descriptive label that is associated with game session queue. Queue
-- names must be unique within each Region. You can use either the queue ID
-- or ARN value.
deleteGameSessionQueue_name :: Lens.Lens' DeleteGameSessionQueue Core.Text
deleteGameSessionQueue_name = Lens.lens (\DeleteGameSessionQueue' {name} -> name) (\s@DeleteGameSessionQueue' {} a -> s {name = a} :: DeleteGameSessionQueue)

instance Core.AWSRequest DeleteGameSessionQueue where
  type
    AWSResponse DeleteGameSessionQueue =
      DeleteGameSessionQueueResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteGameSessionQueueResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteGameSessionQueue

instance Core.NFData DeleteGameSessionQueue

instance Core.ToHeaders DeleteGameSessionQueue where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GameLift.DeleteGameSessionQueue" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteGameSessionQueue where
  toJSON DeleteGameSessionQueue' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.ToPath DeleteGameSessionQueue where
  toPath = Core.const "/"

instance Core.ToQuery DeleteGameSessionQueue where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteGameSessionQueueResponse' smart constructor.
data DeleteGameSessionQueueResponse = DeleteGameSessionQueueResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteGameSessionQueueResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteGameSessionQueueResponse_httpStatus' - The response's http status code.
newDeleteGameSessionQueueResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteGameSessionQueueResponse
newDeleteGameSessionQueueResponse pHttpStatus_ =
  DeleteGameSessionQueueResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteGameSessionQueueResponse_httpStatus :: Lens.Lens' DeleteGameSessionQueueResponse Core.Int
deleteGameSessionQueueResponse_httpStatus = Lens.lens (\DeleteGameSessionQueueResponse' {httpStatus} -> httpStatus) (\s@DeleteGameSessionQueueResponse' {} a -> s {httpStatus = a} :: DeleteGameSessionQueueResponse)

instance Core.NFData DeleteGameSessionQueueResponse
