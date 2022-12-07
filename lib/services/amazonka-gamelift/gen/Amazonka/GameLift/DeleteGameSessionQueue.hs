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
-- Module      : Amazonka.GameLift.DeleteGameSessionQueue
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a game session queue. Once a queue is successfully deleted,
-- unfulfilled
-- <https://docs.aws.amazon.com/gamelift/latest/apireference/API_StartGameSessionPlacement.html StartGameSessionPlacement>
-- requests that reference the queue will fail. To delete a queue, specify
-- the queue name.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/queues-intro.html Using Multi-Region Queues>
--
-- __Related actions__
--
-- <https://docs.aws.amazon.com/gamelift/latest/apireference/API_CreateGameSessionQueue.html CreateGameSessionQueue>
-- |
-- <https://docs.aws.amazon.com/gamelift/latest/apireference/API_DescribeGameSessionQueues.html DescribeGameSessionQueues>
-- |
-- <https://docs.aws.amazon.com/gamelift/latest/apireference/API_UpdateGameSessionQueue.html UpdateGameSessionQueue>
-- |
-- <https://docs.aws.amazon.com/gamelift/latest/apireference/API_DeleteGameSessionQueue.html DeleteGameSessionQueue>
-- |
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
module Amazonka.GameLift.DeleteGameSessionQueue
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newDeleteGameSessionQueue' smart constructor.
data DeleteGameSessionQueue = DeleteGameSessionQueue'
  { -- | A descriptive label that is associated with game session queue. Queue
    -- names must be unique within each Region. You can use either the queue ID
    -- or ARN value.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DeleteGameSessionQueue
newDeleteGameSessionQueue pName_ =
  DeleteGameSessionQueue' {name = pName_}

-- | A descriptive label that is associated with game session queue. Queue
-- names must be unique within each Region. You can use either the queue ID
-- or ARN value.
deleteGameSessionQueue_name :: Lens.Lens' DeleteGameSessionQueue Prelude.Text
deleteGameSessionQueue_name = Lens.lens (\DeleteGameSessionQueue' {name} -> name) (\s@DeleteGameSessionQueue' {} a -> s {name = a} :: DeleteGameSessionQueue)

instance Core.AWSRequest DeleteGameSessionQueue where
  type
    AWSResponse DeleteGameSessionQueue =
      DeleteGameSessionQueueResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteGameSessionQueueResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteGameSessionQueue where
  hashWithSalt _salt DeleteGameSessionQueue' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteGameSessionQueue where
  rnf DeleteGameSessionQueue' {..} = Prelude.rnf name

instance Data.ToHeaders DeleteGameSessionQueue where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GameLift.DeleteGameSessionQueue" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteGameSessionQueue where
  toJSON DeleteGameSessionQueue' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Data..= name)]
      )

instance Data.ToPath DeleteGameSessionQueue where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteGameSessionQueue where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteGameSessionQueueResponse' smart constructor.
data DeleteGameSessionQueueResponse = DeleteGameSessionQueueResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeleteGameSessionQueueResponse
newDeleteGameSessionQueueResponse pHttpStatus_ =
  DeleteGameSessionQueueResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteGameSessionQueueResponse_httpStatus :: Lens.Lens' DeleteGameSessionQueueResponse Prelude.Int
deleteGameSessionQueueResponse_httpStatus = Lens.lens (\DeleteGameSessionQueueResponse' {httpStatus} -> httpStatus) (\s@DeleteGameSessionQueueResponse' {} a -> s {httpStatus = a} :: DeleteGameSessionQueueResponse)

instance
  Prelude.NFData
    DeleteGameSessionQueueResponse
  where
  rnf DeleteGameSessionQueueResponse' {..} =
    Prelude.rnf httpStatus
