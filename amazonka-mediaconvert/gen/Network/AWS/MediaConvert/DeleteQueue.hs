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
-- Module      : Network.AWS.MediaConvert.DeleteQueue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently delete a queue you have created.
module Network.AWS.MediaConvert.DeleteQueue
  ( -- * Creating a Request
    DeleteQueue (..),
    newDeleteQueue,

    -- * Request Lenses
    deleteQueue_name,

    -- * Destructuring the Response
    DeleteQueueResponse (..),
    newDeleteQueueResponse,

    -- * Response Lenses
    deleteQueueResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteQueue' smart constructor.
data DeleteQueue = DeleteQueue'
  { -- | The name of the queue that you want to delete.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteQueue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteQueue_name' - The name of the queue that you want to delete.
newDeleteQueue ::
  -- | 'name'
  Core.Text ->
  DeleteQueue
newDeleteQueue pName_ = DeleteQueue' {name = pName_}

-- | The name of the queue that you want to delete.
deleteQueue_name :: Lens.Lens' DeleteQueue Core.Text
deleteQueue_name = Lens.lens (\DeleteQueue' {name} -> name) (\s@DeleteQueue' {} a -> s {name = a} :: DeleteQueue)

instance Core.AWSRequest DeleteQueue where
  type AWSResponse DeleteQueue = DeleteQueueResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteQueueResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteQueue

instance Core.NFData DeleteQueue

instance Core.ToHeaders DeleteQueue where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DeleteQueue where
  toPath DeleteQueue' {..} =
    Core.mconcat
      ["/2017-08-29/queues/", Core.toBS name]

instance Core.ToQuery DeleteQueue where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteQueueResponse' smart constructor.
data DeleteQueueResponse = DeleteQueueResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteQueueResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteQueueResponse_httpStatus' - The response's http status code.
newDeleteQueueResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteQueueResponse
newDeleteQueueResponse pHttpStatus_ =
  DeleteQueueResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteQueueResponse_httpStatus :: Lens.Lens' DeleteQueueResponse Core.Int
deleteQueueResponse_httpStatus = Lens.lens (\DeleteQueueResponse' {httpStatus} -> httpStatus) (\s@DeleteQueueResponse' {} a -> s {httpStatus = a} :: DeleteQueueResponse)

instance Core.NFData DeleteQueueResponse
