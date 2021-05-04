{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteQueue' smart constructor.
data DeleteQueue = DeleteQueue'
  { -- | The name of the queue that you want to delete.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeleteQueue
newDeleteQueue pName_ = DeleteQueue' {name = pName_}

-- | The name of the queue that you want to delete.
deleteQueue_name :: Lens.Lens' DeleteQueue Prelude.Text
deleteQueue_name = Lens.lens (\DeleteQueue' {name} -> name) (\s@DeleteQueue' {} a -> s {name = a} :: DeleteQueue)

instance Prelude.AWSRequest DeleteQueue where
  type Rs DeleteQueue = DeleteQueueResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteQueueResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteQueue

instance Prelude.NFData DeleteQueue

instance Prelude.ToHeaders DeleteQueue where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath DeleteQueue where
  toPath DeleteQueue' {..} =
    Prelude.mconcat
      ["/2017-08-29/queues/", Prelude.toBS name]

instance Prelude.ToQuery DeleteQueue where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteQueueResponse' smart constructor.
data DeleteQueueResponse = DeleteQueueResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DeleteQueueResponse
newDeleteQueueResponse pHttpStatus_ =
  DeleteQueueResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteQueueResponse_httpStatus :: Lens.Lens' DeleteQueueResponse Prelude.Int
deleteQueueResponse_httpStatus = Lens.lens (\DeleteQueueResponse' {httpStatus} -> httpStatus) (\s@DeleteQueueResponse' {} a -> s {httpStatus = a} :: DeleteQueueResponse)

instance Prelude.NFData DeleteQueueResponse
