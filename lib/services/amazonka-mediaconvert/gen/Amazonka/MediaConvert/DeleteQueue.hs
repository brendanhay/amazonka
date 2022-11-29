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
-- Module      : Amazonka.MediaConvert.DeleteQueue
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently delete a queue you have created.
module Amazonka.MediaConvert.DeleteQueue
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaConvert.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteQueue' smart constructor.
data DeleteQueue = DeleteQueue'
  { -- | The name of the queue that you want to delete.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DeleteQueue where
  type AWSResponse DeleteQueue = DeleteQueueResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteQueueResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteQueue where
  hashWithSalt _salt DeleteQueue' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteQueue where
  rnf DeleteQueue' {..} = Prelude.rnf name

instance Core.ToHeaders DeleteQueue where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteQueue where
  toPath DeleteQueue' {..} =
    Prelude.mconcat
      ["/2017-08-29/queues/", Core.toBS name]

instance Core.ToQuery DeleteQueue where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteQueueResponse' smart constructor.
data DeleteQueueResponse = DeleteQueueResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData DeleteQueueResponse where
  rnf DeleteQueueResponse' {..} = Prelude.rnf httpStatus
