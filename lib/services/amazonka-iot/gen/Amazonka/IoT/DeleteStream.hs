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
-- Module      : Amazonka.IoT.DeleteStream
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a stream.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions DeleteStream>
-- action.
module Amazonka.IoT.DeleteStream
  ( -- * Creating a Request
    DeleteStream (..),
    newDeleteStream,

    -- * Request Lenses
    deleteStream_streamId,

    -- * Destructuring the Response
    DeleteStreamResponse (..),
    newDeleteStreamResponse,

    -- * Response Lenses
    deleteStreamResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteStream' smart constructor.
data DeleteStream = DeleteStream'
  { -- | The stream ID.
    streamId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamId', 'deleteStream_streamId' - The stream ID.
newDeleteStream ::
  -- | 'streamId'
  Prelude.Text ->
  DeleteStream
newDeleteStream pStreamId_ =
  DeleteStream' {streamId = pStreamId_}

-- | The stream ID.
deleteStream_streamId :: Lens.Lens' DeleteStream Prelude.Text
deleteStream_streamId = Lens.lens (\DeleteStream' {streamId} -> streamId) (\s@DeleteStream' {} a -> s {streamId = a} :: DeleteStream)

instance Core.AWSRequest DeleteStream where
  type AWSResponse DeleteStream = DeleteStreamResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteStreamResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteStream where
  hashWithSalt _salt DeleteStream' {..} =
    _salt `Prelude.hashWithSalt` streamId

instance Prelude.NFData DeleteStream where
  rnf DeleteStream' {..} = Prelude.rnf streamId

instance Data.ToHeaders DeleteStream where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteStream where
  toPath DeleteStream' {..} =
    Prelude.mconcat ["/streams/", Data.toBS streamId]

instance Data.ToQuery DeleteStream where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteStreamResponse' smart constructor.
data DeleteStreamResponse = DeleteStreamResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteStreamResponse_httpStatus' - The response's http status code.
newDeleteStreamResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteStreamResponse
newDeleteStreamResponse pHttpStatus_ =
  DeleteStreamResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteStreamResponse_httpStatus :: Lens.Lens' DeleteStreamResponse Prelude.Int
deleteStreamResponse_httpStatus = Lens.lens (\DeleteStreamResponse' {httpStatus} -> httpStatus) (\s@DeleteStreamResponse' {} a -> s {httpStatus = a} :: DeleteStreamResponse)

instance Prelude.NFData DeleteStreamResponse where
  rnf DeleteStreamResponse' {..} =
    Prelude.rnf httpStatus
