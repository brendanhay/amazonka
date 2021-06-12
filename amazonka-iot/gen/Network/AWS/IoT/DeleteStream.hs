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
-- Module      : Network.AWS.IoT.DeleteStream
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a stream.
module Network.AWS.IoT.DeleteStream
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

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteStream' smart constructor.
data DeleteStream = DeleteStream'
  { -- | The stream ID.
    streamId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DeleteStream
newDeleteStream pStreamId_ =
  DeleteStream' {streamId = pStreamId_}

-- | The stream ID.
deleteStream_streamId :: Lens.Lens' DeleteStream Core.Text
deleteStream_streamId = Lens.lens (\DeleteStream' {streamId} -> streamId) (\s@DeleteStream' {} a -> s {streamId = a} :: DeleteStream)

instance Core.AWSRequest DeleteStream where
  type AWSResponse DeleteStream = DeleteStreamResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteStreamResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteStream

instance Core.NFData DeleteStream

instance Core.ToHeaders DeleteStream where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteStream where
  toPath DeleteStream' {..} =
    Core.mconcat ["/streams/", Core.toBS streamId]

instance Core.ToQuery DeleteStream where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteStreamResponse' smart constructor.
data DeleteStreamResponse = DeleteStreamResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DeleteStreamResponse
newDeleteStreamResponse pHttpStatus_ =
  DeleteStreamResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteStreamResponse_httpStatus :: Lens.Lens' DeleteStreamResponse Core.Int
deleteStreamResponse_httpStatus = Lens.lens (\DeleteStreamResponse' {httpStatus} -> httpStatus) (\s@DeleteStreamResponse' {} a -> s {httpStatus = a} :: DeleteStreamResponse)

instance Core.NFData DeleteStreamResponse
