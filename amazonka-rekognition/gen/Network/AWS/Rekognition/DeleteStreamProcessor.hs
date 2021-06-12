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
-- Module      : Network.AWS.Rekognition.DeleteStreamProcessor
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the stream processor identified by @Name@. You assign the value
-- for @Name@ when you create the stream processor with
-- CreateStreamProcessor. You might not be able to use the same name for a
-- stream processor for a few seconds after calling
-- @DeleteStreamProcessor@.
module Network.AWS.Rekognition.DeleteStreamProcessor
  ( -- * Creating a Request
    DeleteStreamProcessor (..),
    newDeleteStreamProcessor,

    -- * Request Lenses
    deleteStreamProcessor_name,

    -- * Destructuring the Response
    DeleteStreamProcessorResponse (..),
    newDeleteStreamProcessorResponse,

    -- * Response Lenses
    deleteStreamProcessorResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteStreamProcessor' smart constructor.
data DeleteStreamProcessor = DeleteStreamProcessor'
  { -- | The name of the stream processor you want to delete.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteStreamProcessor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteStreamProcessor_name' - The name of the stream processor you want to delete.
newDeleteStreamProcessor ::
  -- | 'name'
  Core.Text ->
  DeleteStreamProcessor
newDeleteStreamProcessor pName_ =
  DeleteStreamProcessor' {name = pName_}

-- | The name of the stream processor you want to delete.
deleteStreamProcessor_name :: Lens.Lens' DeleteStreamProcessor Core.Text
deleteStreamProcessor_name = Lens.lens (\DeleteStreamProcessor' {name} -> name) (\s@DeleteStreamProcessor' {} a -> s {name = a} :: DeleteStreamProcessor)

instance Core.AWSRequest DeleteStreamProcessor where
  type
    AWSResponse DeleteStreamProcessor =
      DeleteStreamProcessorResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteStreamProcessorResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteStreamProcessor

instance Core.NFData DeleteStreamProcessor

instance Core.ToHeaders DeleteStreamProcessor where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "RekognitionService.DeleteStreamProcessor" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteStreamProcessor where
  toJSON DeleteStreamProcessor' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.ToPath DeleteStreamProcessor where
  toPath = Core.const "/"

instance Core.ToQuery DeleteStreamProcessor where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteStreamProcessorResponse' smart constructor.
data DeleteStreamProcessorResponse = DeleteStreamProcessorResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteStreamProcessorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteStreamProcessorResponse_httpStatus' - The response's http status code.
newDeleteStreamProcessorResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteStreamProcessorResponse
newDeleteStreamProcessorResponse pHttpStatus_ =
  DeleteStreamProcessorResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteStreamProcessorResponse_httpStatus :: Lens.Lens' DeleteStreamProcessorResponse Core.Int
deleteStreamProcessorResponse_httpStatus = Lens.lens (\DeleteStreamProcessorResponse' {httpStatus} -> httpStatus) (\s@DeleteStreamProcessorResponse' {} a -> s {httpStatus = a} :: DeleteStreamProcessorResponse)

instance Core.NFData DeleteStreamProcessorResponse
