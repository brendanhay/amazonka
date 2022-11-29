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
-- Module      : Amazonka.ImageBuilder.DeleteImagePipeline
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an image pipeline.
module Amazonka.ImageBuilder.DeleteImagePipeline
  ( -- * Creating a Request
    DeleteImagePipeline (..),
    newDeleteImagePipeline,

    -- * Request Lenses
    deleteImagePipeline_imagePipelineArn,

    -- * Destructuring the Response
    DeleteImagePipelineResponse (..),
    newDeleteImagePipelineResponse,

    -- * Response Lenses
    deleteImagePipelineResponse_requestId,
    deleteImagePipelineResponse_imagePipelineArn,
    deleteImagePipelineResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ImageBuilder.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteImagePipeline' smart constructor.
data DeleteImagePipeline = DeleteImagePipeline'
  { -- | The Amazon Resource Name (ARN) of the image pipeline to delete.
    imagePipelineArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteImagePipeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imagePipelineArn', 'deleteImagePipeline_imagePipelineArn' - The Amazon Resource Name (ARN) of the image pipeline to delete.
newDeleteImagePipeline ::
  -- | 'imagePipelineArn'
  Prelude.Text ->
  DeleteImagePipeline
newDeleteImagePipeline pImagePipelineArn_ =
  DeleteImagePipeline'
    { imagePipelineArn =
        pImagePipelineArn_
    }

-- | The Amazon Resource Name (ARN) of the image pipeline to delete.
deleteImagePipeline_imagePipelineArn :: Lens.Lens' DeleteImagePipeline Prelude.Text
deleteImagePipeline_imagePipelineArn = Lens.lens (\DeleteImagePipeline' {imagePipelineArn} -> imagePipelineArn) (\s@DeleteImagePipeline' {} a -> s {imagePipelineArn = a} :: DeleteImagePipeline)

instance Core.AWSRequest DeleteImagePipeline where
  type
    AWSResponse DeleteImagePipeline =
      DeleteImagePipelineResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteImagePipelineResponse'
            Prelude.<$> (x Core..?> "requestId")
            Prelude.<*> (x Core..?> "imagePipelineArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteImagePipeline where
  hashWithSalt _salt DeleteImagePipeline' {..} =
    _salt `Prelude.hashWithSalt` imagePipelineArn

instance Prelude.NFData DeleteImagePipeline where
  rnf DeleteImagePipeline' {..} =
    Prelude.rnf imagePipelineArn

instance Core.ToHeaders DeleteImagePipeline where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteImagePipeline where
  toPath = Prelude.const "/DeleteImagePipeline"

instance Core.ToQuery DeleteImagePipeline where
  toQuery DeleteImagePipeline' {..} =
    Prelude.mconcat
      ["imagePipelineArn" Core.=: imagePipelineArn]

-- | /See:/ 'newDeleteImagePipelineResponse' smart constructor.
data DeleteImagePipelineResponse = DeleteImagePipelineResponse'
  { -- | The request ID that uniquely identifies this request.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the image pipeline that was deleted.
    imagePipelineArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteImagePipelineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'deleteImagePipelineResponse_requestId' - The request ID that uniquely identifies this request.
--
-- 'imagePipelineArn', 'deleteImagePipelineResponse_imagePipelineArn' - The Amazon Resource Name (ARN) of the image pipeline that was deleted.
--
-- 'httpStatus', 'deleteImagePipelineResponse_httpStatus' - The response's http status code.
newDeleteImagePipelineResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteImagePipelineResponse
newDeleteImagePipelineResponse pHttpStatus_ =
  DeleteImagePipelineResponse'
    { requestId =
        Prelude.Nothing,
      imagePipelineArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The request ID that uniquely identifies this request.
deleteImagePipelineResponse_requestId :: Lens.Lens' DeleteImagePipelineResponse (Prelude.Maybe Prelude.Text)
deleteImagePipelineResponse_requestId = Lens.lens (\DeleteImagePipelineResponse' {requestId} -> requestId) (\s@DeleteImagePipelineResponse' {} a -> s {requestId = a} :: DeleteImagePipelineResponse)

-- | The Amazon Resource Name (ARN) of the image pipeline that was deleted.
deleteImagePipelineResponse_imagePipelineArn :: Lens.Lens' DeleteImagePipelineResponse (Prelude.Maybe Prelude.Text)
deleteImagePipelineResponse_imagePipelineArn = Lens.lens (\DeleteImagePipelineResponse' {imagePipelineArn} -> imagePipelineArn) (\s@DeleteImagePipelineResponse' {} a -> s {imagePipelineArn = a} :: DeleteImagePipelineResponse)

-- | The response's http status code.
deleteImagePipelineResponse_httpStatus :: Lens.Lens' DeleteImagePipelineResponse Prelude.Int
deleteImagePipelineResponse_httpStatus = Lens.lens (\DeleteImagePipelineResponse' {httpStatus} -> httpStatus) (\s@DeleteImagePipelineResponse' {} a -> s {httpStatus = a} :: DeleteImagePipelineResponse)

instance Prelude.NFData DeleteImagePipelineResponse where
  rnf DeleteImagePipelineResponse' {..} =
    Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf imagePipelineArn
      `Prelude.seq` Prelude.rnf httpStatus
