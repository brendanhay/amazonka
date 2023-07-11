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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
    deleteImagePipelineResponse_imagePipelineArn,
    deleteImagePipelineResponse_requestId,
    deleteImagePipelineResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
            Prelude.<$> (x Data..?> "imagePipelineArn")
            Prelude.<*> (x Data..?> "requestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteImagePipeline where
  hashWithSalt _salt DeleteImagePipeline' {..} =
    _salt `Prelude.hashWithSalt` imagePipelineArn

instance Prelude.NFData DeleteImagePipeline where
  rnf DeleteImagePipeline' {..} =
    Prelude.rnf imagePipelineArn

instance Data.ToHeaders DeleteImagePipeline where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteImagePipeline where
  toPath = Prelude.const "/DeleteImagePipeline"

instance Data.ToQuery DeleteImagePipeline where
  toQuery DeleteImagePipeline' {..} =
    Prelude.mconcat
      ["imagePipelineArn" Data.=: imagePipelineArn]

-- | /See:/ 'newDeleteImagePipelineResponse' smart constructor.
data DeleteImagePipelineResponse = DeleteImagePipelineResponse'
  { -- | The Amazon Resource Name (ARN) of the image pipeline that was deleted.
    imagePipelineArn :: Prelude.Maybe Prelude.Text,
    -- | The request ID that uniquely identifies this request.
    requestId :: Prelude.Maybe Prelude.Text,
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
-- 'imagePipelineArn', 'deleteImagePipelineResponse_imagePipelineArn' - The Amazon Resource Name (ARN) of the image pipeline that was deleted.
--
-- 'requestId', 'deleteImagePipelineResponse_requestId' - The request ID that uniquely identifies this request.
--
-- 'httpStatus', 'deleteImagePipelineResponse_httpStatus' - The response's http status code.
newDeleteImagePipelineResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteImagePipelineResponse
newDeleteImagePipelineResponse pHttpStatus_ =
  DeleteImagePipelineResponse'
    { imagePipelineArn =
        Prelude.Nothing,
      requestId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the image pipeline that was deleted.
deleteImagePipelineResponse_imagePipelineArn :: Lens.Lens' DeleteImagePipelineResponse (Prelude.Maybe Prelude.Text)
deleteImagePipelineResponse_imagePipelineArn = Lens.lens (\DeleteImagePipelineResponse' {imagePipelineArn} -> imagePipelineArn) (\s@DeleteImagePipelineResponse' {} a -> s {imagePipelineArn = a} :: DeleteImagePipelineResponse)

-- | The request ID that uniquely identifies this request.
deleteImagePipelineResponse_requestId :: Lens.Lens' DeleteImagePipelineResponse (Prelude.Maybe Prelude.Text)
deleteImagePipelineResponse_requestId = Lens.lens (\DeleteImagePipelineResponse' {requestId} -> requestId) (\s@DeleteImagePipelineResponse' {} a -> s {requestId = a} :: DeleteImagePipelineResponse)

-- | The response's http status code.
deleteImagePipelineResponse_httpStatus :: Lens.Lens' DeleteImagePipelineResponse Prelude.Int
deleteImagePipelineResponse_httpStatus = Lens.lens (\DeleteImagePipelineResponse' {httpStatus} -> httpStatus) (\s@DeleteImagePipelineResponse' {} a -> s {httpStatus = a} :: DeleteImagePipelineResponse)

instance Prelude.NFData DeleteImagePipelineResponse where
  rnf DeleteImagePipelineResponse' {..} =
    Prelude.rnf imagePipelineArn
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf httpStatus
