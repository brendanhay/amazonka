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
-- Module      : Amazonka.ImageBuilder.DeleteImage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Image Builder image resource. This does not delete any EC2
-- AMIs or ECR container images that are created during the image build
-- process. You must clean those up separately, using the appropriate
-- Amazon EC2 or Amazon ECR console actions, or API or CLI commands.
--
-- -   To deregister an EC2 Linux AMI, see
--     <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/deregister-ami.html Deregister your Linux AMI>
--     in the //Amazon EC2 User Guide// .
--
-- -   To deregister an EC2 Windows AMI, see
--     <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/deregister-ami.html Deregister your Windows AMI>
--     in the //Amazon EC2 Windows Guide// .
--
-- -   To delete a container image from Amazon ECR, see
--     <https://docs.aws.amazon.com/AmazonECR/latest/userguide/delete_image.html Deleting an image>
--     in the /Amazon ECR User Guide/.
module Amazonka.ImageBuilder.DeleteImage
  ( -- * Creating a Request
    DeleteImage (..),
    newDeleteImage,

    -- * Request Lenses
    deleteImage_imageBuildVersionArn,

    -- * Destructuring the Response
    DeleteImageResponse (..),
    newDeleteImageResponse,

    -- * Response Lenses
    deleteImageResponse_requestId,
    deleteImageResponse_imageBuildVersionArn,
    deleteImageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteImage' smart constructor.
data DeleteImage = DeleteImage'
  { -- | The Amazon Resource Name (ARN) of the Image Builder image resource to
    -- delete.
    imageBuildVersionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageBuildVersionArn', 'deleteImage_imageBuildVersionArn' - The Amazon Resource Name (ARN) of the Image Builder image resource to
-- delete.
newDeleteImage ::
  -- | 'imageBuildVersionArn'
  Prelude.Text ->
  DeleteImage
newDeleteImage pImageBuildVersionArn_ =
  DeleteImage'
    { imageBuildVersionArn =
        pImageBuildVersionArn_
    }

-- | The Amazon Resource Name (ARN) of the Image Builder image resource to
-- delete.
deleteImage_imageBuildVersionArn :: Lens.Lens' DeleteImage Prelude.Text
deleteImage_imageBuildVersionArn = Lens.lens (\DeleteImage' {imageBuildVersionArn} -> imageBuildVersionArn) (\s@DeleteImage' {} a -> s {imageBuildVersionArn = a} :: DeleteImage)

instance Core.AWSRequest DeleteImage where
  type AWSResponse DeleteImage = DeleteImageResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteImageResponse'
            Prelude.<$> (x Data..?> "requestId")
            Prelude.<*> (x Data..?> "imageBuildVersionArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteImage where
  hashWithSalt _salt DeleteImage' {..} =
    _salt `Prelude.hashWithSalt` imageBuildVersionArn

instance Prelude.NFData DeleteImage where
  rnf DeleteImage' {..} =
    Prelude.rnf imageBuildVersionArn

instance Data.ToHeaders DeleteImage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteImage where
  toPath = Prelude.const "/DeleteImage"

instance Data.ToQuery DeleteImage where
  toQuery DeleteImage' {..} =
    Prelude.mconcat
      ["imageBuildVersionArn" Data.=: imageBuildVersionArn]

-- | /See:/ 'newDeleteImageResponse' smart constructor.
data DeleteImageResponse = DeleteImageResponse'
  { -- | The request ID that uniquely identifies this request.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Image Builder image resource that
    -- was deleted.
    imageBuildVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteImageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'deleteImageResponse_requestId' - The request ID that uniquely identifies this request.
--
-- 'imageBuildVersionArn', 'deleteImageResponse_imageBuildVersionArn' - The Amazon Resource Name (ARN) of the Image Builder image resource that
-- was deleted.
--
-- 'httpStatus', 'deleteImageResponse_httpStatus' - The response's http status code.
newDeleteImageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteImageResponse
newDeleteImageResponse pHttpStatus_ =
  DeleteImageResponse'
    { requestId = Prelude.Nothing,
      imageBuildVersionArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The request ID that uniquely identifies this request.
deleteImageResponse_requestId :: Lens.Lens' DeleteImageResponse (Prelude.Maybe Prelude.Text)
deleteImageResponse_requestId = Lens.lens (\DeleteImageResponse' {requestId} -> requestId) (\s@DeleteImageResponse' {} a -> s {requestId = a} :: DeleteImageResponse)

-- | The Amazon Resource Name (ARN) of the Image Builder image resource that
-- was deleted.
deleteImageResponse_imageBuildVersionArn :: Lens.Lens' DeleteImageResponse (Prelude.Maybe Prelude.Text)
deleteImageResponse_imageBuildVersionArn = Lens.lens (\DeleteImageResponse' {imageBuildVersionArn} -> imageBuildVersionArn) (\s@DeleteImageResponse' {} a -> s {imageBuildVersionArn = a} :: DeleteImageResponse)

-- | The response's http status code.
deleteImageResponse_httpStatus :: Lens.Lens' DeleteImageResponse Prelude.Int
deleteImageResponse_httpStatus = Lens.lens (\DeleteImageResponse' {httpStatus} -> httpStatus) (\s@DeleteImageResponse' {} a -> s {httpStatus = a} :: DeleteImageResponse)

instance Prelude.NFData DeleteImageResponse where
  rnf DeleteImageResponse' {..} =
    Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf imageBuildVersionArn
      `Prelude.seq` Prelude.rnf httpStatus
