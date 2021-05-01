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
-- Module      : Network.AWS.WorkSpaces.DeleteWorkspaceImage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified image from your account. To delete an image, you
-- must first delete any bundles that are associated with the image and
-- unshare the image if it is shared with other accounts.
module Network.AWS.WorkSpaces.DeleteWorkspaceImage
  ( -- * Creating a Request
    DeleteWorkspaceImage (..),
    newDeleteWorkspaceImage,

    -- * Request Lenses
    deleteWorkspaceImage_imageId,

    -- * Destructuring the Response
    DeleteWorkspaceImageResponse (..),
    newDeleteWorkspaceImageResponse,

    -- * Response Lenses
    deleteWorkspaceImageResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newDeleteWorkspaceImage' smart constructor.
data DeleteWorkspaceImage = DeleteWorkspaceImage'
  { -- | The identifier of the image.
    imageId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteWorkspaceImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageId', 'deleteWorkspaceImage_imageId' - The identifier of the image.
newDeleteWorkspaceImage ::
  -- | 'imageId'
  Prelude.Text ->
  DeleteWorkspaceImage
newDeleteWorkspaceImage pImageId_ =
  DeleteWorkspaceImage' {imageId = pImageId_}

-- | The identifier of the image.
deleteWorkspaceImage_imageId :: Lens.Lens' DeleteWorkspaceImage Prelude.Text
deleteWorkspaceImage_imageId = Lens.lens (\DeleteWorkspaceImage' {imageId} -> imageId) (\s@DeleteWorkspaceImage' {} a -> s {imageId = a} :: DeleteWorkspaceImage)

instance Prelude.AWSRequest DeleteWorkspaceImage where
  type
    Rs DeleteWorkspaceImage =
      DeleteWorkspaceImageResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteWorkspaceImageResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteWorkspaceImage

instance Prelude.NFData DeleteWorkspaceImage

instance Prelude.ToHeaders DeleteWorkspaceImage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "WorkspacesService.DeleteWorkspaceImage" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteWorkspaceImage where
  toJSON DeleteWorkspaceImage' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("ImageId" Prelude..= imageId)]
      )

instance Prelude.ToPath DeleteWorkspaceImage where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteWorkspaceImage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteWorkspaceImageResponse' smart constructor.
data DeleteWorkspaceImageResponse = DeleteWorkspaceImageResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteWorkspaceImageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteWorkspaceImageResponse_httpStatus' - The response's http status code.
newDeleteWorkspaceImageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteWorkspaceImageResponse
newDeleteWorkspaceImageResponse pHttpStatus_ =
  DeleteWorkspaceImageResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteWorkspaceImageResponse_httpStatus :: Lens.Lens' DeleteWorkspaceImageResponse Prelude.Int
deleteWorkspaceImageResponse_httpStatus = Lens.lens (\DeleteWorkspaceImageResponse' {httpStatus} -> httpStatus) (\s@DeleteWorkspaceImageResponse' {} a -> s {httpStatus = a} :: DeleteWorkspaceImageResponse)

instance Prelude.NFData DeleteWorkspaceImageResponse
