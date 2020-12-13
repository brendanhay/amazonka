{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.UpdateWorkspaceImagePermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Shares or unshares an image with one account by specifying whether that account has permission to copy the image. If the copy image permission is granted, the image is shared with that account. If the copy image permission is revoked, the image is unshared with the account. For more information about sharing images, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/share-custom-image.html Share or Unshare a Custom WorkSpaces Image> .
module Network.AWS.WorkSpaces.UpdateWorkspaceImagePermission
  ( -- * Creating a request
    UpdateWorkspaceImagePermission (..),
    mkUpdateWorkspaceImagePermission,

    -- ** Request lenses
    uwipSharedAccountId,
    uwipAllowCopyImage,
    uwipImageId,

    -- * Destructuring the response
    UpdateWorkspaceImagePermissionResponse (..),
    mkUpdateWorkspaceImagePermissionResponse,

    -- ** Response lenses
    uwiprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'mkUpdateWorkspaceImagePermission' smart constructor.
data UpdateWorkspaceImagePermission = UpdateWorkspaceImagePermission'
  { -- | The identifier of the AWS account to share or unshare the image with.
    --
    -- /Important:/ Before sharing the image, confirm that you are sharing to the correct AWS account ID.
    sharedAccountId :: Lude.Text,
    -- | The permission to copy the image. This permission can be revoked only after an image has been shared.
    allowCopyImage :: Lude.Bool,
    -- | The identifier of the image.
    imageId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateWorkspaceImagePermission' with the minimum fields required to make a request.
--
-- * 'sharedAccountId' - The identifier of the AWS account to share or unshare the image with.
--
-- /Important:/ Before sharing the image, confirm that you are sharing to the correct AWS account ID.
-- * 'allowCopyImage' - The permission to copy the image. This permission can be revoked only after an image has been shared.
-- * 'imageId' - The identifier of the image.
mkUpdateWorkspaceImagePermission ::
  -- | 'sharedAccountId'
  Lude.Text ->
  -- | 'allowCopyImage'
  Lude.Bool ->
  -- | 'imageId'
  Lude.Text ->
  UpdateWorkspaceImagePermission
mkUpdateWorkspaceImagePermission
  pSharedAccountId_
  pAllowCopyImage_
  pImageId_ =
    UpdateWorkspaceImagePermission'
      { sharedAccountId =
          pSharedAccountId_,
        allowCopyImage = pAllowCopyImage_,
        imageId = pImageId_
      }

-- | The identifier of the AWS account to share or unshare the image with.
--
-- /Important:/ Before sharing the image, confirm that you are sharing to the correct AWS account ID.
--
-- /Note:/ Consider using 'sharedAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwipSharedAccountId :: Lens.Lens' UpdateWorkspaceImagePermission Lude.Text
uwipSharedAccountId = Lens.lens (sharedAccountId :: UpdateWorkspaceImagePermission -> Lude.Text) (\s a -> s {sharedAccountId = a} :: UpdateWorkspaceImagePermission)
{-# DEPRECATED uwipSharedAccountId "Use generic-lens or generic-optics with 'sharedAccountId' instead." #-}

-- | The permission to copy the image. This permission can be revoked only after an image has been shared.
--
-- /Note:/ Consider using 'allowCopyImage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwipAllowCopyImage :: Lens.Lens' UpdateWorkspaceImagePermission Lude.Bool
uwipAllowCopyImage = Lens.lens (allowCopyImage :: UpdateWorkspaceImagePermission -> Lude.Bool) (\s a -> s {allowCopyImage = a} :: UpdateWorkspaceImagePermission)
{-# DEPRECATED uwipAllowCopyImage "Use generic-lens or generic-optics with 'allowCopyImage' instead." #-}

-- | The identifier of the image.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwipImageId :: Lens.Lens' UpdateWorkspaceImagePermission Lude.Text
uwipImageId = Lens.lens (imageId :: UpdateWorkspaceImagePermission -> Lude.Text) (\s a -> s {imageId = a} :: UpdateWorkspaceImagePermission)
{-# DEPRECATED uwipImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

instance Lude.AWSRequest UpdateWorkspaceImagePermission where
  type
    Rs UpdateWorkspaceImagePermission =
      UpdateWorkspaceImagePermissionResponse
  request = Req.postJSON workSpacesService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateWorkspaceImagePermissionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateWorkspaceImagePermission where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "WorkspacesService.UpdateWorkspaceImagePermission" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateWorkspaceImagePermission where
  toJSON UpdateWorkspaceImagePermission' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("SharedAccountId" Lude..= sharedAccountId),
            Lude.Just ("AllowCopyImage" Lude..= allowCopyImage),
            Lude.Just ("ImageId" Lude..= imageId)
          ]
      )

instance Lude.ToPath UpdateWorkspaceImagePermission where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateWorkspaceImagePermission where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateWorkspaceImagePermissionResponse' smart constructor.
newtype UpdateWorkspaceImagePermissionResponse = UpdateWorkspaceImagePermissionResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateWorkspaceImagePermissionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateWorkspaceImagePermissionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateWorkspaceImagePermissionResponse
mkUpdateWorkspaceImagePermissionResponse pResponseStatus_ =
  UpdateWorkspaceImagePermissionResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwiprsResponseStatus :: Lens.Lens' UpdateWorkspaceImagePermissionResponse Lude.Int
uwiprsResponseStatus = Lens.lens (responseStatus :: UpdateWorkspaceImagePermissionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateWorkspaceImagePermissionResponse)
{-# DEPRECATED uwiprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
