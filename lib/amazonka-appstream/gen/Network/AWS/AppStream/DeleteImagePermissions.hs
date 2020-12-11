{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.DeleteImagePermissions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes permissions for the specified private image. After you delete permissions for an image, AWS accounts to which you previously granted these permissions can no longer use the image.
module Network.AWS.AppStream.DeleteImagePermissions
  ( -- * Creating a request
    DeleteImagePermissions (..),
    mkDeleteImagePermissions,

    -- ** Request lenses
    dipName,
    dipSharedAccountId,

    -- * Destructuring the response
    DeleteImagePermissionsResponse (..),
    mkDeleteImagePermissionsResponse,

    -- ** Response lenses
    diprsResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteImagePermissions' smart constructor.
data DeleteImagePermissions = DeleteImagePermissions'
  { name ::
      Lude.Text,
    sharedAccountId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteImagePermissions' with the minimum fields required to make a request.
--
-- * 'name' - The name of the private image.
-- * 'sharedAccountId' - The 12-digit identifier of the AWS account for which to delete image permissions.
mkDeleteImagePermissions ::
  -- | 'name'
  Lude.Text ->
  -- | 'sharedAccountId'
  Lude.Text ->
  DeleteImagePermissions
mkDeleteImagePermissions pName_ pSharedAccountId_ =
  DeleteImagePermissions'
    { name = pName_,
      sharedAccountId = pSharedAccountId_
    }

-- | The name of the private image.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipName :: Lens.Lens' DeleteImagePermissions Lude.Text
dipName = Lens.lens (name :: DeleteImagePermissions -> Lude.Text) (\s a -> s {name = a} :: DeleteImagePermissions)
{-# DEPRECATED dipName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The 12-digit identifier of the AWS account for which to delete image permissions.
--
-- /Note:/ Consider using 'sharedAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipSharedAccountId :: Lens.Lens' DeleteImagePermissions Lude.Text
dipSharedAccountId = Lens.lens (sharedAccountId :: DeleteImagePermissions -> Lude.Text) (\s a -> s {sharedAccountId = a} :: DeleteImagePermissions)
{-# DEPRECATED dipSharedAccountId "Use generic-lens or generic-optics with 'sharedAccountId' instead." #-}

instance Lude.AWSRequest DeleteImagePermissions where
  type Rs DeleteImagePermissions = DeleteImagePermissionsResponse
  request = Req.postJSON appStreamService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteImagePermissionsResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteImagePermissions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "PhotonAdminProxyService.DeleteImagePermissions" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteImagePermissions where
  toJSON DeleteImagePermissions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Name" Lude..= name),
            Lude.Just ("SharedAccountId" Lude..= sharedAccountId)
          ]
      )

instance Lude.ToPath DeleteImagePermissions where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteImagePermissions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteImagePermissionsResponse' smart constructor.
newtype DeleteImagePermissionsResponse = DeleteImagePermissionsResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteImagePermissionsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteImagePermissionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteImagePermissionsResponse
mkDeleteImagePermissionsResponse pResponseStatus_ =
  DeleteImagePermissionsResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diprsResponseStatus :: Lens.Lens' DeleteImagePermissionsResponse Lude.Int
diprsResponseStatus = Lens.lens (responseStatus :: DeleteImagePermissionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteImagePermissionsResponse)
{-# DEPRECATED diprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
