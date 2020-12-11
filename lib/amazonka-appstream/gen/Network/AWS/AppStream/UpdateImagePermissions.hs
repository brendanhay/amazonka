{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.UpdateImagePermissions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates permissions for the specified private image.
module Network.AWS.AppStream.UpdateImagePermissions
  ( -- * Creating a request
    UpdateImagePermissions (..),
    mkUpdateImagePermissions,

    -- ** Request lenses
    uipName,
    uipSharedAccountId,
    uipImagePermissions,

    -- * Destructuring the response
    UpdateImagePermissionsResponse (..),
    mkUpdateImagePermissionsResponse,

    -- ** Response lenses
    uiprsResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateImagePermissions' smart constructor.
data UpdateImagePermissions = UpdateImagePermissions'
  { name ::
      Lude.Text,
    sharedAccountId :: Lude.Text,
    imagePermissions :: ImagePermissions
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateImagePermissions' with the minimum fields required to make a request.
--
-- * 'imagePermissions' - The permissions for the image.
-- * 'name' - The name of the private image.
-- * 'sharedAccountId' - The 12-digit identifier of the AWS account for which you want add or update image permissions.
mkUpdateImagePermissions ::
  -- | 'name'
  Lude.Text ->
  -- | 'sharedAccountId'
  Lude.Text ->
  -- | 'imagePermissions'
  ImagePermissions ->
  UpdateImagePermissions
mkUpdateImagePermissions
  pName_
  pSharedAccountId_
  pImagePermissions_ =
    UpdateImagePermissions'
      { name = pName_,
        sharedAccountId = pSharedAccountId_,
        imagePermissions = pImagePermissions_
      }

-- | The name of the private image.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipName :: Lens.Lens' UpdateImagePermissions Lude.Text
uipName = Lens.lens (name :: UpdateImagePermissions -> Lude.Text) (\s a -> s {name = a} :: UpdateImagePermissions)
{-# DEPRECATED uipName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The 12-digit identifier of the AWS account for which you want add or update image permissions.
--
-- /Note:/ Consider using 'sharedAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipSharedAccountId :: Lens.Lens' UpdateImagePermissions Lude.Text
uipSharedAccountId = Lens.lens (sharedAccountId :: UpdateImagePermissions -> Lude.Text) (\s a -> s {sharedAccountId = a} :: UpdateImagePermissions)
{-# DEPRECATED uipSharedAccountId "Use generic-lens or generic-optics with 'sharedAccountId' instead." #-}

-- | The permissions for the image.
--
-- /Note:/ Consider using 'imagePermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipImagePermissions :: Lens.Lens' UpdateImagePermissions ImagePermissions
uipImagePermissions = Lens.lens (imagePermissions :: UpdateImagePermissions -> ImagePermissions) (\s a -> s {imagePermissions = a} :: UpdateImagePermissions)
{-# DEPRECATED uipImagePermissions "Use generic-lens or generic-optics with 'imagePermissions' instead." #-}

instance Lude.AWSRequest UpdateImagePermissions where
  type Rs UpdateImagePermissions = UpdateImagePermissionsResponse
  request = Req.postJSON appStreamService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateImagePermissionsResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateImagePermissions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "PhotonAdminProxyService.UpdateImagePermissions" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateImagePermissions where
  toJSON UpdateImagePermissions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Name" Lude..= name),
            Lude.Just ("SharedAccountId" Lude..= sharedAccountId),
            Lude.Just ("ImagePermissions" Lude..= imagePermissions)
          ]
      )

instance Lude.ToPath UpdateImagePermissions where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateImagePermissions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateImagePermissionsResponse' smart constructor.
newtype UpdateImagePermissionsResponse = UpdateImagePermissionsResponse'
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

-- | Creates a value of 'UpdateImagePermissionsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateImagePermissionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateImagePermissionsResponse
mkUpdateImagePermissionsResponse pResponseStatus_ =
  UpdateImagePermissionsResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiprsResponseStatus :: Lens.Lens' UpdateImagePermissionsResponse Lude.Int
uiprsResponseStatus = Lens.lens (responseStatus :: UpdateImagePermissionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateImagePermissionsResponse)
{-# DEPRECATED uiprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
