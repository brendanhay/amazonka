{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStoreData.DeleteObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an object at the specified path.
module Network.AWS.MediaStoreData.DeleteObject
  ( -- * Creating a request
    DeleteObject (..),
    mkDeleteObject,

    -- ** Request lenses
    doPath,

    -- * Destructuring the response
    DeleteObjectResponse (..),
    mkDeleteObjectResponse,

    -- ** Response lenses
    dorsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaStoreData.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteObject' smart constructor.
newtype DeleteObject = DeleteObject' {path :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteObject' with the minimum fields required to make a request.
--
-- * 'path' - The path (including the file name) where the object is stored in the container. Format: <folder name>/<folder name>/<file name>
mkDeleteObject ::
  -- | 'path'
  Lude.Text ->
  DeleteObject
mkDeleteObject pPath_ = DeleteObject' {path = pPath_}

-- | The path (including the file name) where the object is stored in the container. Format: <folder name>/<folder name>/<file name>
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doPath :: Lens.Lens' DeleteObject Lude.Text
doPath = Lens.lens (path :: DeleteObject -> Lude.Text) (\s a -> s {path = a} :: DeleteObject)
{-# DEPRECATED doPath "Use generic-lens or generic-optics with 'path' instead." #-}

instance Lude.AWSRequest DeleteObject where
  type Rs DeleteObject = DeleteObjectResponse
  request = Req.delete mediaStoreDataService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteObjectResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteObject where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteObject where
  toPath DeleteObject' {..} = Lude.mconcat ["/", Lude.toBS path]

instance Lude.ToQuery DeleteObject where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteObjectResponse' smart constructor.
newtype DeleteObjectResponse = DeleteObjectResponse'
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

-- | Creates a value of 'DeleteObjectResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteObjectResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteObjectResponse
mkDeleteObjectResponse pResponseStatus_ =
  DeleteObjectResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorsResponseStatus :: Lens.Lens' DeleteObjectResponse Lude.Int
dorsResponseStatus = Lens.lens (responseStatus :: DeleteObjectResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteObjectResponse)
{-# DEPRECATED dorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
