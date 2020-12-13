{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.DeleteObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an object and its associated attributes. Only objects with no children and no parents can be deleted. The maximum number of attributes that can be deleted during an object deletion is 30. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/limits.html Amazon Cloud Directory Limits> .
module Network.AWS.CloudDirectory.DeleteObject
  ( -- * Creating a request
    DeleteObject (..),
    mkDeleteObject,

    -- ** Request lenses
    dofDirectoryARN,
    dofObjectReference,

    -- * Destructuring the response
    DeleteObjectResponse (..),
    mkDeleteObjectResponse,

    -- ** Response lenses
    dorsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteObject' smart constructor.
data DeleteObject = DeleteObject'
  { -- | The Amazon Resource Name (ARN) that is associated with the 'Directory' where the object resides. For more information, see 'arns' .
    directoryARN :: Lude.Text,
    -- | A reference that identifies the object.
    objectReference :: ObjectReference
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteObject' with the minimum fields required to make a request.
--
-- * 'directoryARN' - The Amazon Resource Name (ARN) that is associated with the 'Directory' where the object resides. For more information, see 'arns' .
-- * 'objectReference' - A reference that identifies the object.
mkDeleteObject ::
  -- | 'directoryARN'
  Lude.Text ->
  -- | 'objectReference'
  ObjectReference ->
  DeleteObject
mkDeleteObject pDirectoryARN_ pObjectReference_ =
  DeleteObject'
    { directoryARN = pDirectoryARN_,
      objectReference = pObjectReference_
    }

-- | The Amazon Resource Name (ARN) that is associated with the 'Directory' where the object resides. For more information, see 'arns' .
--
-- /Note:/ Consider using 'directoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dofDirectoryARN :: Lens.Lens' DeleteObject Lude.Text
dofDirectoryARN = Lens.lens (directoryARN :: DeleteObject -> Lude.Text) (\s a -> s {directoryARN = a} :: DeleteObject)
{-# DEPRECATED dofDirectoryARN "Use generic-lens or generic-optics with 'directoryARN' instead." #-}

-- | A reference that identifies the object.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dofObjectReference :: Lens.Lens' DeleteObject ObjectReference
dofObjectReference = Lens.lens (objectReference :: DeleteObject -> ObjectReference) (\s a -> s {objectReference = a} :: DeleteObject)
{-# DEPRECATED dofObjectReference "Use generic-lens or generic-optics with 'objectReference' instead." #-}

instance Lude.AWSRequest DeleteObject where
  type Rs DeleteObject = DeleteObjectResponse
  request = Req.putJSON cloudDirectoryService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteObjectResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteObject where
  toHeaders DeleteObject' {..} =
    Lude.mconcat ["x-amz-data-partition" Lude.=# directoryARN]

instance Lude.ToJSON DeleteObject where
  toJSON DeleteObject' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("ObjectReference" Lude..= objectReference)]
      )

instance Lude.ToPath DeleteObject where
  toPath =
    Lude.const "/amazonclouddirectory/2017-01-11/object/delete"

instance Lude.ToQuery DeleteObject where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteObjectResponse' smart constructor.
newtype DeleteObjectResponse = DeleteObjectResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
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
