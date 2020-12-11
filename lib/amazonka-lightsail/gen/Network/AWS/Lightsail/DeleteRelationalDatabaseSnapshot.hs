{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DeleteRelationalDatabaseSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a database snapshot in Amazon Lightsail.
--
-- The @delete relational database snapshot@ operation supports tag-based access control via resource tags applied to the resource identified by relationalDatabaseName. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.DeleteRelationalDatabaseSnapshot
  ( -- * Creating a request
    DeleteRelationalDatabaseSnapshot (..),
    mkDeleteRelationalDatabaseSnapshot,

    -- ** Request lenses
    drdsRelationalDatabaseSnapshotName,

    -- * Destructuring the response
    DeleteRelationalDatabaseSnapshotResponse (..),
    mkDeleteRelationalDatabaseSnapshotResponse,

    -- ** Response lenses
    drdsrsOperations,
    drdsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteRelationalDatabaseSnapshot' smart constructor.
newtype DeleteRelationalDatabaseSnapshot = DeleteRelationalDatabaseSnapshot'
  { relationalDatabaseSnapshotName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRelationalDatabaseSnapshot' with the minimum fields required to make a request.
--
-- * 'relationalDatabaseSnapshotName' - The name of the database snapshot that you are deleting.
mkDeleteRelationalDatabaseSnapshot ::
  -- | 'relationalDatabaseSnapshotName'
  Lude.Text ->
  DeleteRelationalDatabaseSnapshot
mkDeleteRelationalDatabaseSnapshot pRelationalDatabaseSnapshotName_ =
  DeleteRelationalDatabaseSnapshot'
    { relationalDatabaseSnapshotName =
        pRelationalDatabaseSnapshotName_
    }

-- | The name of the database snapshot that you are deleting.
--
-- /Note:/ Consider using 'relationalDatabaseSnapshotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdsRelationalDatabaseSnapshotName :: Lens.Lens' DeleteRelationalDatabaseSnapshot Lude.Text
drdsRelationalDatabaseSnapshotName = Lens.lens (relationalDatabaseSnapshotName :: DeleteRelationalDatabaseSnapshot -> Lude.Text) (\s a -> s {relationalDatabaseSnapshotName = a} :: DeleteRelationalDatabaseSnapshot)
{-# DEPRECATED drdsRelationalDatabaseSnapshotName "Use generic-lens or generic-optics with 'relationalDatabaseSnapshotName' instead." #-}

instance Lude.AWSRequest DeleteRelationalDatabaseSnapshot where
  type
    Rs DeleteRelationalDatabaseSnapshot =
      DeleteRelationalDatabaseSnapshotResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteRelationalDatabaseSnapshotResponse'
            Lude.<$> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteRelationalDatabaseSnapshot where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Lightsail_20161128.DeleteRelationalDatabaseSnapshot" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteRelationalDatabaseSnapshot where
  toJSON DeleteRelationalDatabaseSnapshot' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ( "relationalDatabaseSnapshotName"
                  Lude..= relationalDatabaseSnapshotName
              )
          ]
      )

instance Lude.ToPath DeleteRelationalDatabaseSnapshot where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteRelationalDatabaseSnapshot where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteRelationalDatabaseSnapshotResponse' smart constructor.
data DeleteRelationalDatabaseSnapshotResponse = DeleteRelationalDatabaseSnapshotResponse'
  { operations ::
      Lude.Maybe
        [Operation],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRelationalDatabaseSnapshotResponse' with the minimum fields required to make a request.
--
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkDeleteRelationalDatabaseSnapshotResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteRelationalDatabaseSnapshotResponse
mkDeleteRelationalDatabaseSnapshotResponse pResponseStatus_ =
  DeleteRelationalDatabaseSnapshotResponse'
    { operations =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdsrsOperations :: Lens.Lens' DeleteRelationalDatabaseSnapshotResponse (Lude.Maybe [Operation])
drdsrsOperations = Lens.lens (operations :: DeleteRelationalDatabaseSnapshotResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: DeleteRelationalDatabaseSnapshotResponse)
{-# DEPRECATED drdsrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdsrsResponseStatus :: Lens.Lens' DeleteRelationalDatabaseSnapshotResponse Lude.Int
drdsrsResponseStatus = Lens.lens (responseStatus :: DeleteRelationalDatabaseSnapshotResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteRelationalDatabaseSnapshotResponse)
{-# DEPRECATED drdsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
