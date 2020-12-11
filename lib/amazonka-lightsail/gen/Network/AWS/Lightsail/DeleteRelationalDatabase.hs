{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DeleteRelationalDatabase
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a database in Amazon Lightsail.
--
-- The @delete relational database@ operation supports tag-based access control via resource tags applied to the resource identified by relationalDatabaseName. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.DeleteRelationalDatabase
  ( -- * Creating a request
    DeleteRelationalDatabase (..),
    mkDeleteRelationalDatabase,

    -- ** Request lenses
    drdSkipFinalSnapshot,
    drdFinalRelationalDatabaseSnapshotName,
    drdRelationalDatabaseName,

    -- * Destructuring the response
    DeleteRelationalDatabaseResponse (..),
    mkDeleteRelationalDatabaseResponse,

    -- ** Response lenses
    drdrsOperations,
    drdrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteRelationalDatabase' smart constructor.
data DeleteRelationalDatabase = DeleteRelationalDatabase'
  { skipFinalSnapshot ::
      Lude.Maybe Lude.Bool,
    finalRelationalDatabaseSnapshotName ::
      Lude.Maybe Lude.Text,
    relationalDatabaseName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRelationalDatabase' with the minimum fields required to make a request.
--
-- * 'finalRelationalDatabaseSnapshotName' - The name of the database snapshot created if @skip final snapshot@ is @false@ , which is the default value for that parameter.
--
-- Constraints:
--
--     * Must contain from 2 to 255 alphanumeric characters, or hyphens.
--
--
--     * The first and last character must be a letter or number.
--
--
-- * 'relationalDatabaseName' - The name of the database that you are deleting.
-- * 'skipFinalSnapshot' - Determines whether a final database snapshot is created before your database is deleted. If @true@ is specified, no database snapshot is created. If @false@ is specified, a database snapshot is created before your database is deleted.
--
-- You must specify the @final relational database snapshot name@ parameter if the @skip final snapshot@ parameter is @false@ .
-- Default: @false@
mkDeleteRelationalDatabase ::
  -- | 'relationalDatabaseName'
  Lude.Text ->
  DeleteRelationalDatabase
mkDeleteRelationalDatabase pRelationalDatabaseName_ =
  DeleteRelationalDatabase'
    { skipFinalSnapshot = Lude.Nothing,
      finalRelationalDatabaseSnapshotName = Lude.Nothing,
      relationalDatabaseName = pRelationalDatabaseName_
    }

-- | Determines whether a final database snapshot is created before your database is deleted. If @true@ is specified, no database snapshot is created. If @false@ is specified, a database snapshot is created before your database is deleted.
--
-- You must specify the @final relational database snapshot name@ parameter if the @skip final snapshot@ parameter is @false@ .
-- Default: @false@
--
-- /Note:/ Consider using 'skipFinalSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdSkipFinalSnapshot :: Lens.Lens' DeleteRelationalDatabase (Lude.Maybe Lude.Bool)
drdSkipFinalSnapshot = Lens.lens (skipFinalSnapshot :: DeleteRelationalDatabase -> Lude.Maybe Lude.Bool) (\s a -> s {skipFinalSnapshot = a} :: DeleteRelationalDatabase)
{-# DEPRECATED drdSkipFinalSnapshot "Use generic-lens or generic-optics with 'skipFinalSnapshot' instead." #-}

-- | The name of the database snapshot created if @skip final snapshot@ is @false@ , which is the default value for that parameter.
--
-- Constraints:
--
--     * Must contain from 2 to 255 alphanumeric characters, or hyphens.
--
--
--     * The first and last character must be a letter or number.
--
--
--
-- /Note:/ Consider using 'finalRelationalDatabaseSnapshotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdFinalRelationalDatabaseSnapshotName :: Lens.Lens' DeleteRelationalDatabase (Lude.Maybe Lude.Text)
drdFinalRelationalDatabaseSnapshotName = Lens.lens (finalRelationalDatabaseSnapshotName :: DeleteRelationalDatabase -> Lude.Maybe Lude.Text) (\s a -> s {finalRelationalDatabaseSnapshotName = a} :: DeleteRelationalDatabase)
{-# DEPRECATED drdFinalRelationalDatabaseSnapshotName "Use generic-lens or generic-optics with 'finalRelationalDatabaseSnapshotName' instead." #-}

-- | The name of the database that you are deleting.
--
-- /Note:/ Consider using 'relationalDatabaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdRelationalDatabaseName :: Lens.Lens' DeleteRelationalDatabase Lude.Text
drdRelationalDatabaseName = Lens.lens (relationalDatabaseName :: DeleteRelationalDatabase -> Lude.Text) (\s a -> s {relationalDatabaseName = a} :: DeleteRelationalDatabase)
{-# DEPRECATED drdRelationalDatabaseName "Use generic-lens or generic-optics with 'relationalDatabaseName' instead." #-}

instance Lude.AWSRequest DeleteRelationalDatabase where
  type Rs DeleteRelationalDatabase = DeleteRelationalDatabaseResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteRelationalDatabaseResponse'
            Lude.<$> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteRelationalDatabase where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.DeleteRelationalDatabase" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteRelationalDatabase where
  toJSON DeleteRelationalDatabase' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("skipFinalSnapshot" Lude..=) Lude.<$> skipFinalSnapshot,
            ("finalRelationalDatabaseSnapshotName" Lude..=)
              Lude.<$> finalRelationalDatabaseSnapshotName,
            Lude.Just
              ("relationalDatabaseName" Lude..= relationalDatabaseName)
          ]
      )

instance Lude.ToPath DeleteRelationalDatabase where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteRelationalDatabase where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteRelationalDatabaseResponse' smart constructor.
data DeleteRelationalDatabaseResponse = DeleteRelationalDatabaseResponse'
  { operations ::
      Lude.Maybe [Operation],
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

-- | Creates a value of 'DeleteRelationalDatabaseResponse' with the minimum fields required to make a request.
--
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkDeleteRelationalDatabaseResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteRelationalDatabaseResponse
mkDeleteRelationalDatabaseResponse pResponseStatus_ =
  DeleteRelationalDatabaseResponse'
    { operations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdrsOperations :: Lens.Lens' DeleteRelationalDatabaseResponse (Lude.Maybe [Operation])
drdrsOperations = Lens.lens (operations :: DeleteRelationalDatabaseResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: DeleteRelationalDatabaseResponse)
{-# DEPRECATED drdrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdrsResponseStatus :: Lens.Lens' DeleteRelationalDatabaseResponse Lude.Int
drdrsResponseStatus = Lens.lens (responseStatus :: DeleteRelationalDatabaseResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteRelationalDatabaseResponse)
{-# DEPRECATED drdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
