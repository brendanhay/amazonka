{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.CreateRelationalDatabaseSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a snapshot of your database in Amazon Lightsail. You can use snapshots for backups, to make copies of a database, and to save data before deleting a database.
--
-- The @create relational database snapshot@ operation supports tag-based access control via request tags. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.CreateRelationalDatabaseSnapshot
  ( -- * Creating a request
    CreateRelationalDatabaseSnapshot (..),
    mkCreateRelationalDatabaseSnapshot,

    -- ** Request lenses
    crdsRelationalDatabaseSnapshotName,
    crdsRelationalDatabaseName,
    crdsTags,

    -- * Destructuring the response
    CreateRelationalDatabaseSnapshotResponse (..),
    mkCreateRelationalDatabaseSnapshotResponse,

    -- ** Response lenses
    crdsrsOperations,
    crdsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateRelationalDatabaseSnapshot' smart constructor.
data CreateRelationalDatabaseSnapshot = CreateRelationalDatabaseSnapshot'
  { -- | The name for your new database snapshot.
    --
    -- Constraints:
    --
    --     * Must contain from 2 to 255 alphanumeric characters, or hyphens.
    --
    --
    --     * The first and last character must be a letter or number.
    relationalDatabaseSnapshotName :: Lude.Text,
    -- | The name of the database on which to base your new snapshot.
    relationalDatabaseName :: Lude.Text,
    -- | The tag keys and optional values to add to the resource during create.
    --
    -- Use the @TagResource@ action to tag a resource after it's created.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateRelationalDatabaseSnapshot' with the minimum fields required to make a request.
--
-- * 'relationalDatabaseSnapshotName' - The name for your new database snapshot.
--
-- Constraints:
--
--     * Must contain from 2 to 255 alphanumeric characters, or hyphens.
--
--
--     * The first and last character must be a letter or number.
--
--
-- * 'relationalDatabaseName' - The name of the database on which to base your new snapshot.
-- * 'tags' - The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
mkCreateRelationalDatabaseSnapshot ::
  -- | 'relationalDatabaseSnapshotName'
  Lude.Text ->
  -- | 'relationalDatabaseName'
  Lude.Text ->
  CreateRelationalDatabaseSnapshot
mkCreateRelationalDatabaseSnapshot
  pRelationalDatabaseSnapshotName_
  pRelationalDatabaseName_ =
    CreateRelationalDatabaseSnapshot'
      { relationalDatabaseSnapshotName =
          pRelationalDatabaseSnapshotName_,
        relationalDatabaseName = pRelationalDatabaseName_,
        tags = Lude.Nothing
      }

-- | The name for your new database snapshot.
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
-- /Note:/ Consider using 'relationalDatabaseSnapshotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdsRelationalDatabaseSnapshotName :: Lens.Lens' CreateRelationalDatabaseSnapshot Lude.Text
crdsRelationalDatabaseSnapshotName = Lens.lens (relationalDatabaseSnapshotName :: CreateRelationalDatabaseSnapshot -> Lude.Text) (\s a -> s {relationalDatabaseSnapshotName = a} :: CreateRelationalDatabaseSnapshot)
{-# DEPRECATED crdsRelationalDatabaseSnapshotName "Use generic-lens or generic-optics with 'relationalDatabaseSnapshotName' instead." #-}

-- | The name of the database on which to base your new snapshot.
--
-- /Note:/ Consider using 'relationalDatabaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdsRelationalDatabaseName :: Lens.Lens' CreateRelationalDatabaseSnapshot Lude.Text
crdsRelationalDatabaseName = Lens.lens (relationalDatabaseName :: CreateRelationalDatabaseSnapshot -> Lude.Text) (\s a -> s {relationalDatabaseName = a} :: CreateRelationalDatabaseSnapshot)
{-# DEPRECATED crdsRelationalDatabaseName "Use generic-lens or generic-optics with 'relationalDatabaseName' instead." #-}

-- | The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdsTags :: Lens.Lens' CreateRelationalDatabaseSnapshot (Lude.Maybe [Tag])
crdsTags = Lens.lens (tags :: CreateRelationalDatabaseSnapshot -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateRelationalDatabaseSnapshot)
{-# DEPRECATED crdsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateRelationalDatabaseSnapshot where
  type
    Rs CreateRelationalDatabaseSnapshot =
      CreateRelationalDatabaseSnapshotResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateRelationalDatabaseSnapshotResponse'
            Lude.<$> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateRelationalDatabaseSnapshot where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Lightsail_20161128.CreateRelationalDatabaseSnapshot" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateRelationalDatabaseSnapshot where
  toJSON CreateRelationalDatabaseSnapshot' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ( "relationalDatabaseSnapshotName"
                  Lude..= relationalDatabaseSnapshotName
              ),
            Lude.Just
              ("relationalDatabaseName" Lude..= relationalDatabaseName),
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateRelationalDatabaseSnapshot where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateRelationalDatabaseSnapshot where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateRelationalDatabaseSnapshotResponse' smart constructor.
data CreateRelationalDatabaseSnapshotResponse = CreateRelationalDatabaseSnapshotResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Lude.Maybe [Operation],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateRelationalDatabaseSnapshotResponse' with the minimum fields required to make a request.
--
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkCreateRelationalDatabaseSnapshotResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateRelationalDatabaseSnapshotResponse
mkCreateRelationalDatabaseSnapshotResponse pResponseStatus_ =
  CreateRelationalDatabaseSnapshotResponse'
    { operations =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdsrsOperations :: Lens.Lens' CreateRelationalDatabaseSnapshotResponse (Lude.Maybe [Operation])
crdsrsOperations = Lens.lens (operations :: CreateRelationalDatabaseSnapshotResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: CreateRelationalDatabaseSnapshotResponse)
{-# DEPRECATED crdsrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdsrsResponseStatus :: Lens.Lens' CreateRelationalDatabaseSnapshotResponse Lude.Int
crdsrsResponseStatus = Lens.lens (responseStatus :: CreateRelationalDatabaseSnapshotResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateRelationalDatabaseSnapshotResponse)
{-# DEPRECATED crdsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
