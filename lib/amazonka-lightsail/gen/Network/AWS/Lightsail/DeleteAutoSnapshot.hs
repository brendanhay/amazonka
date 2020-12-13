{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DeleteAutoSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an automatic snapshot of an instance or disk. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide> .
module Network.AWS.Lightsail.DeleteAutoSnapshot
  ( -- * Creating a request
    DeleteAutoSnapshot (..),
    mkDeleteAutoSnapshot,

    -- ** Request lenses
    dasResourceName,
    dasDate,

    -- * Destructuring the response
    DeleteAutoSnapshotResponse (..),
    mkDeleteAutoSnapshotResponse,

    -- ** Response lenses
    dasrsOperations,
    dasrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteAutoSnapshot' smart constructor.
data DeleteAutoSnapshot = DeleteAutoSnapshot'
  { -- | The name of the source instance or disk from which to delete the automatic snapshot.
    resourceName :: Lude.Text,
    -- | The date of the automatic snapshot to delete in @YYYY-MM-DD@ format. Use the @get auto snapshots@ operation to get the available automatic snapshots for a resource.
    date :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAutoSnapshot' with the minimum fields required to make a request.
--
-- * 'resourceName' - The name of the source instance or disk from which to delete the automatic snapshot.
-- * 'date' - The date of the automatic snapshot to delete in @YYYY-MM-DD@ format. Use the @get auto snapshots@ operation to get the available automatic snapshots for a resource.
mkDeleteAutoSnapshot ::
  -- | 'resourceName'
  Lude.Text ->
  -- | 'date'
  Lude.Text ->
  DeleteAutoSnapshot
mkDeleteAutoSnapshot pResourceName_ pDate_ =
  DeleteAutoSnapshot' {resourceName = pResourceName_, date = pDate_}

-- | The name of the source instance or disk from which to delete the automatic snapshot.
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasResourceName :: Lens.Lens' DeleteAutoSnapshot Lude.Text
dasResourceName = Lens.lens (resourceName :: DeleteAutoSnapshot -> Lude.Text) (\s a -> s {resourceName = a} :: DeleteAutoSnapshot)
{-# DEPRECATED dasResourceName "Use generic-lens or generic-optics with 'resourceName' instead." #-}

-- | The date of the automatic snapshot to delete in @YYYY-MM-DD@ format. Use the @get auto snapshots@ operation to get the available automatic snapshots for a resource.
--
-- /Note:/ Consider using 'date' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasDate :: Lens.Lens' DeleteAutoSnapshot Lude.Text
dasDate = Lens.lens (date :: DeleteAutoSnapshot -> Lude.Text) (\s a -> s {date = a} :: DeleteAutoSnapshot)
{-# DEPRECATED dasDate "Use generic-lens or generic-optics with 'date' instead." #-}

instance Lude.AWSRequest DeleteAutoSnapshot where
  type Rs DeleteAutoSnapshot = DeleteAutoSnapshotResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteAutoSnapshotResponse'
            Lude.<$> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteAutoSnapshot where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.DeleteAutoSnapshot" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteAutoSnapshot where
  toJSON DeleteAutoSnapshot' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("resourceName" Lude..= resourceName),
            Lude.Just ("date" Lude..= date)
          ]
      )

instance Lude.ToPath DeleteAutoSnapshot where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteAutoSnapshot where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteAutoSnapshotResponse' smart constructor.
data DeleteAutoSnapshotResponse = DeleteAutoSnapshotResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Lude.Maybe [Operation],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAutoSnapshotResponse' with the minimum fields required to make a request.
--
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkDeleteAutoSnapshotResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteAutoSnapshotResponse
mkDeleteAutoSnapshotResponse pResponseStatus_ =
  DeleteAutoSnapshotResponse'
    { operations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasrsOperations :: Lens.Lens' DeleteAutoSnapshotResponse (Lude.Maybe [Operation])
dasrsOperations = Lens.lens (operations :: DeleteAutoSnapshotResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: DeleteAutoSnapshotResponse)
{-# DEPRECATED dasrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasrsResponseStatus :: Lens.Lens' DeleteAutoSnapshotResponse Lude.Int
dasrsResponseStatus = Lens.lens (responseStatus :: DeleteAutoSnapshotResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteAutoSnapshotResponse)
{-# DEPRECATED dasrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
