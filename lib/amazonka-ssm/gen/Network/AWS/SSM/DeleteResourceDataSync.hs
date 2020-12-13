{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DeleteResourceDataSync
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Resource Data Sync configuration. After the configuration is deleted, changes to data on managed instances are no longer synced to or from the target. Deleting a sync configuration does not delete data.
module Network.AWS.SSM.DeleteResourceDataSync
  ( -- * Creating a request
    DeleteResourceDataSync (..),
    mkDeleteResourceDataSync,

    -- ** Request lenses
    drdsSyncType,
    drdsSyncName,

    -- * Destructuring the response
    DeleteResourceDataSyncResponse (..),
    mkDeleteResourceDataSyncResponse,

    -- ** Response lenses
    drdsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkDeleteResourceDataSync' smart constructor.
data DeleteResourceDataSync = DeleteResourceDataSync'
  { -- | Specify the type of resource data sync to delete.
    syncType :: Lude.Maybe Lude.Text,
    -- | The name of the configuration to delete.
    syncName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteResourceDataSync' with the minimum fields required to make a request.
--
-- * 'syncType' - Specify the type of resource data sync to delete.
-- * 'syncName' - The name of the configuration to delete.
mkDeleteResourceDataSync ::
  -- | 'syncName'
  Lude.Text ->
  DeleteResourceDataSync
mkDeleteResourceDataSync pSyncName_ =
  DeleteResourceDataSync'
    { syncType = Lude.Nothing,
      syncName = pSyncName_
    }

-- | Specify the type of resource data sync to delete.
--
-- /Note:/ Consider using 'syncType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdsSyncType :: Lens.Lens' DeleteResourceDataSync (Lude.Maybe Lude.Text)
drdsSyncType = Lens.lens (syncType :: DeleteResourceDataSync -> Lude.Maybe Lude.Text) (\s a -> s {syncType = a} :: DeleteResourceDataSync)
{-# DEPRECATED drdsSyncType "Use generic-lens or generic-optics with 'syncType' instead." #-}

-- | The name of the configuration to delete.
--
-- /Note:/ Consider using 'syncName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdsSyncName :: Lens.Lens' DeleteResourceDataSync Lude.Text
drdsSyncName = Lens.lens (syncName :: DeleteResourceDataSync -> Lude.Text) (\s a -> s {syncName = a} :: DeleteResourceDataSync)
{-# DEPRECATED drdsSyncName "Use generic-lens or generic-optics with 'syncName' instead." #-}

instance Lude.AWSRequest DeleteResourceDataSync where
  type Rs DeleteResourceDataSync = DeleteResourceDataSyncResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteResourceDataSyncResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteResourceDataSync where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.DeleteResourceDataSync" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteResourceDataSync where
  toJSON DeleteResourceDataSync' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SyncType" Lude..=) Lude.<$> syncType,
            Lude.Just ("SyncName" Lude..= syncName)
          ]
      )

instance Lude.ToPath DeleteResourceDataSync where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteResourceDataSync where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteResourceDataSyncResponse' smart constructor.
newtype DeleteResourceDataSyncResponse = DeleteResourceDataSyncResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteResourceDataSyncResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteResourceDataSyncResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteResourceDataSyncResponse
mkDeleteResourceDataSyncResponse pResponseStatus_ =
  DeleteResourceDataSyncResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdsrsResponseStatus :: Lens.Lens' DeleteResourceDataSyncResponse Lude.Int
drdsrsResponseStatus = Lens.lens (responseStatus :: DeleteResourceDataSyncResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteResourceDataSyncResponse)
{-# DEPRECATED drdsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
