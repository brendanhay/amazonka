{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.UpdateResourceDataSync
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a resource data sync. After you create a resource data sync for a Region, you can't change the account options for that sync. For example, if you create a sync in the us-east-2 (Ohio) Region and you choose the Include only the current account option, you can't edit that sync later and choose the Include all accounts from my AWS Organizations configuration option. Instead, you must delete the first resource data sync, and create a new one.
module Network.AWS.SSM.UpdateResourceDataSync
  ( -- * Creating a request
    UpdateResourceDataSync (..),
    mkUpdateResourceDataSync,

    -- ** Request lenses
    urdsSyncType,
    urdsSyncSource,
    urdsSyncName,

    -- * Destructuring the response
    UpdateResourceDataSyncResponse (..),
    mkUpdateResourceDataSyncResponse,

    -- ** Response lenses
    urdsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkUpdateResourceDataSync' smart constructor.
data UpdateResourceDataSync = UpdateResourceDataSync'
  { -- | The type of resource data sync. The supported @SyncType@ is SyncFromSource.
    syncType :: Lude.Text,
    -- | Specify information about the data sources to synchronize.
    syncSource :: ResourceDataSyncSource,
    -- | The name of the resource data sync you want to update.
    syncName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateResourceDataSync' with the minimum fields required to make a request.
--
-- * 'syncType' - The type of resource data sync. The supported @SyncType@ is SyncFromSource.
-- * 'syncSource' - Specify information about the data sources to synchronize.
-- * 'syncName' - The name of the resource data sync you want to update.
mkUpdateResourceDataSync ::
  -- | 'syncType'
  Lude.Text ->
  -- | 'syncSource'
  ResourceDataSyncSource ->
  -- | 'syncName'
  Lude.Text ->
  UpdateResourceDataSync
mkUpdateResourceDataSync pSyncType_ pSyncSource_ pSyncName_ =
  UpdateResourceDataSync'
    { syncType = pSyncType_,
      syncSource = pSyncSource_,
      syncName = pSyncName_
    }

-- | The type of resource data sync. The supported @SyncType@ is SyncFromSource.
--
-- /Note:/ Consider using 'syncType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urdsSyncType :: Lens.Lens' UpdateResourceDataSync Lude.Text
urdsSyncType = Lens.lens (syncType :: UpdateResourceDataSync -> Lude.Text) (\s a -> s {syncType = a} :: UpdateResourceDataSync)
{-# DEPRECATED urdsSyncType "Use generic-lens or generic-optics with 'syncType' instead." #-}

-- | Specify information about the data sources to synchronize.
--
-- /Note:/ Consider using 'syncSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urdsSyncSource :: Lens.Lens' UpdateResourceDataSync ResourceDataSyncSource
urdsSyncSource = Lens.lens (syncSource :: UpdateResourceDataSync -> ResourceDataSyncSource) (\s a -> s {syncSource = a} :: UpdateResourceDataSync)
{-# DEPRECATED urdsSyncSource "Use generic-lens or generic-optics with 'syncSource' instead." #-}

-- | The name of the resource data sync you want to update.
--
-- /Note:/ Consider using 'syncName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urdsSyncName :: Lens.Lens' UpdateResourceDataSync Lude.Text
urdsSyncName = Lens.lens (syncName :: UpdateResourceDataSync -> Lude.Text) (\s a -> s {syncName = a} :: UpdateResourceDataSync)
{-# DEPRECATED urdsSyncName "Use generic-lens or generic-optics with 'syncName' instead." #-}

instance Lude.AWSRequest UpdateResourceDataSync where
  type Rs UpdateResourceDataSync = UpdateResourceDataSyncResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateResourceDataSyncResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateResourceDataSync where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.UpdateResourceDataSync" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateResourceDataSync where
  toJSON UpdateResourceDataSync' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("SyncType" Lude..= syncType),
            Lude.Just ("SyncSource" Lude..= syncSource),
            Lude.Just ("SyncName" Lude..= syncName)
          ]
      )

instance Lude.ToPath UpdateResourceDataSync where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateResourceDataSync where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateResourceDataSyncResponse' smart constructor.
newtype UpdateResourceDataSyncResponse = UpdateResourceDataSyncResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateResourceDataSyncResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateResourceDataSyncResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateResourceDataSyncResponse
mkUpdateResourceDataSyncResponse pResponseStatus_ =
  UpdateResourceDataSyncResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urdsrsResponseStatus :: Lens.Lens' UpdateResourceDataSyncResponse Lude.Int
urdsrsResponseStatus = Lens.lens (responseStatus :: UpdateResourceDataSyncResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateResourceDataSyncResponse)
{-# DEPRECATED urdsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
