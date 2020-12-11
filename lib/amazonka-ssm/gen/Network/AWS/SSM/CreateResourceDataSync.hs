{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.CreateResourceDataSync
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A resource data sync helps you view data from multiple sources in a single location. Systems Manager offers two types of resource data sync: @SyncToDestination@ and @SyncFromSource@ .
--
-- You can configure Systems Manager Inventory to use the @SyncToDestination@ type to synchronize Inventory data from multiple AWS Regions to a single S3 bucket. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-inventory-datasync.html Configuring Resource Data Sync for Inventory> in the /AWS Systems Manager User Guide/ .
-- You can configure Systems Manager Explorer to use the @SyncFromSource@ type to synchronize operational work items (OpsItems) and operational data (OpsData) from multiple AWS Regions to a single S3 bucket. This type can synchronize OpsItems and OpsData from multiple AWS accounts and Regions or @EntireOrganization@ by using AWS Organizations. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/Explorer-resource-data-sync.html Setting up Systems Manager Explorer to display data from multiple accounts and Regions> in the /AWS Systems Manager User Guide/ .
-- A resource data sync is an asynchronous operation that returns immediately. After a successful initial sync is completed, the system continuously syncs data. To check the status of a sync, use the 'ListResourceDataSync' .
module Network.AWS.SSM.CreateResourceDataSync
  ( -- * Creating a request
    CreateResourceDataSync (..),
    mkCreateResourceDataSync,

    -- ** Request lenses
    crdsSyncType,
    crdsSyncSource,
    crdsS3Destination,
    crdsSyncName,

    -- * Destructuring the response
    CreateResourceDataSyncResponse (..),
    mkCreateResourceDataSyncResponse,

    -- ** Response lenses
    crdsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkCreateResourceDataSync' smart constructor.
data CreateResourceDataSync = CreateResourceDataSync'
  { syncType ::
      Lude.Maybe Lude.Text,
    syncSource ::
      Lude.Maybe ResourceDataSyncSource,
    s3Destination ::
      Lude.Maybe ResourceDataSyncS3Destination,
    syncName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateResourceDataSync' with the minimum fields required to make a request.
--
-- * 's3Destination' - Amazon S3 configuration details for the sync. This parameter is required if the @SyncType@ value is SyncToDestination.
-- * 'syncName' - A name for the configuration.
-- * 'syncSource' - Specify information about the data sources to synchronize. This parameter is required if the @SyncType@ value is SyncFromSource.
-- * 'syncType' - Specify @SyncToDestination@ to create a resource data sync that synchronizes data to an S3 bucket for Inventory. If you specify @SyncToDestination@ , you must provide a value for @S3Destination@ . Specify @SyncFromSource@ to synchronize data from a single account and multiple Regions, or multiple AWS accounts and Regions, as listed in AWS Organizations for Explorer. If you specify @SyncFromSource@ , you must provide a value for @SyncSource@ . The default value is @SyncToDestination@ .
mkCreateResourceDataSync ::
  -- | 'syncName'
  Lude.Text ->
  CreateResourceDataSync
mkCreateResourceDataSync pSyncName_ =
  CreateResourceDataSync'
    { syncType = Lude.Nothing,
      syncSource = Lude.Nothing,
      s3Destination = Lude.Nothing,
      syncName = pSyncName_
    }

-- | Specify @SyncToDestination@ to create a resource data sync that synchronizes data to an S3 bucket for Inventory. If you specify @SyncToDestination@ , you must provide a value for @S3Destination@ . Specify @SyncFromSource@ to synchronize data from a single account and multiple Regions, or multiple AWS accounts and Regions, as listed in AWS Organizations for Explorer. If you specify @SyncFromSource@ , you must provide a value for @SyncSource@ . The default value is @SyncToDestination@ .
--
-- /Note:/ Consider using 'syncType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdsSyncType :: Lens.Lens' CreateResourceDataSync (Lude.Maybe Lude.Text)
crdsSyncType = Lens.lens (syncType :: CreateResourceDataSync -> Lude.Maybe Lude.Text) (\s a -> s {syncType = a} :: CreateResourceDataSync)
{-# DEPRECATED crdsSyncType "Use generic-lens or generic-optics with 'syncType' instead." #-}

-- | Specify information about the data sources to synchronize. This parameter is required if the @SyncType@ value is SyncFromSource.
--
-- /Note:/ Consider using 'syncSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdsSyncSource :: Lens.Lens' CreateResourceDataSync (Lude.Maybe ResourceDataSyncSource)
crdsSyncSource = Lens.lens (syncSource :: CreateResourceDataSync -> Lude.Maybe ResourceDataSyncSource) (\s a -> s {syncSource = a} :: CreateResourceDataSync)
{-# DEPRECATED crdsSyncSource "Use generic-lens or generic-optics with 'syncSource' instead." #-}

-- | Amazon S3 configuration details for the sync. This parameter is required if the @SyncType@ value is SyncToDestination.
--
-- /Note:/ Consider using 's3Destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdsS3Destination :: Lens.Lens' CreateResourceDataSync (Lude.Maybe ResourceDataSyncS3Destination)
crdsS3Destination = Lens.lens (s3Destination :: CreateResourceDataSync -> Lude.Maybe ResourceDataSyncS3Destination) (\s a -> s {s3Destination = a} :: CreateResourceDataSync)
{-# DEPRECATED crdsS3Destination "Use generic-lens or generic-optics with 's3Destination' instead." #-}

-- | A name for the configuration.
--
-- /Note:/ Consider using 'syncName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdsSyncName :: Lens.Lens' CreateResourceDataSync Lude.Text
crdsSyncName = Lens.lens (syncName :: CreateResourceDataSync -> Lude.Text) (\s a -> s {syncName = a} :: CreateResourceDataSync)
{-# DEPRECATED crdsSyncName "Use generic-lens or generic-optics with 'syncName' instead." #-}

instance Lude.AWSRequest CreateResourceDataSync where
  type Rs CreateResourceDataSync = CreateResourceDataSyncResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveEmpty
      ( \s h x ->
          CreateResourceDataSyncResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateResourceDataSync where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.CreateResourceDataSync" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateResourceDataSync where
  toJSON CreateResourceDataSync' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SyncType" Lude..=) Lude.<$> syncType,
            ("SyncSource" Lude..=) Lude.<$> syncSource,
            ("S3Destination" Lude..=) Lude.<$> s3Destination,
            Lude.Just ("SyncName" Lude..= syncName)
          ]
      )

instance Lude.ToPath CreateResourceDataSync where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateResourceDataSync where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateResourceDataSyncResponse' smart constructor.
newtype CreateResourceDataSyncResponse = CreateResourceDataSyncResponse'
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

-- | Creates a value of 'CreateResourceDataSyncResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCreateResourceDataSyncResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateResourceDataSyncResponse
mkCreateResourceDataSyncResponse pResponseStatus_ =
  CreateResourceDataSyncResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdsrsResponseStatus :: Lens.Lens' CreateResourceDataSyncResponse Lude.Int
crdsrsResponseStatus = Lens.lens (responseStatus :: CreateResourceDataSyncResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateResourceDataSyncResponse)
{-# DEPRECATED crdsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
