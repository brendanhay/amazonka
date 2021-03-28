{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateResourceDataSync (..)
    , mkCreateResourceDataSync
    -- ** Request lenses
    , crdsSyncName
    , crdsS3Destination
    , crdsSyncSource
    , crdsSyncType

    -- * Destructuring the response
    , CreateResourceDataSyncResponse (..)
    , mkCreateResourceDataSyncResponse
    -- ** Response lenses
    , crdsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkCreateResourceDataSync' smart constructor.
data CreateResourceDataSync = CreateResourceDataSync'
  { syncName :: Types.ResourceDataSyncName
    -- ^ A name for the configuration.
  , s3Destination :: Core.Maybe Types.ResourceDataSyncS3Destination
    -- ^ Amazon S3 configuration details for the sync. This parameter is required if the @SyncType@ value is SyncToDestination.
  , syncSource :: Core.Maybe Types.ResourceDataSyncSource
    -- ^ Specify information about the data sources to synchronize. This parameter is required if the @SyncType@ value is SyncFromSource.
  , syncType :: Core.Maybe Types.ResourceDataSyncType
    -- ^ Specify @SyncToDestination@ to create a resource data sync that synchronizes data to an S3 bucket for Inventory. If you specify @SyncToDestination@ , you must provide a value for @S3Destination@ . Specify @SyncFromSource@ to synchronize data from a single account and multiple Regions, or multiple AWS accounts and Regions, as listed in AWS Organizations for Explorer. If you specify @SyncFromSource@ , you must provide a value for @SyncSource@ . The default value is @SyncToDestination@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateResourceDataSync' value with any optional fields omitted.
mkCreateResourceDataSync
    :: Types.ResourceDataSyncName -- ^ 'syncName'
    -> CreateResourceDataSync
mkCreateResourceDataSync syncName
  = CreateResourceDataSync'{syncName, s3Destination = Core.Nothing,
                            syncSource = Core.Nothing, syncType = Core.Nothing}

-- | A name for the configuration.
--
-- /Note:/ Consider using 'syncName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdsSyncName :: Lens.Lens' CreateResourceDataSync Types.ResourceDataSyncName
crdsSyncName = Lens.field @"syncName"
{-# INLINEABLE crdsSyncName #-}
{-# DEPRECATED syncName "Use generic-lens or generic-optics with 'syncName' instead"  #-}

-- | Amazon S3 configuration details for the sync. This parameter is required if the @SyncType@ value is SyncToDestination.
--
-- /Note:/ Consider using 's3Destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdsS3Destination :: Lens.Lens' CreateResourceDataSync (Core.Maybe Types.ResourceDataSyncS3Destination)
crdsS3Destination = Lens.field @"s3Destination"
{-# INLINEABLE crdsS3Destination #-}
{-# DEPRECATED s3Destination "Use generic-lens or generic-optics with 's3Destination' instead"  #-}

-- | Specify information about the data sources to synchronize. This parameter is required if the @SyncType@ value is SyncFromSource.
--
-- /Note:/ Consider using 'syncSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdsSyncSource :: Lens.Lens' CreateResourceDataSync (Core.Maybe Types.ResourceDataSyncSource)
crdsSyncSource = Lens.field @"syncSource"
{-# INLINEABLE crdsSyncSource #-}
{-# DEPRECATED syncSource "Use generic-lens or generic-optics with 'syncSource' instead"  #-}

-- | Specify @SyncToDestination@ to create a resource data sync that synchronizes data to an S3 bucket for Inventory. If you specify @SyncToDestination@ , you must provide a value for @S3Destination@ . Specify @SyncFromSource@ to synchronize data from a single account and multiple Regions, or multiple AWS accounts and Regions, as listed in AWS Organizations for Explorer. If you specify @SyncFromSource@ , you must provide a value for @SyncSource@ . The default value is @SyncToDestination@ .
--
-- /Note:/ Consider using 'syncType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdsSyncType :: Lens.Lens' CreateResourceDataSync (Core.Maybe Types.ResourceDataSyncType)
crdsSyncType = Lens.field @"syncType"
{-# INLINEABLE crdsSyncType #-}
{-# DEPRECATED syncType "Use generic-lens or generic-optics with 'syncType' instead"  #-}

instance Core.ToQuery CreateResourceDataSync where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateResourceDataSync where
        toHeaders CreateResourceDataSync{..}
          = Core.pure ("X-Amz-Target", "AmazonSSM.CreateResourceDataSync")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateResourceDataSync where
        toJSON CreateResourceDataSync{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("SyncName" Core..= syncName),
                  ("S3Destination" Core..=) Core.<$> s3Destination,
                  ("SyncSource" Core..=) Core.<$> syncSource,
                  ("SyncType" Core..=) Core.<$> syncType])

instance Core.AWSRequest CreateResourceDataSync where
        type Rs CreateResourceDataSync = CreateResourceDataSyncResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 CreateResourceDataSyncResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateResourceDataSyncResponse' smart constructor.
newtype CreateResourceDataSyncResponse = CreateResourceDataSyncResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateResourceDataSyncResponse' value with any optional fields omitted.
mkCreateResourceDataSyncResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateResourceDataSyncResponse
mkCreateResourceDataSyncResponse responseStatus
  = CreateResourceDataSyncResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdsrrsResponseStatus :: Lens.Lens' CreateResourceDataSyncResponse Core.Int
crdsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE crdsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
