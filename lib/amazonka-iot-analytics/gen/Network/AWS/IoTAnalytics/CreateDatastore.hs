{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.CreateDatastore
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a data store, which is a repository for messages.
module Network.AWS.IoTAnalytics.CreateDatastore
    (
    -- * Creating a request
      CreateDatastore (..)
    , mkCreateDatastore
    -- ** Request lenses
    , cdDatastoreName
    , cdDatastoreStorage
    , cdRetentionPeriod
    , cdTags

    -- * Destructuring the response
    , CreateDatastoreResponse (..)
    , mkCreateDatastoreResponse
    -- ** Response lenses
    , cdrrsDatastoreArn
    , cdrrsDatastoreName
    , cdrrsRetentionPeriod
    , cdrrsResponseStatus
    ) where

import qualified Network.AWS.IoTAnalytics.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateDatastore' smart constructor.
data CreateDatastore = CreateDatastore'
  { datastoreName :: Types.DatastoreName
    -- ^ The name of the data store.
  , datastoreStorage :: Core.Maybe Types.DatastoreStorage
    -- ^ Where data store data is stored. You can choose one of @serviceManagedS3@ or @customerManagedS3@ storage. If not specified, the default is @serviceManagedS3@ . You cannot change this storage option after the data store is created.
  , retentionPeriod :: Core.Maybe Types.RetentionPeriod
    -- ^ How long, in days, message data is kept for the data store. When @customerManagedS3@ storage is selected, this parameter is ignored.
  , tags :: Core.Maybe (Core.NonEmpty Types.Tag)
    -- ^ Metadata which can be used to manage the data store.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDatastore' value with any optional fields omitted.
mkCreateDatastore
    :: Types.DatastoreName -- ^ 'datastoreName'
    -> CreateDatastore
mkCreateDatastore datastoreName
  = CreateDatastore'{datastoreName, datastoreStorage = Core.Nothing,
                     retentionPeriod = Core.Nothing, tags = Core.Nothing}

-- | The name of the data store.
--
-- /Note:/ Consider using 'datastoreName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDatastoreName :: Lens.Lens' CreateDatastore Types.DatastoreName
cdDatastoreName = Lens.field @"datastoreName"
{-# INLINEABLE cdDatastoreName #-}
{-# DEPRECATED datastoreName "Use generic-lens or generic-optics with 'datastoreName' instead"  #-}

-- | Where data store data is stored. You can choose one of @serviceManagedS3@ or @customerManagedS3@ storage. If not specified, the default is @serviceManagedS3@ . You cannot change this storage option after the data store is created.
--
-- /Note:/ Consider using 'datastoreStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDatastoreStorage :: Lens.Lens' CreateDatastore (Core.Maybe Types.DatastoreStorage)
cdDatastoreStorage = Lens.field @"datastoreStorage"
{-# INLINEABLE cdDatastoreStorage #-}
{-# DEPRECATED datastoreStorage "Use generic-lens or generic-optics with 'datastoreStorage' instead"  #-}

-- | How long, in days, message data is kept for the data store. When @customerManagedS3@ storage is selected, this parameter is ignored.
--
-- /Note:/ Consider using 'retentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdRetentionPeriod :: Lens.Lens' CreateDatastore (Core.Maybe Types.RetentionPeriod)
cdRetentionPeriod = Lens.field @"retentionPeriod"
{-# INLINEABLE cdRetentionPeriod #-}
{-# DEPRECATED retentionPeriod "Use generic-lens or generic-optics with 'retentionPeriod' instead"  #-}

-- | Metadata which can be used to manage the data store.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdTags :: Lens.Lens' CreateDatastore (Core.Maybe (Core.NonEmpty Types.Tag))
cdTags = Lens.field @"tags"
{-# INLINEABLE cdTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateDatastore where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateDatastore where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON CreateDatastore where
        toJSON CreateDatastore{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("datastoreName" Core..= datastoreName),
                  ("datastoreStorage" Core..=) Core.<$> datastoreStorage,
                  ("retentionPeriod" Core..=) Core.<$> retentionPeriod,
                  ("tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateDatastore where
        type Rs CreateDatastore = CreateDatastoreResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/datastores",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateDatastoreResponse' Core.<$>
                   (x Core..:? "datastoreArn") Core.<*> x Core..:? "datastoreName"
                     Core.<*> x Core..:? "retentionPeriod"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateDatastoreResponse' smart constructor.
data CreateDatastoreResponse = CreateDatastoreResponse'
  { datastoreArn :: Core.Maybe Types.DatastoreArn
    -- ^ The ARN of the data store.
  , datastoreName :: Core.Maybe Types.DatastoreName
    -- ^ The name of the data store.
  , retentionPeriod :: Core.Maybe Types.RetentionPeriod
    -- ^ How long, in days, message data is kept for the data store.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDatastoreResponse' value with any optional fields omitted.
mkCreateDatastoreResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateDatastoreResponse
mkCreateDatastoreResponse responseStatus
  = CreateDatastoreResponse'{datastoreArn = Core.Nothing,
                             datastoreName = Core.Nothing, retentionPeriod = Core.Nothing,
                             responseStatus}

-- | The ARN of the data store.
--
-- /Note:/ Consider using 'datastoreArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrrsDatastoreArn :: Lens.Lens' CreateDatastoreResponse (Core.Maybe Types.DatastoreArn)
cdrrsDatastoreArn = Lens.field @"datastoreArn"
{-# INLINEABLE cdrrsDatastoreArn #-}
{-# DEPRECATED datastoreArn "Use generic-lens or generic-optics with 'datastoreArn' instead"  #-}

-- | The name of the data store.
--
-- /Note:/ Consider using 'datastoreName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrrsDatastoreName :: Lens.Lens' CreateDatastoreResponse (Core.Maybe Types.DatastoreName)
cdrrsDatastoreName = Lens.field @"datastoreName"
{-# INLINEABLE cdrrsDatastoreName #-}
{-# DEPRECATED datastoreName "Use generic-lens or generic-optics with 'datastoreName' instead"  #-}

-- | How long, in days, message data is kept for the data store.
--
-- /Note:/ Consider using 'retentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrrsRetentionPeriod :: Lens.Lens' CreateDatastoreResponse (Core.Maybe Types.RetentionPeriod)
cdrrsRetentionPeriod = Lens.field @"retentionPeriod"
{-# INLINEABLE cdrrsRetentionPeriod #-}
{-# DEPRECATED retentionPeriod "Use generic-lens or generic-optics with 'retentionPeriod' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrrsResponseStatus :: Lens.Lens' CreateDatastoreResponse Core.Int
cdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
