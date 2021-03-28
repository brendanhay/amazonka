{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.DeleteDatastore
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified data store.
module Network.AWS.IoTAnalytics.DeleteDatastore
    (
    -- * Creating a request
      DeleteDatastore (..)
    , mkDeleteDatastore
    -- ** Request lenses
    , ddDatastoreName

    -- * Destructuring the response
    , DeleteDatastoreResponse (..)
    , mkDeleteDatastoreResponse
    ) where

import qualified Network.AWS.IoTAnalytics.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteDatastore' smart constructor.
newtype DeleteDatastore = DeleteDatastore'
  { datastoreName :: Types.DatastoreName
    -- ^ The name of the data store to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDatastore' value with any optional fields omitted.
mkDeleteDatastore
    :: Types.DatastoreName -- ^ 'datastoreName'
    -> DeleteDatastore
mkDeleteDatastore datastoreName = DeleteDatastore'{datastoreName}

-- | The name of the data store to delete.
--
-- /Note:/ Consider using 'datastoreName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDatastoreName :: Lens.Lens' DeleteDatastore Types.DatastoreName
ddDatastoreName = Lens.field @"datastoreName"
{-# INLINEABLE ddDatastoreName #-}
{-# DEPRECATED datastoreName "Use generic-lens or generic-optics with 'datastoreName' instead"  #-}

instance Core.ToQuery DeleteDatastore where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteDatastore where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteDatastore where
        type Rs DeleteDatastore = DeleteDatastoreResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath = "/datastores/" Core.<> Core.toText datastoreName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteDatastoreResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteDatastoreResponse' smart constructor.
data DeleteDatastoreResponse = DeleteDatastoreResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDatastoreResponse' value with any optional fields omitted.
mkDeleteDatastoreResponse
    :: DeleteDatastoreResponse
mkDeleteDatastoreResponse = DeleteDatastoreResponse'
