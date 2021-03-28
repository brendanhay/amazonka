{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.StopRelationalDatabase
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a specific database that is currently running in Amazon Lightsail.
--
-- The @stop relational database@ operation supports tag-based access control via resource tags applied to the resource identified by relationalDatabaseName. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.StopRelationalDatabase
    (
    -- * Creating a request
      StopRelationalDatabase (..)
    , mkStopRelationalDatabase
    -- ** Request lenses
    , srdRelationalDatabaseName
    , srdRelationalDatabaseSnapshotName

    -- * Destructuring the response
    , StopRelationalDatabaseResponse (..)
    , mkStopRelationalDatabaseResponse
    -- ** Response lenses
    , srdrrsOperations
    , srdrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStopRelationalDatabase' smart constructor.
data StopRelationalDatabase = StopRelationalDatabase'
  { relationalDatabaseName :: Types.RelationalDatabaseName
    -- ^ The name of your database to stop.
  , relationalDatabaseSnapshotName :: Core.Maybe Types.RelationalDatabaseSnapshotName
    -- ^ The name of your new database snapshot to be created before stopping your database.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StopRelationalDatabase' value with any optional fields omitted.
mkStopRelationalDatabase
    :: Types.RelationalDatabaseName -- ^ 'relationalDatabaseName'
    -> StopRelationalDatabase
mkStopRelationalDatabase relationalDatabaseName
  = StopRelationalDatabase'{relationalDatabaseName,
                            relationalDatabaseSnapshotName = Core.Nothing}

-- | The name of your database to stop.
--
-- /Note:/ Consider using 'relationalDatabaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdRelationalDatabaseName :: Lens.Lens' StopRelationalDatabase Types.RelationalDatabaseName
srdRelationalDatabaseName = Lens.field @"relationalDatabaseName"
{-# INLINEABLE srdRelationalDatabaseName #-}
{-# DEPRECATED relationalDatabaseName "Use generic-lens or generic-optics with 'relationalDatabaseName' instead"  #-}

-- | The name of your new database snapshot to be created before stopping your database.
--
-- /Note:/ Consider using 'relationalDatabaseSnapshotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdRelationalDatabaseSnapshotName :: Lens.Lens' StopRelationalDatabase (Core.Maybe Types.RelationalDatabaseSnapshotName)
srdRelationalDatabaseSnapshotName = Lens.field @"relationalDatabaseSnapshotName"
{-# INLINEABLE srdRelationalDatabaseSnapshotName #-}
{-# DEPRECATED relationalDatabaseSnapshotName "Use generic-lens or generic-optics with 'relationalDatabaseSnapshotName' instead"  #-}

instance Core.ToQuery StopRelationalDatabase where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StopRelationalDatabase where
        toHeaders StopRelationalDatabase{..}
          = Core.pure
              ("X-Amz-Target", "Lightsail_20161128.StopRelationalDatabase")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StopRelationalDatabase where
        toJSON StopRelationalDatabase{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("relationalDatabaseName" Core..= relationalDatabaseName),
                  ("relationalDatabaseSnapshotName" Core..=) Core.<$>
                    relationalDatabaseSnapshotName])

instance Core.AWSRequest StopRelationalDatabase where
        type Rs StopRelationalDatabase = StopRelationalDatabaseResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StopRelationalDatabaseResponse' Core.<$>
                   (x Core..:? "operations") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStopRelationalDatabaseResponse' smart constructor.
data StopRelationalDatabaseResponse = StopRelationalDatabaseResponse'
  { operations :: Core.Maybe [Types.Operation]
    -- ^ An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'StopRelationalDatabaseResponse' value with any optional fields omitted.
mkStopRelationalDatabaseResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StopRelationalDatabaseResponse
mkStopRelationalDatabaseResponse responseStatus
  = StopRelationalDatabaseResponse'{operations = Core.Nothing,
                                    responseStatus}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdrrsOperations :: Lens.Lens' StopRelationalDatabaseResponse (Core.Maybe [Types.Operation])
srdrrsOperations = Lens.field @"operations"
{-# INLINEABLE srdrrsOperations #-}
{-# DEPRECATED operations "Use generic-lens or generic-optics with 'operations' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdrrsResponseStatus :: Lens.Lens' StopRelationalDatabaseResponse Core.Int
srdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE srdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
