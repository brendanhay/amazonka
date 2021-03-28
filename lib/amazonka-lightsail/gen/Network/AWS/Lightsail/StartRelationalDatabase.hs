{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.StartRelationalDatabase
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a specific database from a stopped state in Amazon Lightsail. To restart a database, use the @reboot relational database@ operation.
--
-- The @start relational database@ operation supports tag-based access control via resource tags applied to the resource identified by relationalDatabaseName. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.StartRelationalDatabase
    (
    -- * Creating a request
      StartRelationalDatabase (..)
    , mkStartRelationalDatabase
    -- ** Request lenses
    , sRelationalDatabaseName

    -- * Destructuring the response
    , StartRelationalDatabaseResponse (..)
    , mkStartRelationalDatabaseResponse
    -- ** Response lenses
    , srsOperations
    , srsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartRelationalDatabase' smart constructor.
newtype StartRelationalDatabase = StartRelationalDatabase'
  { relationalDatabaseName :: Types.ResourceName
    -- ^ The name of your database to start.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartRelationalDatabase' value with any optional fields omitted.
mkStartRelationalDatabase
    :: Types.ResourceName -- ^ 'relationalDatabaseName'
    -> StartRelationalDatabase
mkStartRelationalDatabase relationalDatabaseName
  = StartRelationalDatabase'{relationalDatabaseName}

-- | The name of your database to start.
--
-- /Note:/ Consider using 'relationalDatabaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sRelationalDatabaseName :: Lens.Lens' StartRelationalDatabase Types.ResourceName
sRelationalDatabaseName = Lens.field @"relationalDatabaseName"
{-# INLINEABLE sRelationalDatabaseName #-}
{-# DEPRECATED relationalDatabaseName "Use generic-lens or generic-optics with 'relationalDatabaseName' instead"  #-}

instance Core.ToQuery StartRelationalDatabase where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartRelationalDatabase where
        toHeaders StartRelationalDatabase{..}
          = Core.pure
              ("X-Amz-Target", "Lightsail_20161128.StartRelationalDatabase")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StartRelationalDatabase where
        toJSON StartRelationalDatabase{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("relationalDatabaseName" Core..= relationalDatabaseName)])

instance Core.AWSRequest StartRelationalDatabase where
        type Rs StartRelationalDatabase = StartRelationalDatabaseResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StartRelationalDatabaseResponse' Core.<$>
                   (x Core..:? "operations") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStartRelationalDatabaseResponse' smart constructor.
data StartRelationalDatabaseResponse = StartRelationalDatabaseResponse'
  { operations :: Core.Maybe [Types.Operation]
    -- ^ An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'StartRelationalDatabaseResponse' value with any optional fields omitted.
mkStartRelationalDatabaseResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StartRelationalDatabaseResponse
mkStartRelationalDatabaseResponse responseStatus
  = StartRelationalDatabaseResponse'{operations = Core.Nothing,
                                     responseStatus}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsOperations :: Lens.Lens' StartRelationalDatabaseResponse (Core.Maybe [Types.Operation])
srsOperations = Lens.field @"operations"
{-# INLINEABLE srsOperations #-}
{-# DEPRECATED operations "Use generic-lens or generic-optics with 'operations' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StartRelationalDatabaseResponse Core.Int
srsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE srsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
