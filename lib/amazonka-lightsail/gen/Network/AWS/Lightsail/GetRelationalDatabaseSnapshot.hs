{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetRelationalDatabaseSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific database snapshot in Amazon Lightsail.
module Network.AWS.Lightsail.GetRelationalDatabaseSnapshot
    (
    -- * Creating a request
      GetRelationalDatabaseSnapshot (..)
    , mkGetRelationalDatabaseSnapshot
    -- ** Request lenses
    , grdsRelationalDatabaseSnapshotName

    -- * Destructuring the response
    , GetRelationalDatabaseSnapshotResponse (..)
    , mkGetRelationalDatabaseSnapshotResponse
    -- ** Response lenses
    , grdsrfrsRelationalDatabaseSnapshot
    , grdsrfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetRelationalDatabaseSnapshot' smart constructor.
newtype GetRelationalDatabaseSnapshot = GetRelationalDatabaseSnapshot'
  { relationalDatabaseSnapshotName :: Types.RelationalDatabaseSnapshotName
    -- ^ The name of the database snapshot for which to get information.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetRelationalDatabaseSnapshot' value with any optional fields omitted.
mkGetRelationalDatabaseSnapshot
    :: Types.RelationalDatabaseSnapshotName -- ^ 'relationalDatabaseSnapshotName'
    -> GetRelationalDatabaseSnapshot
mkGetRelationalDatabaseSnapshot relationalDatabaseSnapshotName
  = GetRelationalDatabaseSnapshot'{relationalDatabaseSnapshotName}

-- | The name of the database snapshot for which to get information.
--
-- /Note:/ Consider using 'relationalDatabaseSnapshotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdsRelationalDatabaseSnapshotName :: Lens.Lens' GetRelationalDatabaseSnapshot Types.RelationalDatabaseSnapshotName
grdsRelationalDatabaseSnapshotName = Lens.field @"relationalDatabaseSnapshotName"
{-# INLINEABLE grdsRelationalDatabaseSnapshotName #-}
{-# DEPRECATED relationalDatabaseSnapshotName "Use generic-lens or generic-optics with 'relationalDatabaseSnapshotName' instead"  #-}

instance Core.ToQuery GetRelationalDatabaseSnapshot where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetRelationalDatabaseSnapshot where
        toHeaders GetRelationalDatabaseSnapshot{..}
          = Core.pure
              ("X-Amz-Target",
               "Lightsail_20161128.GetRelationalDatabaseSnapshot")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetRelationalDatabaseSnapshot where
        toJSON GetRelationalDatabaseSnapshot{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("relationalDatabaseSnapshotName" Core..=
                       relationalDatabaseSnapshotName)])

instance Core.AWSRequest GetRelationalDatabaseSnapshot where
        type Rs GetRelationalDatabaseSnapshot =
             GetRelationalDatabaseSnapshotResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetRelationalDatabaseSnapshotResponse' Core.<$>
                   (x Core..:? "relationalDatabaseSnapshot") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetRelationalDatabaseSnapshotResponse' smart constructor.
data GetRelationalDatabaseSnapshotResponse = GetRelationalDatabaseSnapshotResponse'
  { relationalDatabaseSnapshot :: Core.Maybe Types.RelationalDatabaseSnapshot
    -- ^ An object describing the specified database snapshot.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetRelationalDatabaseSnapshotResponse' value with any optional fields omitted.
mkGetRelationalDatabaseSnapshotResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetRelationalDatabaseSnapshotResponse
mkGetRelationalDatabaseSnapshotResponse responseStatus
  = GetRelationalDatabaseSnapshotResponse'{relationalDatabaseSnapshot
                                             = Core.Nothing,
                                           responseStatus}

-- | An object describing the specified database snapshot.
--
-- /Note:/ Consider using 'relationalDatabaseSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdsrfrsRelationalDatabaseSnapshot :: Lens.Lens' GetRelationalDatabaseSnapshotResponse (Core.Maybe Types.RelationalDatabaseSnapshot)
grdsrfrsRelationalDatabaseSnapshot = Lens.field @"relationalDatabaseSnapshot"
{-# INLINEABLE grdsrfrsRelationalDatabaseSnapshot #-}
{-# DEPRECATED relationalDatabaseSnapshot "Use generic-lens or generic-optics with 'relationalDatabaseSnapshot' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdsrfrsResponseStatus :: Lens.Lens' GetRelationalDatabaseSnapshotResponse Core.Int
grdsrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE grdsrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
