{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.GetDatabase
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a database object for the specfied database and data catalog.
module Network.AWS.Athena.GetDatabase
    (
    -- * Creating a request
      GetDatabase (..)
    , mkGetDatabase
    -- ** Request lenses
    , gdCatalogName
    , gdDatabaseName

    -- * Destructuring the response
    , GetDatabaseResponse (..)
    , mkGetDatabaseResponse
    -- ** Response lenses
    , gdrrsDatabase
    , gdrrsResponseStatus
    ) where

import qualified Network.AWS.Athena.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetDatabase' smart constructor.
data GetDatabase = GetDatabase'
  { catalogName :: Types.CatalogName
    -- ^ The name of the data catalog that contains the database to return.
  , databaseName :: Types.NameString
    -- ^ The name of the database to return.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDatabase' value with any optional fields omitted.
mkGetDatabase
    :: Types.CatalogName -- ^ 'catalogName'
    -> Types.NameString -- ^ 'databaseName'
    -> GetDatabase
mkGetDatabase catalogName databaseName
  = GetDatabase'{catalogName, databaseName}

-- | The name of the data catalog that contains the database to return.
--
-- /Note:/ Consider using 'catalogName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdCatalogName :: Lens.Lens' GetDatabase Types.CatalogName
gdCatalogName = Lens.field @"catalogName"
{-# INLINEABLE gdCatalogName #-}
{-# DEPRECATED catalogName "Use generic-lens or generic-optics with 'catalogName' instead"  #-}

-- | The name of the database to return.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdDatabaseName :: Lens.Lens' GetDatabase Types.NameString
gdDatabaseName = Lens.field @"databaseName"
{-# INLINEABLE gdDatabaseName #-}
{-# DEPRECATED databaseName "Use generic-lens or generic-optics with 'databaseName' instead"  #-}

instance Core.ToQuery GetDatabase where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetDatabase where
        toHeaders GetDatabase{..}
          = Core.pure ("X-Amz-Target", "AmazonAthena.GetDatabase") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetDatabase where
        toJSON GetDatabase{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("CatalogName" Core..= catalogName),
                  Core.Just ("DatabaseName" Core..= databaseName)])

instance Core.AWSRequest GetDatabase where
        type Rs GetDatabase = GetDatabaseResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetDatabaseResponse' Core.<$>
                   (x Core..:? "Database") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetDatabaseResponse' smart constructor.
data GetDatabaseResponse = GetDatabaseResponse'
  { database :: Core.Maybe Types.Database
    -- ^ The database returned.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDatabaseResponse' value with any optional fields omitted.
mkGetDatabaseResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetDatabaseResponse
mkGetDatabaseResponse responseStatus
  = GetDatabaseResponse'{database = Core.Nothing, responseStatus}

-- | The database returned.
--
-- /Note:/ Consider using 'database' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsDatabase :: Lens.Lens' GetDatabaseResponse (Core.Maybe Types.Database)
gdrrsDatabase = Lens.field @"database"
{-# INLINEABLE gdrrsDatabase #-}
{-# DEPRECATED database "Use generic-lens or generic-optics with 'database' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsResponseStatus :: Lens.Lens' GetDatabaseResponse Core.Int
gdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
