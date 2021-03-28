{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.GetDataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a @DataSource@ object.
module Network.AWS.AppSync.GetDataSource
    (
    -- * Creating a request
      GetDataSource (..)
    , mkGetDataSource
    -- ** Request lenses
    , gdsApiId
    , gdsName

    -- * Destructuring the response
    , GetDataSourceResponse (..)
    , mkGetDataSourceResponse
    -- ** Response lenses
    , gdsrrsDataSource
    , gdsrrsResponseStatus
    ) where

import qualified Network.AWS.AppSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetDataSource' smart constructor.
data GetDataSource = GetDataSource'
  { apiId :: Core.Text
    -- ^ The API ID.
  , name :: Types.Name
    -- ^ The name of the data source.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDataSource' value with any optional fields omitted.
mkGetDataSource
    :: Core.Text -- ^ 'apiId'
    -> Types.Name -- ^ 'name'
    -> GetDataSource
mkGetDataSource apiId name = GetDataSource'{apiId, name}

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsApiId :: Lens.Lens' GetDataSource Core.Text
gdsApiId = Lens.field @"apiId"
{-# INLINEABLE gdsApiId #-}
{-# DEPRECATED apiId "Use generic-lens or generic-optics with 'apiId' instead"  #-}

-- | The name of the data source.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsName :: Lens.Lens' GetDataSource Types.Name
gdsName = Lens.field @"name"
{-# INLINEABLE gdsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery GetDataSource where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetDataSource where
        toHeaders GetDataSource{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetDataSource where
        type Rs GetDataSource = GetDataSourceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/v1/apis/" Core.<> Core.toText apiId Core.<> "/datasources/"
                             Core.<> Core.toText name,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetDataSourceResponse' Core.<$>
                   (x Core..:? "dataSource") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetDataSourceResponse' smart constructor.
data GetDataSourceResponse = GetDataSourceResponse'
  { dataSource :: Core.Maybe Types.DataSource
    -- ^ The @DataSource@ object.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDataSourceResponse' value with any optional fields omitted.
mkGetDataSourceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetDataSourceResponse
mkGetDataSourceResponse responseStatus
  = GetDataSourceResponse'{dataSource = Core.Nothing, responseStatus}

-- | The @DataSource@ object.
--
-- /Note:/ Consider using 'dataSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrrsDataSource :: Lens.Lens' GetDataSourceResponse (Core.Maybe Types.DataSource)
gdsrrsDataSource = Lens.field @"dataSource"
{-# INLINEABLE gdsrrsDataSource #-}
{-# DEPRECATED dataSource "Use generic-lens or generic-optics with 'dataSource' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrrsResponseStatus :: Lens.Lens' GetDataSourceResponse Core.Int
gdsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gdsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
