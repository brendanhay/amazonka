{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.DeleteDataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a @DataSource@ object.
module Network.AWS.AppSync.DeleteDataSource
  ( -- * Creating a request
    DeleteDataSource (..),
    mkDeleteDataSource,

    -- ** Request lenses
    ddsApiId,
    ddsName,

    -- * Destructuring the response
    DeleteDataSourceResponse (..),
    mkDeleteDataSourceResponse,

    -- ** Response lenses
    ddsrrsResponseStatus,
  )
where

import qualified Network.AWS.AppSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteDataSource' smart constructor.
data DeleteDataSource = DeleteDataSource'
  { -- | The API ID.
    apiId :: Types.String,
    -- | The name of the data source.
    name :: Types.ResourceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDataSource' value with any optional fields omitted.
mkDeleteDataSource ::
  -- | 'apiId'
  Types.String ->
  -- | 'name'
  Types.ResourceName ->
  DeleteDataSource
mkDeleteDataSource apiId name = DeleteDataSource' {apiId, name}

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsApiId :: Lens.Lens' DeleteDataSource Types.String
ddsApiId = Lens.field @"apiId"
{-# DEPRECATED ddsApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

-- | The name of the data source.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsName :: Lens.Lens' DeleteDataSource Types.ResourceName
ddsName = Lens.field @"name"
{-# DEPRECATED ddsName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.AWSRequest DeleteDataSource where
  type Rs DeleteDataSource = DeleteDataSourceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ( "/v1/apis/" Core.<> (Core.toText apiId) Core.<> ("/datasources/")
                Core.<> (Core.toText name)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteDataSourceResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteDataSourceResponse' smart constructor.
newtype DeleteDataSourceResponse = DeleteDataSourceResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDataSourceResponse' value with any optional fields omitted.
mkDeleteDataSourceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteDataSourceResponse
mkDeleteDataSourceResponse responseStatus =
  DeleteDataSourceResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsrrsResponseStatus :: Lens.Lens' DeleteDataSourceResponse Core.Int
ddsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
