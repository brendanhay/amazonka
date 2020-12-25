{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.DescribeGlobalTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified global table.
module Network.AWS.DynamoDB.DescribeGlobalTable
  ( -- * Creating a request
    DescribeGlobalTable (..),
    mkDescribeGlobalTable,

    -- ** Request lenses
    dgtGlobalTableName,

    -- * Destructuring the response
    DescribeGlobalTableResponse (..),
    mkDescribeGlobalTableResponse,

    -- ** Response lenses
    dgtrrsGlobalTableDescription,
    dgtrrsResponseStatus,
  )
where

import qualified Network.AWS.DynamoDB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeGlobalTable' smart constructor.
newtype DescribeGlobalTable = DescribeGlobalTable'
  { -- | The name of the global table.
    globalTableName :: Types.GlobalTableName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeGlobalTable' value with any optional fields omitted.
mkDescribeGlobalTable ::
  -- | 'globalTableName'
  Types.GlobalTableName ->
  DescribeGlobalTable
mkDescribeGlobalTable globalTableName =
  DescribeGlobalTable' {globalTableName}

-- | The name of the global table.
--
-- /Note:/ Consider using 'globalTableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgtGlobalTableName :: Lens.Lens' DescribeGlobalTable Types.GlobalTableName
dgtGlobalTableName = Lens.field @"globalTableName"
{-# DEPRECATED dgtGlobalTableName "Use generic-lens or generic-optics with 'globalTableName' instead." #-}

instance Core.FromJSON DescribeGlobalTable where
  toJSON DescribeGlobalTable {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("GlobalTableName" Core..= globalTableName)]
      )

instance Core.AWSRequest DescribeGlobalTable where
  type Rs DescribeGlobalTable = DescribeGlobalTableResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "DynamoDB_20120810.DescribeGlobalTable")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.0")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeGlobalTableResponse'
            Core.<$> (x Core..:? "GlobalTableDescription")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeGlobalTableResponse' smart constructor.
data DescribeGlobalTableResponse = DescribeGlobalTableResponse'
  { -- | Contains the details of the global table.
    globalTableDescription :: Core.Maybe Types.GlobalTableDescription,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeGlobalTableResponse' value with any optional fields omitted.
mkDescribeGlobalTableResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeGlobalTableResponse
mkDescribeGlobalTableResponse responseStatus =
  DescribeGlobalTableResponse'
    { globalTableDescription =
        Core.Nothing,
      responseStatus
    }

-- | Contains the details of the global table.
--
-- /Note:/ Consider using 'globalTableDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgtrrsGlobalTableDescription :: Lens.Lens' DescribeGlobalTableResponse (Core.Maybe Types.GlobalTableDescription)
dgtrrsGlobalTableDescription = Lens.field @"globalTableDescription"
{-# DEPRECATED dgtrrsGlobalTableDescription "Use generic-lens or generic-optics with 'globalTableDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgtrrsResponseStatus :: Lens.Lens' DescribeGlobalTableResponse Core.Int
dgtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dgtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
