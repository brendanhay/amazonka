{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.ListSchemaExtensions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all schema extensions applied to a Microsoft AD Directory.
--
-- This operation returns paginated results.
module Network.AWS.DirectoryService.ListSchemaExtensions
  ( -- * Creating a request
    ListSchemaExtensions (..),
    mkListSchemaExtensions,

    -- ** Request lenses
    lseDirectoryId,
    lseLimit,
    lseNextToken,

    -- * Destructuring the response
    ListSchemaExtensionsResponse (..),
    mkListSchemaExtensionsResponse,

    -- ** Response lenses
    lserrsNextToken,
    lserrsSchemaExtensionsInfo,
    lserrsResponseStatus,
  )
where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListSchemaExtensions' smart constructor.
data ListSchemaExtensions = ListSchemaExtensions'
  { -- | The identifier of the directory from which to retrieve the schema extension information.
    directoryId :: Types.DirectoryId,
    -- | The maximum number of items to return.
    limit :: Core.Maybe Core.Natural,
    -- | The @ListSchemaExtensions.NextToken@ value from a previous call to @ListSchemaExtensions@ . Pass null if this is the first call.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListSchemaExtensions' value with any optional fields omitted.
mkListSchemaExtensions ::
  -- | 'directoryId'
  Types.DirectoryId ->
  ListSchemaExtensions
mkListSchemaExtensions directoryId =
  ListSchemaExtensions'
    { directoryId,
      limit = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The identifier of the directory from which to retrieve the schema extension information.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lseDirectoryId :: Lens.Lens' ListSchemaExtensions Types.DirectoryId
lseDirectoryId = Lens.field @"directoryId"
{-# DEPRECATED lseDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The maximum number of items to return.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lseLimit :: Lens.Lens' ListSchemaExtensions (Core.Maybe Core.Natural)
lseLimit = Lens.field @"limit"
{-# DEPRECATED lseLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The @ListSchemaExtensions.NextToken@ value from a previous call to @ListSchemaExtensions@ . Pass null if this is the first call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lseNextToken :: Lens.Lens' ListSchemaExtensions (Core.Maybe Types.NextToken)
lseNextToken = Lens.field @"nextToken"
{-# DEPRECATED lseNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListSchemaExtensions where
  toJSON ListSchemaExtensions {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DirectoryId" Core..= directoryId),
            ("Limit" Core..=) Core.<$> limit,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListSchemaExtensions where
  type Rs ListSchemaExtensions = ListSchemaExtensionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "DirectoryService_20150416.ListSchemaExtensions")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSchemaExtensionsResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "SchemaExtensionsInfo")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListSchemaExtensions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"schemaExtensionsInfo" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListSchemaExtensionsResponse' smart constructor.
data ListSchemaExtensionsResponse = ListSchemaExtensionsResponse'
  { -- | If not null, more results are available. Pass this value for the @NextToken@ parameter in a subsequent call to @ListSchemaExtensions@ to retrieve the next set of items.
    nextToken :: Core.Maybe Types.NextToken,
    -- | Information about the schema extensions applied to the directory.
    schemaExtensionsInfo :: Core.Maybe [Types.SchemaExtensionInfo],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListSchemaExtensionsResponse' value with any optional fields omitted.
mkListSchemaExtensionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListSchemaExtensionsResponse
mkListSchemaExtensionsResponse responseStatus =
  ListSchemaExtensionsResponse'
    { nextToken = Core.Nothing,
      schemaExtensionsInfo = Core.Nothing,
      responseStatus
    }

-- | If not null, more results are available. Pass this value for the @NextToken@ parameter in a subsequent call to @ListSchemaExtensions@ to retrieve the next set of items.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lserrsNextToken :: Lens.Lens' ListSchemaExtensionsResponse (Core.Maybe Types.NextToken)
lserrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lserrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the schema extensions applied to the directory.
--
-- /Note:/ Consider using 'schemaExtensionsInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lserrsSchemaExtensionsInfo :: Lens.Lens' ListSchemaExtensionsResponse (Core.Maybe [Types.SchemaExtensionInfo])
lserrsSchemaExtensionsInfo = Lens.field @"schemaExtensionsInfo"
{-# DEPRECATED lserrsSchemaExtensionsInfo "Use generic-lens or generic-optics with 'schemaExtensionsInfo' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lserrsResponseStatus :: Lens.Lens' ListSchemaExtensionsResponse Core.Int
lserrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lserrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
