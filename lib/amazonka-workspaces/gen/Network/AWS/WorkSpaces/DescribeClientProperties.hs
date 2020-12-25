{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.DescribeClientProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes one or more specified Amazon WorkSpaces clients.
module Network.AWS.WorkSpaces.DescribeClientProperties
  ( -- * Creating a request
    DescribeClientProperties (..),
    mkDescribeClientProperties,

    -- ** Request lenses
    dcpResourceIds,

    -- * Destructuring the response
    DescribeClientPropertiesResponse (..),
    mkDescribeClientPropertiesResponse,

    -- ** Response lenses
    dcprrsClientPropertiesList,
    dcprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkDescribeClientProperties' smart constructor.
newtype DescribeClientProperties = DescribeClientProperties'
  { -- | The resource identifier, in the form of directory IDs.
    resourceIds :: Core.NonEmpty Types.NonEmptyString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeClientProperties' value with any optional fields omitted.
mkDescribeClientProperties ::
  -- | 'resourceIds'
  Core.NonEmpty Types.NonEmptyString ->
  DescribeClientProperties
mkDescribeClientProperties resourceIds =
  DescribeClientProperties' {resourceIds}

-- | The resource identifier, in the form of directory IDs.
--
-- /Note:/ Consider using 'resourceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpResourceIds :: Lens.Lens' DescribeClientProperties (Core.NonEmpty Types.NonEmptyString)
dcpResourceIds = Lens.field @"resourceIds"
{-# DEPRECATED dcpResourceIds "Use generic-lens or generic-optics with 'resourceIds' instead." #-}

instance Core.FromJSON DescribeClientProperties where
  toJSON DescribeClientProperties {..} =
    Core.object
      (Core.catMaybes [Core.Just ("ResourceIds" Core..= resourceIds)])

instance Core.AWSRequest DescribeClientProperties where
  type Rs DescribeClientProperties = DescribeClientPropertiesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "WorkspacesService.DescribeClientProperties")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeClientPropertiesResponse'
            Core.<$> (x Core..:? "ClientPropertiesList")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeClientPropertiesResponse' smart constructor.
data DescribeClientPropertiesResponse = DescribeClientPropertiesResponse'
  { -- | Information about the specified Amazon WorkSpaces clients.
    clientPropertiesList :: Core.Maybe [Types.ClientPropertiesResult],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeClientPropertiesResponse' value with any optional fields omitted.
mkDescribeClientPropertiesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeClientPropertiesResponse
mkDescribeClientPropertiesResponse responseStatus =
  DescribeClientPropertiesResponse'
    { clientPropertiesList =
        Core.Nothing,
      responseStatus
    }

-- | Information about the specified Amazon WorkSpaces clients.
--
-- /Note:/ Consider using 'clientPropertiesList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprrsClientPropertiesList :: Lens.Lens' DescribeClientPropertiesResponse (Core.Maybe [Types.ClientPropertiesResult])
dcprrsClientPropertiesList = Lens.field @"clientPropertiesList"
{-# DEPRECATED dcprrsClientPropertiesList "Use generic-lens or generic-optics with 'clientPropertiesList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprrsResponseStatus :: Lens.Lens' DescribeClientPropertiesResponse Core.Int
dcprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
