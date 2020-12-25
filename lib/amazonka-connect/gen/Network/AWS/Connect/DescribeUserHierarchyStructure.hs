{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.DescribeUserHierarchyStructure
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the hierarchy structure of the specified Amazon Connect instance.
module Network.AWS.Connect.DescribeUserHierarchyStructure
  ( -- * Creating a request
    DescribeUserHierarchyStructure (..),
    mkDescribeUserHierarchyStructure,

    -- ** Request lenses
    duhsInstanceId,

    -- * Destructuring the response
    DescribeUserHierarchyStructureResponse (..),
    mkDescribeUserHierarchyStructureResponse,

    -- ** Response lenses
    duhsrrsHierarchyStructure,
    duhsrrsResponseStatus,
  )
where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeUserHierarchyStructure' smart constructor.
newtype DescribeUserHierarchyStructure = DescribeUserHierarchyStructure'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Types.InstanceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeUserHierarchyStructure' value with any optional fields omitted.
mkDescribeUserHierarchyStructure ::
  -- | 'instanceId'
  Types.InstanceId ->
  DescribeUserHierarchyStructure
mkDescribeUserHierarchyStructure instanceId =
  DescribeUserHierarchyStructure' {instanceId}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duhsInstanceId :: Lens.Lens' DescribeUserHierarchyStructure Types.InstanceId
duhsInstanceId = Lens.field @"instanceId"
{-# DEPRECATED duhsInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Core.AWSRequest DescribeUserHierarchyStructure where
  type
    Rs DescribeUserHierarchyStructure =
      DescribeUserHierarchyStructureResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ("/user-hierarchy-structure/" Core.<> (Core.toText instanceId)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeUserHierarchyStructureResponse'
            Core.<$> (x Core..:? "HierarchyStructure")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeUserHierarchyStructureResponse' smart constructor.
data DescribeUserHierarchyStructureResponse = DescribeUserHierarchyStructureResponse'
  { -- | Information about the hierarchy structure.
    hierarchyStructure :: Core.Maybe Types.HierarchyStructure,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeUserHierarchyStructureResponse' value with any optional fields omitted.
mkDescribeUserHierarchyStructureResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeUserHierarchyStructureResponse
mkDescribeUserHierarchyStructureResponse responseStatus =
  DescribeUserHierarchyStructureResponse'
    { hierarchyStructure =
        Core.Nothing,
      responseStatus
    }

-- | Information about the hierarchy structure.
--
-- /Note:/ Consider using 'hierarchyStructure' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duhsrrsHierarchyStructure :: Lens.Lens' DescribeUserHierarchyStructureResponse (Core.Maybe Types.HierarchyStructure)
duhsrrsHierarchyStructure = Lens.field @"hierarchyStructure"
{-# DEPRECATED duhsrrsHierarchyStructure "Use generic-lens or generic-optics with 'hierarchyStructure' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duhsrrsResponseStatus :: Lens.Lens' DescribeUserHierarchyStructureResponse Core.Int
duhsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED duhsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
