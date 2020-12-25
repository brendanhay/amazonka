{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.DeleteAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more custom attributes from an Amazon ECS resource.
module Network.AWS.ECS.DeleteAttributes
  ( -- * Creating a request
    DeleteAttributes (..),
    mkDeleteAttributes,

    -- ** Request lenses
    daAttributes,
    daCluster,

    -- * Destructuring the response
    DeleteAttributesResponse (..),
    mkDeleteAttributesResponse,

    -- ** Response lenses
    darrsAttributes,
    darrsResponseStatus,
  )
where

import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteAttributes' smart constructor.
data DeleteAttributes = DeleteAttributes'
  { -- | The attributes to delete from your resource. You can specify up to 10 attributes per request. For custom attributes, specify the attribute name and target ID, but do not specify the value. If you specify the target ID using the short form, you must also specify the target type.
    attributes :: [Types.Attribute],
    -- | The short name or full Amazon Resource Name (ARN) of the cluster that contains the resource to delete attributes. If you do not specify a cluster, the default cluster is assumed.
    cluster :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAttributes' value with any optional fields omitted.
mkDeleteAttributes ::
  DeleteAttributes
mkDeleteAttributes =
  DeleteAttributes'
    { attributes = Core.mempty,
      cluster = Core.Nothing
    }

-- | The attributes to delete from your resource. You can specify up to 10 attributes per request. For custom attributes, specify the attribute name and target ID, but do not specify the value. If you specify the target ID using the short form, you must also specify the target type.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAttributes :: Lens.Lens' DeleteAttributes [Types.Attribute]
daAttributes = Lens.field @"attributes"
{-# DEPRECATED daAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The short name or full Amazon Resource Name (ARN) of the cluster that contains the resource to delete attributes. If you do not specify a cluster, the default cluster is assumed.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daCluster :: Lens.Lens' DeleteAttributes (Core.Maybe Types.String)
daCluster = Lens.field @"cluster"
{-# DEPRECATED daCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

instance Core.FromJSON DeleteAttributes where
  toJSON DeleteAttributes {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("attributes" Core..= attributes),
            ("cluster" Core..=) Core.<$> cluster
          ]
      )

instance Core.AWSRequest DeleteAttributes where
  type Rs DeleteAttributes = DeleteAttributesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AmazonEC2ContainerServiceV20141113.DeleteAttributes"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteAttributesResponse'
            Core.<$> (x Core..:? "attributes") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteAttributesResponse' smart constructor.
data DeleteAttributesResponse = DeleteAttributesResponse'
  { -- | A list of attribute objects that were successfully deleted from your resource.
    attributes :: Core.Maybe [Types.Attribute],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAttributesResponse' value with any optional fields omitted.
mkDeleteAttributesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteAttributesResponse
mkDeleteAttributesResponse responseStatus =
  DeleteAttributesResponse'
    { attributes = Core.Nothing,
      responseStatus
    }

-- | A list of attribute objects that were successfully deleted from your resource.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsAttributes :: Lens.Lens' DeleteAttributesResponse (Core.Maybe [Types.Attribute])
darrsAttributes = Lens.field @"attributes"
{-# DEPRECATED darrsAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsResponseStatus :: Lens.Lens' DeleteAttributesResponse Core.Int
darrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED darrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
