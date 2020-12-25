{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.DescribeInstanceAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified instance attribute.
module Network.AWS.Connect.DescribeInstanceAttribute
  ( -- * Creating a request
    DescribeInstanceAttribute (..),
    mkDescribeInstanceAttribute,

    -- ** Request lenses
    diaInstanceId,
    diaAttributeType,

    -- * Destructuring the response
    DescribeInstanceAttributeResponse (..),
    mkDescribeInstanceAttributeResponse,

    -- ** Response lenses
    diarrsAttribute,
    diarrsResponseStatus,
  )
where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeInstanceAttribute' smart constructor.
data DescribeInstanceAttribute = DescribeInstanceAttribute'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Types.InstanceId,
    -- | The type of attribute.
    attributeType :: Types.InstanceAttributeType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeInstanceAttribute' value with any optional fields omitted.
mkDescribeInstanceAttribute ::
  -- | 'instanceId'
  Types.InstanceId ->
  -- | 'attributeType'
  Types.InstanceAttributeType ->
  DescribeInstanceAttribute
mkDescribeInstanceAttribute instanceId attributeType =
  DescribeInstanceAttribute' {instanceId, attributeType}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diaInstanceId :: Lens.Lens' DescribeInstanceAttribute Types.InstanceId
diaInstanceId = Lens.field @"instanceId"
{-# DEPRECATED diaInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The type of attribute.
--
-- /Note:/ Consider using 'attributeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diaAttributeType :: Lens.Lens' DescribeInstanceAttribute Types.InstanceAttributeType
diaAttributeType = Lens.field @"attributeType"
{-# DEPRECATED diaAttributeType "Use generic-lens or generic-optics with 'attributeType' instead." #-}

instance Core.AWSRequest DescribeInstanceAttribute where
  type
    Rs DescribeInstanceAttribute =
      DescribeInstanceAttributeResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/instance/" Core.<> (Core.toText instanceId)
                Core.<> ("/attribute/")
                Core.<> (Core.toText attributeType)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeInstanceAttributeResponse'
            Core.<$> (x Core..:? "Attribute") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeInstanceAttributeResponse' smart constructor.
data DescribeInstanceAttributeResponse = DescribeInstanceAttributeResponse'
  { -- | The type of attribute.
    attribute :: Core.Maybe Types.Attribute,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeInstanceAttributeResponse' value with any optional fields omitted.
mkDescribeInstanceAttributeResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeInstanceAttributeResponse
mkDescribeInstanceAttributeResponse responseStatus =
  DescribeInstanceAttributeResponse'
    { attribute = Core.Nothing,
      responseStatus
    }

-- | The type of attribute.
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarrsAttribute :: Lens.Lens' DescribeInstanceAttributeResponse (Core.Maybe Types.Attribute)
diarrsAttribute = Lens.field @"attribute"
{-# DEPRECATED diarrsAttribute "Use generic-lens or generic-optics with 'attribute' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarrsResponseStatus :: Lens.Lens' DescribeInstanceAttributeResponse Core.Int
diarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED diarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
