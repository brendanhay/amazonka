{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreateDimension
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a dimension that you can use to limit the scope of a metric used in a security profile for AWS IoT Device Defender. For example, using a @TOPIC_FILTER@ dimension, you can narrow down the scope of the metric only to MQTT topics whose name match the pattern specified in the dimension.
module Network.AWS.IoT.CreateDimension
  ( -- * Creating a request
    CreateDimension (..),
    mkCreateDimension,

    -- ** Request lenses
    cdName,
    cdType,
    cdStringValues,
    cdClientRequestToken,
    cdTags,

    -- * Destructuring the response
    CreateDimensionResponse (..),
    mkCreateDimensionResponse,

    -- ** Response lenses
    cdrrsArn,
    cdrrsName,
    cdrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateDimension' smart constructor.
data CreateDimension = CreateDimension'
  { -- | A unique identifier for the dimension. Choose something that describes the type and value to make it easy to remember what it does.
    name :: Types.Name,
    -- | Specifies the type of dimension. Supported types: @TOPIC_FILTER.@
    type' :: Types.DimensionType,
    -- | Specifies the value or list of values for the dimension. For @TOPIC_FILTER@ dimensions, this is a pattern used to match the MQTT topic (for example, "admin/#").
    stringValues :: Core.NonEmpty Types.DimensionStringValue,
    -- | Each dimension must have a unique client request token. If you try to create a new dimension with the same token as a dimension that already exists, an exception occurs. If you omit this value, AWS SDKs will automatically generate a unique client request.
    clientRequestToken :: Types.ClientRequestToken,
    -- | Metadata that can be used to manage the dimension.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDimension' value with any optional fields omitted.
mkCreateDimension ::
  -- | 'name'
  Types.Name ->
  -- | 'type\''
  Types.DimensionType ->
  -- | 'stringValues'
  Core.NonEmpty Types.DimensionStringValue ->
  -- | 'clientRequestToken'
  Types.ClientRequestToken ->
  CreateDimension
mkCreateDimension name type' stringValues clientRequestToken =
  CreateDimension'
    { name,
      type',
      stringValues,
      clientRequestToken,
      tags = Core.Nothing
    }

-- | A unique identifier for the dimension. Choose something that describes the type and value to make it easy to remember what it does.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdName :: Lens.Lens' CreateDimension Types.Name
cdName = Lens.field @"name"
{-# DEPRECATED cdName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Specifies the type of dimension. Supported types: @TOPIC_FILTER.@
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdType :: Lens.Lens' CreateDimension Types.DimensionType
cdType = Lens.field @"type'"
{-# DEPRECATED cdType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | Specifies the value or list of values for the dimension. For @TOPIC_FILTER@ dimensions, this is a pattern used to match the MQTT topic (for example, "admin/#").
--
-- /Note:/ Consider using 'stringValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdStringValues :: Lens.Lens' CreateDimension (Core.NonEmpty Types.DimensionStringValue)
cdStringValues = Lens.field @"stringValues"
{-# DEPRECATED cdStringValues "Use generic-lens or generic-optics with 'stringValues' instead." #-}

-- | Each dimension must have a unique client request token. If you try to create a new dimension with the same token as a dimension that already exists, an exception occurs. If you omit this value, AWS SDKs will automatically generate a unique client request.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdClientRequestToken :: Lens.Lens' CreateDimension Types.ClientRequestToken
cdClientRequestToken = Lens.field @"clientRequestToken"
{-# DEPRECATED cdClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | Metadata that can be used to manage the dimension.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdTags :: Lens.Lens' CreateDimension (Core.Maybe [Types.Tag])
cdTags = Lens.field @"tags"
{-# DEPRECATED cdTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateDimension where
  toJSON CreateDimension {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("type" Core..= type'),
            Core.Just ("stringValues" Core..= stringValues),
            Core.Just ("clientRequestToken" Core..= clientRequestToken),
            ("tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateDimension where
  type Rs CreateDimension = CreateDimensionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath ("/dimensions/" Core.<> (Core.toText name)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDimensionResponse'
            Core.<$> (x Core..:? "arn")
            Core.<*> (x Core..:? "name")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateDimensionResponse' smart constructor.
data CreateDimensionResponse = CreateDimensionResponse'
  { -- | The ARN (Amazon resource name) of the created dimension.
    arn :: Core.Maybe Types.DimensionArn,
    -- | A unique identifier for the dimension.
    name :: Core.Maybe Types.DimensionName,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDimensionResponse' value with any optional fields omitted.
mkCreateDimensionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateDimensionResponse
mkCreateDimensionResponse responseStatus =
  CreateDimensionResponse'
    { arn = Core.Nothing,
      name = Core.Nothing,
      responseStatus
    }

-- | The ARN (Amazon resource name) of the created dimension.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrrsArn :: Lens.Lens' CreateDimensionResponse (Core.Maybe Types.DimensionArn)
cdrrsArn = Lens.field @"arn"
{-# DEPRECATED cdrrsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | A unique identifier for the dimension.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrrsName :: Lens.Lens' CreateDimensionResponse (Core.Maybe Types.DimensionName)
cdrrsName = Lens.field @"name"
{-# DEPRECATED cdrrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrrsResponseStatus :: Lens.Lens' CreateDimensionResponse Core.Int
cdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
