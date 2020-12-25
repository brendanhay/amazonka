{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreateThingType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new thing type.
module Network.AWS.IoT.CreateThingType
  ( -- * Creating a request
    CreateThingType (..),
    mkCreateThingType,

    -- ** Request lenses
    cttThingTypeName,
    cttTags,
    cttThingTypeProperties,

    -- * Destructuring the response
    CreateThingTypeResponse (..),
    mkCreateThingTypeResponse,

    -- ** Response lenses
    cttrrsThingTypeArn,
    cttrrsThingTypeId,
    cttrrsThingTypeName,
    cttrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the CreateThingType operation.
--
-- /See:/ 'mkCreateThingType' smart constructor.
data CreateThingType = CreateThingType'
  { -- | The name of the thing type.
    thingTypeName :: Types.ThingTypeName,
    -- | Metadata which can be used to manage the thing type.
    tags :: Core.Maybe [Types.Tag],
    -- | The ThingTypeProperties for the thing type to create. It contains information about the new thing type including a description, and a list of searchable thing attribute names.
    thingTypeProperties :: Core.Maybe Types.ThingTypeProperties
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateThingType' value with any optional fields omitted.
mkCreateThingType ::
  -- | 'thingTypeName'
  Types.ThingTypeName ->
  CreateThingType
mkCreateThingType thingTypeName =
  CreateThingType'
    { thingTypeName,
      tags = Core.Nothing,
      thingTypeProperties = Core.Nothing
    }

-- | The name of the thing type.
--
-- /Note:/ Consider using 'thingTypeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cttThingTypeName :: Lens.Lens' CreateThingType Types.ThingTypeName
cttThingTypeName = Lens.field @"thingTypeName"
{-# DEPRECATED cttThingTypeName "Use generic-lens or generic-optics with 'thingTypeName' instead." #-}

-- | Metadata which can be used to manage the thing type.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cttTags :: Lens.Lens' CreateThingType (Core.Maybe [Types.Tag])
cttTags = Lens.field @"tags"
{-# DEPRECATED cttTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The ThingTypeProperties for the thing type to create. It contains information about the new thing type including a description, and a list of searchable thing attribute names.
--
-- /Note:/ Consider using 'thingTypeProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cttThingTypeProperties :: Lens.Lens' CreateThingType (Core.Maybe Types.ThingTypeProperties)
cttThingTypeProperties = Lens.field @"thingTypeProperties"
{-# DEPRECATED cttThingTypeProperties "Use generic-lens or generic-optics with 'thingTypeProperties' instead." #-}

instance Core.FromJSON CreateThingType where
  toJSON CreateThingType {..} =
    Core.object
      ( Core.catMaybes
          [ ("tags" Core..=) Core.<$> tags,
            ("thingTypeProperties" Core..=) Core.<$> thingTypeProperties
          ]
      )

instance Core.AWSRequest CreateThingType where
  type Rs CreateThingType = CreateThingTypeResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath ("/thing-types/" Core.<> (Core.toText thingTypeName)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateThingTypeResponse'
            Core.<$> (x Core..:? "thingTypeArn")
            Core.<*> (x Core..:? "thingTypeId")
            Core.<*> (x Core..:? "thingTypeName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The output of the CreateThingType operation.
--
-- /See:/ 'mkCreateThingTypeResponse' smart constructor.
data CreateThingTypeResponse = CreateThingTypeResponse'
  { -- | The Amazon Resource Name (ARN) of the thing type.
    thingTypeArn :: Core.Maybe Types.ThingTypeArn,
    -- | The thing type ID.
    thingTypeId :: Core.Maybe Types.ThingTypeId,
    -- | The name of the thing type.
    thingTypeName :: Core.Maybe Types.ThingTypeName,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateThingTypeResponse' value with any optional fields omitted.
mkCreateThingTypeResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateThingTypeResponse
mkCreateThingTypeResponse responseStatus =
  CreateThingTypeResponse'
    { thingTypeArn = Core.Nothing,
      thingTypeId = Core.Nothing,
      thingTypeName = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name (ARN) of the thing type.
--
-- /Note:/ Consider using 'thingTypeArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cttrrsThingTypeArn :: Lens.Lens' CreateThingTypeResponse (Core.Maybe Types.ThingTypeArn)
cttrrsThingTypeArn = Lens.field @"thingTypeArn"
{-# DEPRECATED cttrrsThingTypeArn "Use generic-lens or generic-optics with 'thingTypeArn' instead." #-}

-- | The thing type ID.
--
-- /Note:/ Consider using 'thingTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cttrrsThingTypeId :: Lens.Lens' CreateThingTypeResponse (Core.Maybe Types.ThingTypeId)
cttrrsThingTypeId = Lens.field @"thingTypeId"
{-# DEPRECATED cttrrsThingTypeId "Use generic-lens or generic-optics with 'thingTypeId' instead." #-}

-- | The name of the thing type.
--
-- /Note:/ Consider using 'thingTypeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cttrrsThingTypeName :: Lens.Lens' CreateThingTypeResponse (Core.Maybe Types.ThingTypeName)
cttrrsThingTypeName = Lens.field @"thingTypeName"
{-# DEPRECATED cttrrsThingTypeName "Use generic-lens or generic-optics with 'thingTypeName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cttrrsResponseStatus :: Lens.Lens' CreateThingTypeResponse Core.Int
cttrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cttrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
