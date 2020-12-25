{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.GetSlotType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific version of a slot type. In addition to specifying the slot type name, you must specify the slot type version.
--
-- This operation requires permissions for the @lex:GetSlotType@ action.
module Network.AWS.LexModels.GetSlotType
  ( -- * Creating a request
    GetSlotType (..),
    mkGetSlotType,

    -- ** Request lenses
    gstName,
    gstVersion,

    -- * Destructuring the response
    GetSlotTypeResponse (..),
    mkGetSlotTypeResponse,

    -- ** Response lenses
    gstrfrsChecksum,
    gstrfrsCreatedDate,
    gstrfrsDescription,
    gstrfrsEnumerationValues,
    gstrfrsLastUpdatedDate,
    gstrfrsName,
    gstrfrsParentSlotTypeSignature,
    gstrfrsSlotTypeConfigurations,
    gstrfrsValueSelectionStrategy,
    gstrfrsVersion,
    gstrfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetSlotType' smart constructor.
data GetSlotType = GetSlotType'
  { -- | The name of the slot type. The name is case sensitive.
    name :: Types.SlotTypeName,
    -- | The version of the slot type.
    version :: Types.Version
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSlotType' value with any optional fields omitted.
mkGetSlotType ::
  -- | 'name'
  Types.SlotTypeName ->
  -- | 'version'
  Types.Version ->
  GetSlotType
mkGetSlotType name version = GetSlotType' {name, version}

-- | The name of the slot type. The name is case sensitive.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstName :: Lens.Lens' GetSlotType Types.SlotTypeName
gstName = Lens.field @"name"
{-# DEPRECATED gstName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version of the slot type.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstVersion :: Lens.Lens' GetSlotType Types.Version
gstVersion = Lens.field @"version"
{-# DEPRECATED gstVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Core.AWSRequest GetSlotType where
  type Rs GetSlotType = GetSlotTypeResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/slottypes/" Core.<> (Core.toText name) Core.<> ("/versions/")
                Core.<> (Core.toText version)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSlotTypeResponse'
            Core.<$> (x Core..:? "checksum")
            Core.<*> (x Core..:? "createdDate")
            Core.<*> (x Core..:? "description")
            Core.<*> (x Core..:? "enumerationValues")
            Core.<*> (x Core..:? "lastUpdatedDate")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "parentSlotTypeSignature")
            Core.<*> (x Core..:? "slotTypeConfigurations")
            Core.<*> (x Core..:? "valueSelectionStrategy")
            Core.<*> (x Core..:? "version")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetSlotTypeResponse' smart constructor.
data GetSlotTypeResponse = GetSlotTypeResponse'
  { -- | Checksum of the @> LATEST@ version of the slot type.
    checksum :: Core.Maybe Types.String,
    -- | The date that the slot type was created.
    createdDate :: Core.Maybe Core.NominalDiffTime,
    -- | A description of the slot type.
    description :: Core.Maybe Types.Description,
    -- | A list of @EnumerationValue@ objects that defines the values that the slot type can take.
    enumerationValues :: Core.Maybe [Types.EnumerationValue],
    -- | The date that the slot type was updated. When you create a resource, the creation date and last update date are the same.
    lastUpdatedDate :: Core.Maybe Core.NominalDiffTime,
    -- | The name of the slot type.
    name :: Core.Maybe Types.SlotTypeName,
    -- | The built-in slot type used as a parent for the slot type.
    parentSlotTypeSignature :: Core.Maybe Types.CustomOrBuiltinSlotTypeName,
    -- | Configuration information that extends the parent built-in slot type.
    slotTypeConfigurations :: Core.Maybe [Types.SlotTypeConfiguration],
    -- | The strategy that Amazon Lex uses to determine the value of the slot. For more information, see 'PutSlotType' .
    valueSelectionStrategy :: Core.Maybe Types.SlotValueSelectionStrategy,
    -- | The version of the slot type.
    version :: Core.Maybe Types.Version,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetSlotTypeResponse' value with any optional fields omitted.
mkGetSlotTypeResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetSlotTypeResponse
mkGetSlotTypeResponse responseStatus =
  GetSlotTypeResponse'
    { checksum = Core.Nothing,
      createdDate = Core.Nothing,
      description = Core.Nothing,
      enumerationValues = Core.Nothing,
      lastUpdatedDate = Core.Nothing,
      name = Core.Nothing,
      parentSlotTypeSignature = Core.Nothing,
      slotTypeConfigurations = Core.Nothing,
      valueSelectionStrategy = Core.Nothing,
      version = Core.Nothing,
      responseStatus
    }

-- | Checksum of the @> LATEST@ version of the slot type.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstrfrsChecksum :: Lens.Lens' GetSlotTypeResponse (Core.Maybe Types.String)
gstrfrsChecksum = Lens.field @"checksum"
{-# DEPRECATED gstrfrsChecksum "Use generic-lens or generic-optics with 'checksum' instead." #-}

-- | The date that the slot type was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstrfrsCreatedDate :: Lens.Lens' GetSlotTypeResponse (Core.Maybe Core.NominalDiffTime)
gstrfrsCreatedDate = Lens.field @"createdDate"
{-# DEPRECATED gstrfrsCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | A description of the slot type.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstrfrsDescription :: Lens.Lens' GetSlotTypeResponse (Core.Maybe Types.Description)
gstrfrsDescription = Lens.field @"description"
{-# DEPRECATED gstrfrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A list of @EnumerationValue@ objects that defines the values that the slot type can take.
--
-- /Note:/ Consider using 'enumerationValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstrfrsEnumerationValues :: Lens.Lens' GetSlotTypeResponse (Core.Maybe [Types.EnumerationValue])
gstrfrsEnumerationValues = Lens.field @"enumerationValues"
{-# DEPRECATED gstrfrsEnumerationValues "Use generic-lens or generic-optics with 'enumerationValues' instead." #-}

-- | The date that the slot type was updated. When you create a resource, the creation date and last update date are the same.
--
-- /Note:/ Consider using 'lastUpdatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstrfrsLastUpdatedDate :: Lens.Lens' GetSlotTypeResponse (Core.Maybe Core.NominalDiffTime)
gstrfrsLastUpdatedDate = Lens.field @"lastUpdatedDate"
{-# DEPRECATED gstrfrsLastUpdatedDate "Use generic-lens or generic-optics with 'lastUpdatedDate' instead." #-}

-- | The name of the slot type.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstrfrsName :: Lens.Lens' GetSlotTypeResponse (Core.Maybe Types.SlotTypeName)
gstrfrsName = Lens.field @"name"
{-# DEPRECATED gstrfrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The built-in slot type used as a parent for the slot type.
--
-- /Note:/ Consider using 'parentSlotTypeSignature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstrfrsParentSlotTypeSignature :: Lens.Lens' GetSlotTypeResponse (Core.Maybe Types.CustomOrBuiltinSlotTypeName)
gstrfrsParentSlotTypeSignature = Lens.field @"parentSlotTypeSignature"
{-# DEPRECATED gstrfrsParentSlotTypeSignature "Use generic-lens or generic-optics with 'parentSlotTypeSignature' instead." #-}

-- | Configuration information that extends the parent built-in slot type.
--
-- /Note:/ Consider using 'slotTypeConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstrfrsSlotTypeConfigurations :: Lens.Lens' GetSlotTypeResponse (Core.Maybe [Types.SlotTypeConfiguration])
gstrfrsSlotTypeConfigurations = Lens.field @"slotTypeConfigurations"
{-# DEPRECATED gstrfrsSlotTypeConfigurations "Use generic-lens or generic-optics with 'slotTypeConfigurations' instead." #-}

-- | The strategy that Amazon Lex uses to determine the value of the slot. For more information, see 'PutSlotType' .
--
-- /Note:/ Consider using 'valueSelectionStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstrfrsValueSelectionStrategy :: Lens.Lens' GetSlotTypeResponse (Core.Maybe Types.SlotValueSelectionStrategy)
gstrfrsValueSelectionStrategy = Lens.field @"valueSelectionStrategy"
{-# DEPRECATED gstrfrsValueSelectionStrategy "Use generic-lens or generic-optics with 'valueSelectionStrategy' instead." #-}

-- | The version of the slot type.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstrfrsVersion :: Lens.Lens' GetSlotTypeResponse (Core.Maybe Types.Version)
gstrfrsVersion = Lens.field @"version"
{-# DEPRECATED gstrfrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstrfrsResponseStatus :: Lens.Lens' GetSlotTypeResponse Core.Int
gstrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gstrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
