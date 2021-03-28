{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.CreateSlotTypeVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new version of a slot type based on the @> LATEST@ version of the specified slot type. If the @> LATEST@ version of this resource has not changed since the last version that you created, Amazon Lex doesn't create a new version. It returns the last version that you created. 
--
-- When you create a version of a slot type, Amazon Lex sets the version to 1. Subsequent versions increment by 1. For more information, see 'versioning-intro' . 
-- This operation requires permissions for the @lex:CreateSlotTypeVersion@ action.
module Network.AWS.LexModels.CreateSlotTypeVersion
    (
    -- * Creating a request
      CreateSlotTypeVersion (..)
    , mkCreateSlotTypeVersion
    -- ** Request lenses
    , cstvName
    , cstvChecksum

    -- * Destructuring the response
    , CreateSlotTypeVersionResponse (..)
    , mkCreateSlotTypeVersionResponse
    -- ** Response lenses
    , cstvrrsChecksum
    , cstvrrsCreatedDate
    , cstvrrsDescription
    , cstvrrsEnumerationValues
    , cstvrrsLastUpdatedDate
    , cstvrrsName
    , cstvrrsParentSlotTypeSignature
    , cstvrrsSlotTypeConfigurations
    , cstvrrsValueSelectionStrategy
    , cstvrrsVersion
    , cstvrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateSlotTypeVersion' smart constructor.
data CreateSlotTypeVersion = CreateSlotTypeVersion'
  { name :: Types.Name
    -- ^ The name of the slot type that you want to create a new version for. The name is case sensitive. 
  , checksum :: Core.Maybe Core.Text
    -- ^ Checksum for the @> LATEST@ version of the slot type that you want to publish. If you specify a checksum and the @> LATEST@ version of the slot type has a different checksum, Amazon Lex returns a @PreconditionFailedException@ exception and doesn't publish the new version. If you don't specify a checksum, Amazon Lex publishes the @> LATEST@ version.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSlotTypeVersion' value with any optional fields omitted.
mkCreateSlotTypeVersion
    :: Types.Name -- ^ 'name'
    -> CreateSlotTypeVersion
mkCreateSlotTypeVersion name
  = CreateSlotTypeVersion'{name, checksum = Core.Nothing}

-- | The name of the slot type that you want to create a new version for. The name is case sensitive. 
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cstvName :: Lens.Lens' CreateSlotTypeVersion Types.Name
cstvName = Lens.field @"name"
{-# INLINEABLE cstvName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Checksum for the @> LATEST@ version of the slot type that you want to publish. If you specify a checksum and the @> LATEST@ version of the slot type has a different checksum, Amazon Lex returns a @PreconditionFailedException@ exception and doesn't publish the new version. If you don't specify a checksum, Amazon Lex publishes the @> LATEST@ version.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cstvChecksum :: Lens.Lens' CreateSlotTypeVersion (Core.Maybe Core.Text)
cstvChecksum = Lens.field @"checksum"
{-# INLINEABLE cstvChecksum #-}
{-# DEPRECATED checksum "Use generic-lens or generic-optics with 'checksum' instead"  #-}

instance Core.ToQuery CreateSlotTypeVersion where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateSlotTypeVersion where
        toHeaders CreateSlotTypeVersion{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateSlotTypeVersion where
        toJSON CreateSlotTypeVersion{..}
          = Core.object
              (Core.catMaybes [("checksum" Core..=) Core.<$> checksum])

instance Core.AWSRequest CreateSlotTypeVersion where
        type Rs CreateSlotTypeVersion = CreateSlotTypeVersionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/slottypes/" Core.<> Core.toText name Core.<> "/versions",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateSlotTypeVersionResponse' Core.<$>
                   (x Core..:? "checksum") Core.<*> x Core..:? "createdDate" Core.<*>
                     x Core..:? "description"
                     Core.<*> x Core..:? "enumerationValues"
                     Core.<*> x Core..:? "lastUpdatedDate"
                     Core.<*> x Core..:? "name"
                     Core.<*> x Core..:? "parentSlotTypeSignature"
                     Core.<*> x Core..:? "slotTypeConfigurations"
                     Core.<*> x Core..:? "valueSelectionStrategy"
                     Core.<*> x Core..:? "version"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateSlotTypeVersionResponse' smart constructor.
data CreateSlotTypeVersionResponse = CreateSlotTypeVersionResponse'
  { checksum :: Core.Maybe Core.Text
    -- ^ Checksum of the @> LATEST@ version of the slot type.
  , createdDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date that the slot type was created.
  , description :: Core.Maybe Types.Description
    -- ^ A description of the slot type.
  , enumerationValues :: Core.Maybe [Types.EnumerationValue]
    -- ^ A list of @EnumerationValue@ objects that defines the values that the slot type can take.
  , lastUpdatedDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date that the slot type was updated. When you create a resource, the creation date and last update date are the same.
  , name :: Core.Maybe Types.SlotTypeName
    -- ^ The name of the slot type.
  , parentSlotTypeSignature :: Core.Maybe Types.CustomOrBuiltinSlotTypeName
    -- ^ The built-in slot type used a the parent of the slot type.
  , slotTypeConfigurations :: Core.Maybe [Types.SlotTypeConfiguration]
    -- ^ Configuration information that extends the parent built-in slot type.
  , valueSelectionStrategy :: Core.Maybe Types.SlotValueSelectionStrategy
    -- ^ The strategy that Amazon Lex uses to determine the value of the slot. For more information, see 'PutSlotType' .
  , version :: Core.Maybe Types.Version
    -- ^ The version assigned to the new slot type version. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateSlotTypeVersionResponse' value with any optional fields omitted.
mkCreateSlotTypeVersionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateSlotTypeVersionResponse
mkCreateSlotTypeVersionResponse responseStatus
  = CreateSlotTypeVersionResponse'{checksum = Core.Nothing,
                                   createdDate = Core.Nothing, description = Core.Nothing,
                                   enumerationValues = Core.Nothing, lastUpdatedDate = Core.Nothing,
                                   name = Core.Nothing, parentSlotTypeSignature = Core.Nothing,
                                   slotTypeConfigurations = Core.Nothing,
                                   valueSelectionStrategy = Core.Nothing, version = Core.Nothing,
                                   responseStatus}

-- | Checksum of the @> LATEST@ version of the slot type.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cstvrrsChecksum :: Lens.Lens' CreateSlotTypeVersionResponse (Core.Maybe Core.Text)
cstvrrsChecksum = Lens.field @"checksum"
{-# INLINEABLE cstvrrsChecksum #-}
{-# DEPRECATED checksum "Use generic-lens or generic-optics with 'checksum' instead"  #-}

-- | The date that the slot type was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cstvrrsCreatedDate :: Lens.Lens' CreateSlotTypeVersionResponse (Core.Maybe Core.NominalDiffTime)
cstvrrsCreatedDate = Lens.field @"createdDate"
{-# INLINEABLE cstvrrsCreatedDate #-}
{-# DEPRECATED createdDate "Use generic-lens or generic-optics with 'createdDate' instead"  #-}

-- | A description of the slot type.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cstvrrsDescription :: Lens.Lens' CreateSlotTypeVersionResponse (Core.Maybe Types.Description)
cstvrrsDescription = Lens.field @"description"
{-# INLINEABLE cstvrrsDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | A list of @EnumerationValue@ objects that defines the values that the slot type can take.
--
-- /Note:/ Consider using 'enumerationValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cstvrrsEnumerationValues :: Lens.Lens' CreateSlotTypeVersionResponse (Core.Maybe [Types.EnumerationValue])
cstvrrsEnumerationValues = Lens.field @"enumerationValues"
{-# INLINEABLE cstvrrsEnumerationValues #-}
{-# DEPRECATED enumerationValues "Use generic-lens or generic-optics with 'enumerationValues' instead"  #-}

-- | The date that the slot type was updated. When you create a resource, the creation date and last update date are the same.
--
-- /Note:/ Consider using 'lastUpdatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cstvrrsLastUpdatedDate :: Lens.Lens' CreateSlotTypeVersionResponse (Core.Maybe Core.NominalDiffTime)
cstvrrsLastUpdatedDate = Lens.field @"lastUpdatedDate"
{-# INLINEABLE cstvrrsLastUpdatedDate #-}
{-# DEPRECATED lastUpdatedDate "Use generic-lens or generic-optics with 'lastUpdatedDate' instead"  #-}

-- | The name of the slot type.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cstvrrsName :: Lens.Lens' CreateSlotTypeVersionResponse (Core.Maybe Types.SlotTypeName)
cstvrrsName = Lens.field @"name"
{-# INLINEABLE cstvrrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The built-in slot type used a the parent of the slot type.
--
-- /Note:/ Consider using 'parentSlotTypeSignature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cstvrrsParentSlotTypeSignature :: Lens.Lens' CreateSlotTypeVersionResponse (Core.Maybe Types.CustomOrBuiltinSlotTypeName)
cstvrrsParentSlotTypeSignature = Lens.field @"parentSlotTypeSignature"
{-# INLINEABLE cstvrrsParentSlotTypeSignature #-}
{-# DEPRECATED parentSlotTypeSignature "Use generic-lens or generic-optics with 'parentSlotTypeSignature' instead"  #-}

-- | Configuration information that extends the parent built-in slot type.
--
-- /Note:/ Consider using 'slotTypeConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cstvrrsSlotTypeConfigurations :: Lens.Lens' CreateSlotTypeVersionResponse (Core.Maybe [Types.SlotTypeConfiguration])
cstvrrsSlotTypeConfigurations = Lens.field @"slotTypeConfigurations"
{-# INLINEABLE cstvrrsSlotTypeConfigurations #-}
{-# DEPRECATED slotTypeConfigurations "Use generic-lens or generic-optics with 'slotTypeConfigurations' instead"  #-}

-- | The strategy that Amazon Lex uses to determine the value of the slot. For more information, see 'PutSlotType' .
--
-- /Note:/ Consider using 'valueSelectionStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cstvrrsValueSelectionStrategy :: Lens.Lens' CreateSlotTypeVersionResponse (Core.Maybe Types.SlotValueSelectionStrategy)
cstvrrsValueSelectionStrategy = Lens.field @"valueSelectionStrategy"
{-# INLINEABLE cstvrrsValueSelectionStrategy #-}
{-# DEPRECATED valueSelectionStrategy "Use generic-lens or generic-optics with 'valueSelectionStrategy' instead"  #-}

-- | The version assigned to the new slot type version. 
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cstvrrsVersion :: Lens.Lens' CreateSlotTypeVersionResponse (Core.Maybe Types.Version)
cstvrrsVersion = Lens.field @"version"
{-# INLINEABLE cstvrrsVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cstvrrsResponseStatus :: Lens.Lens' CreateSlotTypeVersionResponse Core.Int
cstvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cstvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
