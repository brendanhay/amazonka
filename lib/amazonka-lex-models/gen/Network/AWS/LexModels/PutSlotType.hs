{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.PutSlotType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a custom slot type or replaces an existing custom slot type.
--
-- To create a custom slot type, specify a name for the slot type and a set of enumeration values, which are the values that a slot of this type can assume. For more information, see 'how-it-works' .
-- If you specify the name of an existing slot type, the fields in the request replace the existing values in the @> LATEST@ version of the slot type. Amazon Lex removes the fields that you don't provide in the request. If you don't specify required fields, Amazon Lex throws an exception. When you update the @> LATEST@ version of a slot type, if a bot uses the @> LATEST@ version of an intent that contains the slot type, the bot's @status@ field is set to @NOT_BUILT@ .
-- This operation requires permissions for the @lex:PutSlotType@ action.
module Network.AWS.LexModels.PutSlotType
    (
    -- * Creating a request
      PutSlotType (..)
    , mkPutSlotType
    -- ** Request lenses
    , pstName
    , pstChecksum
    , pstCreateVersion
    , pstDescription
    , pstEnumerationValues
    , pstParentSlotTypeSignature
    , pstSlotTypeConfigurations
    , pstValueSelectionStrategy

    -- * Destructuring the response
    , PutSlotTypeResponse (..)
    , mkPutSlotTypeResponse
    -- ** Response lenses
    , pstrrsChecksum
    , pstrrsCreateVersion
    , pstrrsCreatedDate
    , pstrrsDescription
    , pstrrsEnumerationValues
    , pstrrsLastUpdatedDate
    , pstrrsName
    , pstrrsParentSlotTypeSignature
    , pstrrsSlotTypeConfigurations
    , pstrrsValueSelectionStrategy
    , pstrrsVersion
    , pstrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutSlotType' smart constructor.
data PutSlotType = PutSlotType'
  { name :: Types.SlotTypeName
    -- ^ The name of the slot type. The name is /not/ case sensitive. 
--
-- The name can't match a built-in slot type name, or a built-in slot type name with "AMAZON." removed. For example, because there is a built-in slot type called @AMAZON.DATE@ , you can't create a custom slot type called @DATE@ .
-- For a list of built-in slot types, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/slot-type-reference Slot Type Reference> in the /Alexa Skills Kit/ .
  , checksum :: Core.Maybe Core.Text
    -- ^ Identifies a specific revision of the @> LATEST@ version.
--
-- When you create a new slot type, leave the @checksum@ field blank. If you specify a checksum you get a @BadRequestException@ exception.
-- When you want to update a slot type, set the @checksum@ field to the checksum of the most recent revision of the @> LATEST@ version. If you don't specify the @checksum@ field, or if the checksum does not match the @> LATEST@ version, you get a @PreconditionFailedException@ exception.
  , createVersion :: Core.Maybe Core.Bool
    -- ^ When set to @true@ a new numbered version of the slot type is created. This is the same as calling the @CreateSlotTypeVersion@ operation. If you do not specify @createVersion@ , the default is @false@ .
  , description :: Core.Maybe Types.Description
    -- ^ A description of the slot type.
  , enumerationValues :: Core.Maybe [Types.EnumerationValue]
    -- ^ A list of @EnumerationValue@ objects that defines the values that the slot type can take. Each value can have a list of @synonyms@ , which are additional values that help train the machine learning model about the values that it resolves for a slot. 
--
-- A regular expression slot type doesn't require enumeration values. All other slot types require a list of enumeration values.
-- When Amazon Lex resolves a slot value, it generates a resolution list that contains up to five possible values for the slot. If you are using a Lambda function, this resolution list is passed to the function. If you are not using a Lambda function you can choose to return the value that the user entered or the first value in the resolution list as the slot value. The @valueSelectionStrategy@ field indicates the option to use. 
  , parentSlotTypeSignature :: Core.Maybe Types.CustomOrBuiltinSlotTypeName
    -- ^ The built-in slot type used as the parent of the slot type. When you define a parent slot type, the new slot type has all of the same configuration as the parent.
--
-- Only @AMAZON.AlphaNumeric@ is supported.
  , slotTypeConfigurations :: Core.Maybe [Types.SlotTypeConfiguration]
    -- ^ Configuration information that extends the parent built-in slot type. The configuration is added to the settings for the parent slot type.
  , valueSelectionStrategy :: Core.Maybe Types.SlotValueSelectionStrategy
    -- ^ Determines the slot resolution strategy that Amazon Lex uses to return slot type values. The field can be set to one of the following values:
--
--
--     * @ORIGINAL_VALUE@ - Returns the value entered by the user, if the user value is similar to the slot value.
--
--
--     * @TOP_RESOLUTION@ - If there is a resolution list for the slot, return the first value in the resolution list as the slot type value. If there is no resolution list, null is returned.
--
--
-- If you don't specify the @valueSelectionStrategy@ , the default is @ORIGINAL_VALUE@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutSlotType' value with any optional fields omitted.
mkPutSlotType
    :: Types.SlotTypeName -- ^ 'name'
    -> PutSlotType
mkPutSlotType name
  = PutSlotType'{name, checksum = Core.Nothing,
                 createVersion = Core.Nothing, description = Core.Nothing,
                 enumerationValues = Core.Nothing,
                 parentSlotTypeSignature = Core.Nothing,
                 slotTypeConfigurations = Core.Nothing,
                 valueSelectionStrategy = Core.Nothing}

-- | The name of the slot type. The name is /not/ case sensitive. 
--
-- The name can't match a built-in slot type name, or a built-in slot type name with "AMAZON." removed. For example, because there is a built-in slot type called @AMAZON.DATE@ , you can't create a custom slot type called @DATE@ .
-- For a list of built-in slot types, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/slot-type-reference Slot Type Reference> in the /Alexa Skills Kit/ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pstName :: Lens.Lens' PutSlotType Types.SlotTypeName
pstName = Lens.field @"name"
{-# INLINEABLE pstName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Identifies a specific revision of the @> LATEST@ version.
--
-- When you create a new slot type, leave the @checksum@ field blank. If you specify a checksum you get a @BadRequestException@ exception.
-- When you want to update a slot type, set the @checksum@ field to the checksum of the most recent revision of the @> LATEST@ version. If you don't specify the @checksum@ field, or if the checksum does not match the @> LATEST@ version, you get a @PreconditionFailedException@ exception.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pstChecksum :: Lens.Lens' PutSlotType (Core.Maybe Core.Text)
pstChecksum = Lens.field @"checksum"
{-# INLINEABLE pstChecksum #-}
{-# DEPRECATED checksum "Use generic-lens or generic-optics with 'checksum' instead"  #-}

-- | When set to @true@ a new numbered version of the slot type is created. This is the same as calling the @CreateSlotTypeVersion@ operation. If you do not specify @createVersion@ , the default is @false@ .
--
-- /Note:/ Consider using 'createVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pstCreateVersion :: Lens.Lens' PutSlotType (Core.Maybe Core.Bool)
pstCreateVersion = Lens.field @"createVersion"
{-# INLINEABLE pstCreateVersion #-}
{-# DEPRECATED createVersion "Use generic-lens or generic-optics with 'createVersion' instead"  #-}

-- | A description of the slot type.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pstDescription :: Lens.Lens' PutSlotType (Core.Maybe Types.Description)
pstDescription = Lens.field @"description"
{-# INLINEABLE pstDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | A list of @EnumerationValue@ objects that defines the values that the slot type can take. Each value can have a list of @synonyms@ , which are additional values that help train the machine learning model about the values that it resolves for a slot. 
--
-- A regular expression slot type doesn't require enumeration values. All other slot types require a list of enumeration values.
-- When Amazon Lex resolves a slot value, it generates a resolution list that contains up to five possible values for the slot. If you are using a Lambda function, this resolution list is passed to the function. If you are not using a Lambda function you can choose to return the value that the user entered or the first value in the resolution list as the slot value. The @valueSelectionStrategy@ field indicates the option to use. 
--
-- /Note:/ Consider using 'enumerationValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pstEnumerationValues :: Lens.Lens' PutSlotType (Core.Maybe [Types.EnumerationValue])
pstEnumerationValues = Lens.field @"enumerationValues"
{-# INLINEABLE pstEnumerationValues #-}
{-# DEPRECATED enumerationValues "Use generic-lens or generic-optics with 'enumerationValues' instead"  #-}

-- | The built-in slot type used as the parent of the slot type. When you define a parent slot type, the new slot type has all of the same configuration as the parent.
--
-- Only @AMAZON.AlphaNumeric@ is supported.
--
-- /Note:/ Consider using 'parentSlotTypeSignature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pstParentSlotTypeSignature :: Lens.Lens' PutSlotType (Core.Maybe Types.CustomOrBuiltinSlotTypeName)
pstParentSlotTypeSignature = Lens.field @"parentSlotTypeSignature"
{-# INLINEABLE pstParentSlotTypeSignature #-}
{-# DEPRECATED parentSlotTypeSignature "Use generic-lens or generic-optics with 'parentSlotTypeSignature' instead"  #-}

-- | Configuration information that extends the parent built-in slot type. The configuration is added to the settings for the parent slot type.
--
-- /Note:/ Consider using 'slotTypeConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pstSlotTypeConfigurations :: Lens.Lens' PutSlotType (Core.Maybe [Types.SlotTypeConfiguration])
pstSlotTypeConfigurations = Lens.field @"slotTypeConfigurations"
{-# INLINEABLE pstSlotTypeConfigurations #-}
{-# DEPRECATED slotTypeConfigurations "Use generic-lens or generic-optics with 'slotTypeConfigurations' instead"  #-}

-- | Determines the slot resolution strategy that Amazon Lex uses to return slot type values. The field can be set to one of the following values:
--
--
--     * @ORIGINAL_VALUE@ - Returns the value entered by the user, if the user value is similar to the slot value.
--
--
--     * @TOP_RESOLUTION@ - If there is a resolution list for the slot, return the first value in the resolution list as the slot type value. If there is no resolution list, null is returned.
--
--
-- If you don't specify the @valueSelectionStrategy@ , the default is @ORIGINAL_VALUE@ .
--
-- /Note:/ Consider using 'valueSelectionStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pstValueSelectionStrategy :: Lens.Lens' PutSlotType (Core.Maybe Types.SlotValueSelectionStrategy)
pstValueSelectionStrategy = Lens.field @"valueSelectionStrategy"
{-# INLINEABLE pstValueSelectionStrategy #-}
{-# DEPRECATED valueSelectionStrategy "Use generic-lens or generic-optics with 'valueSelectionStrategy' instead"  #-}

instance Core.ToQuery PutSlotType where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutSlotType where
        toHeaders PutSlotType{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PutSlotType where
        toJSON PutSlotType{..}
          = Core.object
              (Core.catMaybes
                 [("checksum" Core..=) Core.<$> checksum,
                  ("createVersion" Core..=) Core.<$> createVersion,
                  ("description" Core..=) Core.<$> description,
                  ("enumerationValues" Core..=) Core.<$> enumerationValues,
                  ("parentSlotTypeSignature" Core..=) Core.<$>
                    parentSlotTypeSignature,
                  ("slotTypeConfigurations" Core..=) Core.<$> slotTypeConfigurations,
                  ("valueSelectionStrategy" Core..=) Core.<$>
                    valueSelectionStrategy])

instance Core.AWSRequest PutSlotType where
        type Rs PutSlotType = PutSlotTypeResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/slottypes/" Core.<> Core.toText name Core.<> "/versions/$LATEST",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 PutSlotTypeResponse' Core.<$>
                   (x Core..:? "checksum") Core.<*> x Core..:? "createVersion"
                     Core.<*> x Core..:? "createdDate"
                     Core.<*> x Core..:? "description"
                     Core.<*> x Core..:? "enumerationValues"
                     Core.<*> x Core..:? "lastUpdatedDate"
                     Core.<*> x Core..:? "name"
                     Core.<*> x Core..:? "parentSlotTypeSignature"
                     Core.<*> x Core..:? "slotTypeConfigurations"
                     Core.<*> x Core..:? "valueSelectionStrategy"
                     Core.<*> x Core..:? "version"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutSlotTypeResponse' smart constructor.
data PutSlotTypeResponse = PutSlotTypeResponse'
  { checksum :: Core.Maybe Core.Text
    -- ^ Checksum of the @> LATEST@ version of the slot type.
  , createVersion :: Core.Maybe Core.Bool
    -- ^ @True@ if a new version of the slot type was created. If the @createVersion@ field was not specified in the request, the @createVersion@ field is set to false in the response.
  , createdDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date that the slot type was created.
  , description :: Core.Maybe Types.Description
    -- ^ A description of the slot type.
  , enumerationValues :: Core.Maybe [Types.EnumerationValue]
    -- ^ A list of @EnumerationValue@ objects that defines the values that the slot type can take.
  , lastUpdatedDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date that the slot type was updated. When you create a slot type, the creation date and last update date are the same.
  , name :: Core.Maybe Types.SlotTypeName
    -- ^ The name of the slot type.
  , parentSlotTypeSignature :: Core.Maybe Types.CustomOrBuiltinSlotTypeName
    -- ^ The built-in slot type used as the parent of the slot type.
  , slotTypeConfigurations :: Core.Maybe [Types.SlotTypeConfiguration]
    -- ^ Configuration information that extends the parent built-in slot type.
  , valueSelectionStrategy :: Core.Maybe Types.SlotValueSelectionStrategy
    -- ^ The slot resolution strategy that Amazon Lex uses to determine the value of the slot. For more information, see 'PutSlotType' .
  , version :: Core.Maybe Types.Version
    -- ^ The version of the slot type. For a new slot type, the version is always @> LATEST@ . 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'PutSlotTypeResponse' value with any optional fields omitted.
mkPutSlotTypeResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PutSlotTypeResponse
mkPutSlotTypeResponse responseStatus
  = PutSlotTypeResponse'{checksum = Core.Nothing,
                         createVersion = Core.Nothing, createdDate = Core.Nothing,
                         description = Core.Nothing, enumerationValues = Core.Nothing,
                         lastUpdatedDate = Core.Nothing, name = Core.Nothing,
                         parentSlotTypeSignature = Core.Nothing,
                         slotTypeConfigurations = Core.Nothing,
                         valueSelectionStrategy = Core.Nothing, version = Core.Nothing,
                         responseStatus}

-- | Checksum of the @> LATEST@ version of the slot type.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pstrrsChecksum :: Lens.Lens' PutSlotTypeResponse (Core.Maybe Core.Text)
pstrrsChecksum = Lens.field @"checksum"
{-# INLINEABLE pstrrsChecksum #-}
{-# DEPRECATED checksum "Use generic-lens or generic-optics with 'checksum' instead"  #-}

-- | @True@ if a new version of the slot type was created. If the @createVersion@ field was not specified in the request, the @createVersion@ field is set to false in the response.
--
-- /Note:/ Consider using 'createVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pstrrsCreateVersion :: Lens.Lens' PutSlotTypeResponse (Core.Maybe Core.Bool)
pstrrsCreateVersion = Lens.field @"createVersion"
{-# INLINEABLE pstrrsCreateVersion #-}
{-# DEPRECATED createVersion "Use generic-lens or generic-optics with 'createVersion' instead"  #-}

-- | The date that the slot type was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pstrrsCreatedDate :: Lens.Lens' PutSlotTypeResponse (Core.Maybe Core.NominalDiffTime)
pstrrsCreatedDate = Lens.field @"createdDate"
{-# INLINEABLE pstrrsCreatedDate #-}
{-# DEPRECATED createdDate "Use generic-lens or generic-optics with 'createdDate' instead"  #-}

-- | A description of the slot type.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pstrrsDescription :: Lens.Lens' PutSlotTypeResponse (Core.Maybe Types.Description)
pstrrsDescription = Lens.field @"description"
{-# INLINEABLE pstrrsDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | A list of @EnumerationValue@ objects that defines the values that the slot type can take.
--
-- /Note:/ Consider using 'enumerationValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pstrrsEnumerationValues :: Lens.Lens' PutSlotTypeResponse (Core.Maybe [Types.EnumerationValue])
pstrrsEnumerationValues = Lens.field @"enumerationValues"
{-# INLINEABLE pstrrsEnumerationValues #-}
{-# DEPRECATED enumerationValues "Use generic-lens or generic-optics with 'enumerationValues' instead"  #-}

-- | The date that the slot type was updated. When you create a slot type, the creation date and last update date are the same.
--
-- /Note:/ Consider using 'lastUpdatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pstrrsLastUpdatedDate :: Lens.Lens' PutSlotTypeResponse (Core.Maybe Core.NominalDiffTime)
pstrrsLastUpdatedDate = Lens.field @"lastUpdatedDate"
{-# INLINEABLE pstrrsLastUpdatedDate #-}
{-# DEPRECATED lastUpdatedDate "Use generic-lens or generic-optics with 'lastUpdatedDate' instead"  #-}

-- | The name of the slot type.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pstrrsName :: Lens.Lens' PutSlotTypeResponse (Core.Maybe Types.SlotTypeName)
pstrrsName = Lens.field @"name"
{-# INLINEABLE pstrrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The built-in slot type used as the parent of the slot type.
--
-- /Note:/ Consider using 'parentSlotTypeSignature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pstrrsParentSlotTypeSignature :: Lens.Lens' PutSlotTypeResponse (Core.Maybe Types.CustomOrBuiltinSlotTypeName)
pstrrsParentSlotTypeSignature = Lens.field @"parentSlotTypeSignature"
{-# INLINEABLE pstrrsParentSlotTypeSignature #-}
{-# DEPRECATED parentSlotTypeSignature "Use generic-lens or generic-optics with 'parentSlotTypeSignature' instead"  #-}

-- | Configuration information that extends the parent built-in slot type.
--
-- /Note:/ Consider using 'slotTypeConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pstrrsSlotTypeConfigurations :: Lens.Lens' PutSlotTypeResponse (Core.Maybe [Types.SlotTypeConfiguration])
pstrrsSlotTypeConfigurations = Lens.field @"slotTypeConfigurations"
{-# INLINEABLE pstrrsSlotTypeConfigurations #-}
{-# DEPRECATED slotTypeConfigurations "Use generic-lens or generic-optics with 'slotTypeConfigurations' instead"  #-}

-- | The slot resolution strategy that Amazon Lex uses to determine the value of the slot. For more information, see 'PutSlotType' .
--
-- /Note:/ Consider using 'valueSelectionStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pstrrsValueSelectionStrategy :: Lens.Lens' PutSlotTypeResponse (Core.Maybe Types.SlotValueSelectionStrategy)
pstrrsValueSelectionStrategy = Lens.field @"valueSelectionStrategy"
{-# INLINEABLE pstrrsValueSelectionStrategy #-}
{-# DEPRECATED valueSelectionStrategy "Use generic-lens or generic-optics with 'valueSelectionStrategy' instead"  #-}

-- | The version of the slot type. For a new slot type, the version is always @> LATEST@ . 
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pstrrsVersion :: Lens.Lens' PutSlotTypeResponse (Core.Maybe Types.Version)
pstrrsVersion = Lens.field @"version"
{-# INLINEABLE pstrrsVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pstrrsResponseStatus :: Lens.Lens' PutSlotTypeResponse Core.Int
pstrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE pstrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
