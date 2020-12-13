{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    PutSlotType (..),
    mkPutSlotType,

    -- ** Request lenses
    pstParentSlotTypeSignature,
    pstSlotTypeConfigurations,
    pstChecksum,
    pstValueSelectionStrategy,
    pstName,
    pstCreateVersion,
    pstDescription,
    pstEnumerationValues,

    -- * Destructuring the response
    PutSlotTypeResponse (..),
    mkPutSlotTypeResponse,

    -- ** Response lenses
    pstrsParentSlotTypeSignature,
    pstrsSlotTypeConfigurations,
    pstrsChecksum,
    pstrsValueSelectionStrategy,
    pstrsCreatedDate,
    pstrsName,
    pstrsVersion,
    pstrsLastUpdatedDate,
    pstrsCreateVersion,
    pstrsDescription,
    pstrsEnumerationValues,
    pstrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutSlotType' smart constructor.
data PutSlotType = PutSlotType'
  { -- | The built-in slot type used as the parent of the slot type. When you define a parent slot type, the new slot type has all of the same configuration as the parent.
    --
    -- Only @AMAZON.AlphaNumeric@ is supported.
    parentSlotTypeSignature :: Lude.Maybe Lude.Text,
    -- | Configuration information that extends the parent built-in slot type. The configuration is added to the settings for the parent slot type.
    slotTypeConfigurations :: Lude.Maybe [SlotTypeConfiguration],
    -- | Identifies a specific revision of the @> LATEST@ version.
    --
    -- When you create a new slot type, leave the @checksum@ field blank. If you specify a checksum you get a @BadRequestException@ exception.
    -- When you want to update a slot type, set the @checksum@ field to the checksum of the most recent revision of the @> LATEST@ version. If you don't specify the @checksum@ field, or if the checksum does not match the @> LATEST@ version, you get a @PreconditionFailedException@ exception.
    checksum :: Lude.Maybe Lude.Text,
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
    valueSelectionStrategy :: Lude.Maybe SlotValueSelectionStrategy,
    -- | The name of the slot type. The name is /not/ case sensitive.
    --
    -- The name can't match a built-in slot type name, or a built-in slot type name with "AMAZON." removed. For example, because there is a built-in slot type called @AMAZON.DATE@ , you can't create a custom slot type called @DATE@ .
    -- For a list of built-in slot types, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/slot-type-reference Slot Type Reference> in the /Alexa Skills Kit/ .
    name :: Lude.Text,
    -- | When set to @true@ a new numbered version of the slot type is created. This is the same as calling the @CreateSlotTypeVersion@ operation. If you do not specify @createVersion@ , the default is @false@ .
    createVersion :: Lude.Maybe Lude.Bool,
    -- | A description of the slot type.
    description :: Lude.Maybe Lude.Text,
    -- | A list of @EnumerationValue@ objects that defines the values that the slot type can take. Each value can have a list of @synonyms@ , which are additional values that help train the machine learning model about the values that it resolves for a slot.
    --
    -- A regular expression slot type doesn't require enumeration values. All other slot types require a list of enumeration values.
    -- When Amazon Lex resolves a slot value, it generates a resolution list that contains up to five possible values for the slot. If you are using a Lambda function, this resolution list is passed to the function. If you are not using a Lambda function you can choose to return the value that the user entered or the first value in the resolution list as the slot value. The @valueSelectionStrategy@ field indicates the option to use.
    enumerationValues :: Lude.Maybe [EnumerationValue]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutSlotType' with the minimum fields required to make a request.
--
-- * 'parentSlotTypeSignature' - The built-in slot type used as the parent of the slot type. When you define a parent slot type, the new slot type has all of the same configuration as the parent.
--
-- Only @AMAZON.AlphaNumeric@ is supported.
-- * 'slotTypeConfigurations' - Configuration information that extends the parent built-in slot type. The configuration is added to the settings for the parent slot type.
-- * 'checksum' - Identifies a specific revision of the @> LATEST@ version.
--
-- When you create a new slot type, leave the @checksum@ field blank. If you specify a checksum you get a @BadRequestException@ exception.
-- When you want to update a slot type, set the @checksum@ field to the checksum of the most recent revision of the @> LATEST@ version. If you don't specify the @checksum@ field, or if the checksum does not match the @> LATEST@ version, you get a @PreconditionFailedException@ exception.
-- * 'valueSelectionStrategy' - Determines the slot resolution strategy that Amazon Lex uses to return slot type values. The field can be set to one of the following values:
--
--
--     * @ORIGINAL_VALUE@ - Returns the value entered by the user, if the user value is similar to the slot value.
--
--
--     * @TOP_RESOLUTION@ - If there is a resolution list for the slot, return the first value in the resolution list as the slot type value. If there is no resolution list, null is returned.
--
--
-- If you don't specify the @valueSelectionStrategy@ , the default is @ORIGINAL_VALUE@ .
-- * 'name' - The name of the slot type. The name is /not/ case sensitive.
--
-- The name can't match a built-in slot type name, or a built-in slot type name with "AMAZON." removed. For example, because there is a built-in slot type called @AMAZON.DATE@ , you can't create a custom slot type called @DATE@ .
-- For a list of built-in slot types, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/slot-type-reference Slot Type Reference> in the /Alexa Skills Kit/ .
-- * 'createVersion' - When set to @true@ a new numbered version of the slot type is created. This is the same as calling the @CreateSlotTypeVersion@ operation. If you do not specify @createVersion@ , the default is @false@ .
-- * 'description' - A description of the slot type.
-- * 'enumerationValues' - A list of @EnumerationValue@ objects that defines the values that the slot type can take. Each value can have a list of @synonyms@ , which are additional values that help train the machine learning model about the values that it resolves for a slot.
--
-- A regular expression slot type doesn't require enumeration values. All other slot types require a list of enumeration values.
-- When Amazon Lex resolves a slot value, it generates a resolution list that contains up to five possible values for the slot. If you are using a Lambda function, this resolution list is passed to the function. If you are not using a Lambda function you can choose to return the value that the user entered or the first value in the resolution list as the slot value. The @valueSelectionStrategy@ field indicates the option to use.
mkPutSlotType ::
  -- | 'name'
  Lude.Text ->
  PutSlotType
mkPutSlotType pName_ =
  PutSlotType'
    { parentSlotTypeSignature = Lude.Nothing,
      slotTypeConfigurations = Lude.Nothing,
      checksum = Lude.Nothing,
      valueSelectionStrategy = Lude.Nothing,
      name = pName_,
      createVersion = Lude.Nothing,
      description = Lude.Nothing,
      enumerationValues = Lude.Nothing
    }

-- | The built-in slot type used as the parent of the slot type. When you define a parent slot type, the new slot type has all of the same configuration as the parent.
--
-- Only @AMAZON.AlphaNumeric@ is supported.
--
-- /Note:/ Consider using 'parentSlotTypeSignature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pstParentSlotTypeSignature :: Lens.Lens' PutSlotType (Lude.Maybe Lude.Text)
pstParentSlotTypeSignature = Lens.lens (parentSlotTypeSignature :: PutSlotType -> Lude.Maybe Lude.Text) (\s a -> s {parentSlotTypeSignature = a} :: PutSlotType)
{-# DEPRECATED pstParentSlotTypeSignature "Use generic-lens or generic-optics with 'parentSlotTypeSignature' instead." #-}

-- | Configuration information that extends the parent built-in slot type. The configuration is added to the settings for the parent slot type.
--
-- /Note:/ Consider using 'slotTypeConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pstSlotTypeConfigurations :: Lens.Lens' PutSlotType (Lude.Maybe [SlotTypeConfiguration])
pstSlotTypeConfigurations = Lens.lens (slotTypeConfigurations :: PutSlotType -> Lude.Maybe [SlotTypeConfiguration]) (\s a -> s {slotTypeConfigurations = a} :: PutSlotType)
{-# DEPRECATED pstSlotTypeConfigurations "Use generic-lens or generic-optics with 'slotTypeConfigurations' instead." #-}

-- | Identifies a specific revision of the @> LATEST@ version.
--
-- When you create a new slot type, leave the @checksum@ field blank. If you specify a checksum you get a @BadRequestException@ exception.
-- When you want to update a slot type, set the @checksum@ field to the checksum of the most recent revision of the @> LATEST@ version. If you don't specify the @checksum@ field, or if the checksum does not match the @> LATEST@ version, you get a @PreconditionFailedException@ exception.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pstChecksum :: Lens.Lens' PutSlotType (Lude.Maybe Lude.Text)
pstChecksum = Lens.lens (checksum :: PutSlotType -> Lude.Maybe Lude.Text) (\s a -> s {checksum = a} :: PutSlotType)
{-# DEPRECATED pstChecksum "Use generic-lens or generic-optics with 'checksum' instead." #-}

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
pstValueSelectionStrategy :: Lens.Lens' PutSlotType (Lude.Maybe SlotValueSelectionStrategy)
pstValueSelectionStrategy = Lens.lens (valueSelectionStrategy :: PutSlotType -> Lude.Maybe SlotValueSelectionStrategy) (\s a -> s {valueSelectionStrategy = a} :: PutSlotType)
{-# DEPRECATED pstValueSelectionStrategy "Use generic-lens or generic-optics with 'valueSelectionStrategy' instead." #-}

-- | The name of the slot type. The name is /not/ case sensitive.
--
-- The name can't match a built-in slot type name, or a built-in slot type name with "AMAZON." removed. For example, because there is a built-in slot type called @AMAZON.DATE@ , you can't create a custom slot type called @DATE@ .
-- For a list of built-in slot types, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/slot-type-reference Slot Type Reference> in the /Alexa Skills Kit/ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pstName :: Lens.Lens' PutSlotType Lude.Text
pstName = Lens.lens (name :: PutSlotType -> Lude.Text) (\s a -> s {name = a} :: PutSlotType)
{-# DEPRECATED pstName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | When set to @true@ a new numbered version of the slot type is created. This is the same as calling the @CreateSlotTypeVersion@ operation. If you do not specify @createVersion@ , the default is @false@ .
--
-- /Note:/ Consider using 'createVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pstCreateVersion :: Lens.Lens' PutSlotType (Lude.Maybe Lude.Bool)
pstCreateVersion = Lens.lens (createVersion :: PutSlotType -> Lude.Maybe Lude.Bool) (\s a -> s {createVersion = a} :: PutSlotType)
{-# DEPRECATED pstCreateVersion "Use generic-lens or generic-optics with 'createVersion' instead." #-}

-- | A description of the slot type.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pstDescription :: Lens.Lens' PutSlotType (Lude.Maybe Lude.Text)
pstDescription = Lens.lens (description :: PutSlotType -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: PutSlotType)
{-# DEPRECATED pstDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A list of @EnumerationValue@ objects that defines the values that the slot type can take. Each value can have a list of @synonyms@ , which are additional values that help train the machine learning model about the values that it resolves for a slot.
--
-- A regular expression slot type doesn't require enumeration values. All other slot types require a list of enumeration values.
-- When Amazon Lex resolves a slot value, it generates a resolution list that contains up to five possible values for the slot. If you are using a Lambda function, this resolution list is passed to the function. If you are not using a Lambda function you can choose to return the value that the user entered or the first value in the resolution list as the slot value. The @valueSelectionStrategy@ field indicates the option to use.
--
-- /Note:/ Consider using 'enumerationValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pstEnumerationValues :: Lens.Lens' PutSlotType (Lude.Maybe [EnumerationValue])
pstEnumerationValues = Lens.lens (enumerationValues :: PutSlotType -> Lude.Maybe [EnumerationValue]) (\s a -> s {enumerationValues = a} :: PutSlotType)
{-# DEPRECATED pstEnumerationValues "Use generic-lens or generic-optics with 'enumerationValues' instead." #-}

instance Lude.AWSRequest PutSlotType where
  type Rs PutSlotType = PutSlotTypeResponse
  request = Req.putJSON lexModelsService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutSlotTypeResponse'
            Lude.<$> (x Lude..?> "parentSlotTypeSignature")
            Lude.<*> (x Lude..?> "slotTypeConfigurations" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "checksum")
            Lude.<*> (x Lude..?> "valueSelectionStrategy")
            Lude.<*> (x Lude..?> "createdDate")
            Lude.<*> (x Lude..?> "name")
            Lude.<*> (x Lude..?> "version")
            Lude.<*> (x Lude..?> "lastUpdatedDate")
            Lude.<*> (x Lude..?> "createVersion")
            Lude.<*> (x Lude..?> "description")
            Lude.<*> (x Lude..?> "enumerationValues" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutSlotType where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutSlotType where
  toJSON PutSlotType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("parentSlotTypeSignature" Lude..=)
              Lude.<$> parentSlotTypeSignature,
            ("slotTypeConfigurations" Lude..=) Lude.<$> slotTypeConfigurations,
            ("checksum" Lude..=) Lude.<$> checksum,
            ("valueSelectionStrategy" Lude..=) Lude.<$> valueSelectionStrategy,
            ("createVersion" Lude..=) Lude.<$> createVersion,
            ("description" Lude..=) Lude.<$> description,
            ("enumerationValues" Lude..=) Lude.<$> enumerationValues
          ]
      )

instance Lude.ToPath PutSlotType where
  toPath PutSlotType' {..} =
    Lude.mconcat ["/slottypes/", Lude.toBS name, "/versions/$LATEST"]

instance Lude.ToQuery PutSlotType where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutSlotTypeResponse' smart constructor.
data PutSlotTypeResponse = PutSlotTypeResponse'
  { -- | The built-in slot type used as the parent of the slot type.
    parentSlotTypeSignature :: Lude.Maybe Lude.Text,
    -- | Configuration information that extends the parent built-in slot type.
    slotTypeConfigurations :: Lude.Maybe [SlotTypeConfiguration],
    -- | Checksum of the @> LATEST@ version of the slot type.
    checksum :: Lude.Maybe Lude.Text,
    -- | The slot resolution strategy that Amazon Lex uses to determine the value of the slot. For more information, see 'PutSlotType' .
    valueSelectionStrategy :: Lude.Maybe SlotValueSelectionStrategy,
    -- | The date that the slot type was created.
    createdDate :: Lude.Maybe Lude.Timestamp,
    -- | The name of the slot type.
    name :: Lude.Maybe Lude.Text,
    -- | The version of the slot type. For a new slot type, the version is always @> LATEST@ .
    version :: Lude.Maybe Lude.Text,
    -- | The date that the slot type was updated. When you create a slot type, the creation date and last update date are the same.
    lastUpdatedDate :: Lude.Maybe Lude.Timestamp,
    -- | @True@ if a new version of the slot type was created. If the @createVersion@ field was not specified in the request, the @createVersion@ field is set to false in the response.
    createVersion :: Lude.Maybe Lude.Bool,
    -- | A description of the slot type.
    description :: Lude.Maybe Lude.Text,
    -- | A list of @EnumerationValue@ objects that defines the values that the slot type can take.
    enumerationValues :: Lude.Maybe [EnumerationValue],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutSlotTypeResponse' with the minimum fields required to make a request.
--
-- * 'parentSlotTypeSignature' - The built-in slot type used as the parent of the slot type.
-- * 'slotTypeConfigurations' - Configuration information that extends the parent built-in slot type.
-- * 'checksum' - Checksum of the @> LATEST@ version of the slot type.
-- * 'valueSelectionStrategy' - The slot resolution strategy that Amazon Lex uses to determine the value of the slot. For more information, see 'PutSlotType' .
-- * 'createdDate' - The date that the slot type was created.
-- * 'name' - The name of the slot type.
-- * 'version' - The version of the slot type. For a new slot type, the version is always @> LATEST@ .
-- * 'lastUpdatedDate' - The date that the slot type was updated. When you create a slot type, the creation date and last update date are the same.
-- * 'createVersion' - @True@ if a new version of the slot type was created. If the @createVersion@ field was not specified in the request, the @createVersion@ field is set to false in the response.
-- * 'description' - A description of the slot type.
-- * 'enumerationValues' - A list of @EnumerationValue@ objects that defines the values that the slot type can take.
-- * 'responseStatus' - The response status code.
mkPutSlotTypeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutSlotTypeResponse
mkPutSlotTypeResponse pResponseStatus_ =
  PutSlotTypeResponse'
    { parentSlotTypeSignature = Lude.Nothing,
      slotTypeConfigurations = Lude.Nothing,
      checksum = Lude.Nothing,
      valueSelectionStrategy = Lude.Nothing,
      createdDate = Lude.Nothing,
      name = Lude.Nothing,
      version = Lude.Nothing,
      lastUpdatedDate = Lude.Nothing,
      createVersion = Lude.Nothing,
      description = Lude.Nothing,
      enumerationValues = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The built-in slot type used as the parent of the slot type.
--
-- /Note:/ Consider using 'parentSlotTypeSignature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pstrsParentSlotTypeSignature :: Lens.Lens' PutSlotTypeResponse (Lude.Maybe Lude.Text)
pstrsParentSlotTypeSignature = Lens.lens (parentSlotTypeSignature :: PutSlotTypeResponse -> Lude.Maybe Lude.Text) (\s a -> s {parentSlotTypeSignature = a} :: PutSlotTypeResponse)
{-# DEPRECATED pstrsParentSlotTypeSignature "Use generic-lens or generic-optics with 'parentSlotTypeSignature' instead." #-}

-- | Configuration information that extends the parent built-in slot type.
--
-- /Note:/ Consider using 'slotTypeConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pstrsSlotTypeConfigurations :: Lens.Lens' PutSlotTypeResponse (Lude.Maybe [SlotTypeConfiguration])
pstrsSlotTypeConfigurations = Lens.lens (slotTypeConfigurations :: PutSlotTypeResponse -> Lude.Maybe [SlotTypeConfiguration]) (\s a -> s {slotTypeConfigurations = a} :: PutSlotTypeResponse)
{-# DEPRECATED pstrsSlotTypeConfigurations "Use generic-lens or generic-optics with 'slotTypeConfigurations' instead." #-}

-- | Checksum of the @> LATEST@ version of the slot type.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pstrsChecksum :: Lens.Lens' PutSlotTypeResponse (Lude.Maybe Lude.Text)
pstrsChecksum = Lens.lens (checksum :: PutSlotTypeResponse -> Lude.Maybe Lude.Text) (\s a -> s {checksum = a} :: PutSlotTypeResponse)
{-# DEPRECATED pstrsChecksum "Use generic-lens or generic-optics with 'checksum' instead." #-}

-- | The slot resolution strategy that Amazon Lex uses to determine the value of the slot. For more information, see 'PutSlotType' .
--
-- /Note:/ Consider using 'valueSelectionStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pstrsValueSelectionStrategy :: Lens.Lens' PutSlotTypeResponse (Lude.Maybe SlotValueSelectionStrategy)
pstrsValueSelectionStrategy = Lens.lens (valueSelectionStrategy :: PutSlotTypeResponse -> Lude.Maybe SlotValueSelectionStrategy) (\s a -> s {valueSelectionStrategy = a} :: PutSlotTypeResponse)
{-# DEPRECATED pstrsValueSelectionStrategy "Use generic-lens or generic-optics with 'valueSelectionStrategy' instead." #-}

-- | The date that the slot type was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pstrsCreatedDate :: Lens.Lens' PutSlotTypeResponse (Lude.Maybe Lude.Timestamp)
pstrsCreatedDate = Lens.lens (createdDate :: PutSlotTypeResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdDate = a} :: PutSlotTypeResponse)
{-# DEPRECATED pstrsCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | The name of the slot type.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pstrsName :: Lens.Lens' PutSlotTypeResponse (Lude.Maybe Lude.Text)
pstrsName = Lens.lens (name :: PutSlotTypeResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: PutSlotTypeResponse)
{-# DEPRECATED pstrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version of the slot type. For a new slot type, the version is always @> LATEST@ .
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pstrsVersion :: Lens.Lens' PutSlotTypeResponse (Lude.Maybe Lude.Text)
pstrsVersion = Lens.lens (version :: PutSlotTypeResponse -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: PutSlotTypeResponse)
{-# DEPRECATED pstrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The date that the slot type was updated. When you create a slot type, the creation date and last update date are the same.
--
-- /Note:/ Consider using 'lastUpdatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pstrsLastUpdatedDate :: Lens.Lens' PutSlotTypeResponse (Lude.Maybe Lude.Timestamp)
pstrsLastUpdatedDate = Lens.lens (lastUpdatedDate :: PutSlotTypeResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedDate = a} :: PutSlotTypeResponse)
{-# DEPRECATED pstrsLastUpdatedDate "Use generic-lens or generic-optics with 'lastUpdatedDate' instead." #-}

-- | @True@ if a new version of the slot type was created. If the @createVersion@ field was not specified in the request, the @createVersion@ field is set to false in the response.
--
-- /Note:/ Consider using 'createVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pstrsCreateVersion :: Lens.Lens' PutSlotTypeResponse (Lude.Maybe Lude.Bool)
pstrsCreateVersion = Lens.lens (createVersion :: PutSlotTypeResponse -> Lude.Maybe Lude.Bool) (\s a -> s {createVersion = a} :: PutSlotTypeResponse)
{-# DEPRECATED pstrsCreateVersion "Use generic-lens or generic-optics with 'createVersion' instead." #-}

-- | A description of the slot type.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pstrsDescription :: Lens.Lens' PutSlotTypeResponse (Lude.Maybe Lude.Text)
pstrsDescription = Lens.lens (description :: PutSlotTypeResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: PutSlotTypeResponse)
{-# DEPRECATED pstrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A list of @EnumerationValue@ objects that defines the values that the slot type can take.
--
-- /Note:/ Consider using 'enumerationValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pstrsEnumerationValues :: Lens.Lens' PutSlotTypeResponse (Lude.Maybe [EnumerationValue])
pstrsEnumerationValues = Lens.lens (enumerationValues :: PutSlotTypeResponse -> Lude.Maybe [EnumerationValue]) (\s a -> s {enumerationValues = a} :: PutSlotTypeResponse)
{-# DEPRECATED pstrsEnumerationValues "Use generic-lens or generic-optics with 'enumerationValues' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pstrsResponseStatus :: Lens.Lens' PutSlotTypeResponse Lude.Int
pstrsResponseStatus = Lens.lens (responseStatus :: PutSlotTypeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutSlotTypeResponse)
{-# DEPRECATED pstrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
