{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
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
--
-- To create a custom slot type, specify a name for the slot type and a set of enumeration values, which are the values that a slot of this type can assume. For more information, see 'how-it-works' .
--
-- If you specify the name of an existing slot type, the fields in the request replace the existing values in the @> LATEST@ version of the slot type. Amazon Lex removes the fields that you don't provide in the request. If you don't specify required fields, Amazon Lex throws an exception. When you update the @> LATEST@ version of a slot type, if a bot uses the @> LATEST@ version of an intent that contains the slot type, the bot's @status@ field is set to @NOT_BUILT@ .
--
-- This operation requires permissions for the @lex:PutSlotType@ action.
module Network.AWS.LexModels.PutSlotType
  ( -- * Creating a Request
    putSlotType,
    PutSlotType,

    -- * Request Lenses
    pstParentSlotTypeSignature,
    pstSlotTypeConfigurations,
    pstChecksum,
    pstValueSelectionStrategy,
    pstCreateVersion,
    pstDescription,
    pstEnumerationValues,
    pstName,

    -- * Destructuring the Response
    putSlotTypeResponse,
    PutSlotTypeResponse,

    -- * Response Lenses
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

import Network.AWS.Lens
import Network.AWS.LexModels.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putSlotType' smart constructor.
data PutSlotType = PutSlotType'
  { _pstParentSlotTypeSignature ::
      !(Maybe Text),
    _pstSlotTypeConfigurations :: !(Maybe [SlotTypeConfiguration]),
    _pstChecksum :: !(Maybe Text),
    _pstValueSelectionStrategy :: !(Maybe SlotValueSelectionStrategy),
    _pstCreateVersion :: !(Maybe Bool),
    _pstDescription :: !(Maybe Text),
    _pstEnumerationValues :: !(Maybe [EnumerationValue]),
    _pstName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutSlotType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pstParentSlotTypeSignature' - The built-in slot type used as the parent of the slot type. When you define a parent slot type, the new slot type has all of the same configuration as the parent. Only @AMAZON.AlphaNumeric@ is supported.
--
-- * 'pstSlotTypeConfigurations' - Configuration information that extends the parent built-in slot type. The configuration is added to the settings for the parent slot type.
--
-- * 'pstChecksum' - Identifies a specific revision of the @> LATEST@ version. When you create a new slot type, leave the @checksum@ field blank. If you specify a checksum you get a @BadRequestException@ exception. When you want to update a slot type, set the @checksum@ field to the checksum of the most recent revision of the @> LATEST@ version. If you don't specify the @checksum@ field, or if the checksum does not match the @> LATEST@ version, you get a @PreconditionFailedException@ exception.
--
-- * 'pstValueSelectionStrategy' - Determines the slot resolution strategy that Amazon Lex uses to return slot type values. The field can be set to one of the following values:     * @ORIGINAL_VALUE@ - Returns the value entered by the user, if the user value is similar to the slot value.     * @TOP_RESOLUTION@ - If there is a resolution list for the slot, return the first value in the resolution list as the slot type value. If there is no resolution list, null is returned. If you don't specify the @valueSelectionStrategy@ , the default is @ORIGINAL_VALUE@ .
--
-- * 'pstCreateVersion' - When set to @true@ a new numbered version of the slot type is created. This is the same as calling the @CreateSlotTypeVersion@ operation. If you do not specify @createVersion@ , the default is @false@ .
--
-- * 'pstDescription' - A description of the slot type.
--
-- * 'pstEnumerationValues' - A list of @EnumerationValue@ objects that defines the values that the slot type can take. Each value can have a list of @synonyms@ , which are additional values that help train the machine learning model about the values that it resolves for a slot.  A regular expression slot type doesn't require enumeration values. All other slot types require a list of enumeration values. When Amazon Lex resolves a slot value, it generates a resolution list that contains up to five possible values for the slot. If you are using a Lambda function, this resolution list is passed to the function. If you are not using a Lambda function you can choose to return the value that the user entered or the first value in the resolution list as the slot value. The @valueSelectionStrategy@ field indicates the option to use.
--
-- * 'pstName' - The name of the slot type. The name is /not/ case sensitive.  The name can't match a built-in slot type name, or a built-in slot type name with "AMAZON." removed. For example, because there is a built-in slot type called @AMAZON.DATE@ , you can't create a custom slot type called @DATE@ . For a list of built-in slot types, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/slot-type-reference Slot Type Reference> in the /Alexa Skills Kit/ .
putSlotType ::
  -- | 'pstName'
  Text ->
  PutSlotType
putSlotType pName_ =
  PutSlotType'
    { _pstParentSlotTypeSignature = Nothing,
      _pstSlotTypeConfigurations = Nothing,
      _pstChecksum = Nothing,
      _pstValueSelectionStrategy = Nothing,
      _pstCreateVersion = Nothing,
      _pstDescription = Nothing,
      _pstEnumerationValues = Nothing,
      _pstName = pName_
    }

-- | The built-in slot type used as the parent of the slot type. When you define a parent slot type, the new slot type has all of the same configuration as the parent. Only @AMAZON.AlphaNumeric@ is supported.
pstParentSlotTypeSignature :: Lens' PutSlotType (Maybe Text)
pstParentSlotTypeSignature = lens _pstParentSlotTypeSignature (\s a -> s {_pstParentSlotTypeSignature = a})

-- | Configuration information that extends the parent built-in slot type. The configuration is added to the settings for the parent slot type.
pstSlotTypeConfigurations :: Lens' PutSlotType [SlotTypeConfiguration]
pstSlotTypeConfigurations = lens _pstSlotTypeConfigurations (\s a -> s {_pstSlotTypeConfigurations = a}) . _Default . _Coerce

-- | Identifies a specific revision of the @> LATEST@ version. When you create a new slot type, leave the @checksum@ field blank. If you specify a checksum you get a @BadRequestException@ exception. When you want to update a slot type, set the @checksum@ field to the checksum of the most recent revision of the @> LATEST@ version. If you don't specify the @checksum@ field, or if the checksum does not match the @> LATEST@ version, you get a @PreconditionFailedException@ exception.
pstChecksum :: Lens' PutSlotType (Maybe Text)
pstChecksum = lens _pstChecksum (\s a -> s {_pstChecksum = a})

-- | Determines the slot resolution strategy that Amazon Lex uses to return slot type values. The field can be set to one of the following values:     * @ORIGINAL_VALUE@ - Returns the value entered by the user, if the user value is similar to the slot value.     * @TOP_RESOLUTION@ - If there is a resolution list for the slot, return the first value in the resolution list as the slot type value. If there is no resolution list, null is returned. If you don't specify the @valueSelectionStrategy@ , the default is @ORIGINAL_VALUE@ .
pstValueSelectionStrategy :: Lens' PutSlotType (Maybe SlotValueSelectionStrategy)
pstValueSelectionStrategy = lens _pstValueSelectionStrategy (\s a -> s {_pstValueSelectionStrategy = a})

-- | When set to @true@ a new numbered version of the slot type is created. This is the same as calling the @CreateSlotTypeVersion@ operation. If you do not specify @createVersion@ , the default is @false@ .
pstCreateVersion :: Lens' PutSlotType (Maybe Bool)
pstCreateVersion = lens _pstCreateVersion (\s a -> s {_pstCreateVersion = a})

-- | A description of the slot type.
pstDescription :: Lens' PutSlotType (Maybe Text)
pstDescription = lens _pstDescription (\s a -> s {_pstDescription = a})

-- | A list of @EnumerationValue@ objects that defines the values that the slot type can take. Each value can have a list of @synonyms@ , which are additional values that help train the machine learning model about the values that it resolves for a slot.  A regular expression slot type doesn't require enumeration values. All other slot types require a list of enumeration values. When Amazon Lex resolves a slot value, it generates a resolution list that contains up to five possible values for the slot. If you are using a Lambda function, this resolution list is passed to the function. If you are not using a Lambda function you can choose to return the value that the user entered or the first value in the resolution list as the slot value. The @valueSelectionStrategy@ field indicates the option to use.
pstEnumerationValues :: Lens' PutSlotType [EnumerationValue]
pstEnumerationValues = lens _pstEnumerationValues (\s a -> s {_pstEnumerationValues = a}) . _Default . _Coerce

-- | The name of the slot type. The name is /not/ case sensitive.  The name can't match a built-in slot type name, or a built-in slot type name with "AMAZON." removed. For example, because there is a built-in slot type called @AMAZON.DATE@ , you can't create a custom slot type called @DATE@ . For a list of built-in slot types, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/slot-type-reference Slot Type Reference> in the /Alexa Skills Kit/ .
pstName :: Lens' PutSlotType Text
pstName = lens _pstName (\s a -> s {_pstName = a})

instance AWSRequest PutSlotType where
  type Rs PutSlotType = PutSlotTypeResponse
  request = putJSON lexModels
  response =
    receiveJSON
      ( \s h x ->
          PutSlotTypeResponse'
            <$> (x .?> "parentSlotTypeSignature")
            <*> (x .?> "slotTypeConfigurations" .!@ mempty)
            <*> (x .?> "checksum")
            <*> (x .?> "valueSelectionStrategy")
            <*> (x .?> "createdDate")
            <*> (x .?> "name")
            <*> (x .?> "version")
            <*> (x .?> "lastUpdatedDate")
            <*> (x .?> "createVersion")
            <*> (x .?> "description")
            <*> (x .?> "enumerationValues" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable PutSlotType

instance NFData PutSlotType

instance ToHeaders PutSlotType where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON PutSlotType where
  toJSON PutSlotType' {..} =
    object
      ( catMaybes
          [ ("parentSlotTypeSignature" .=) <$> _pstParentSlotTypeSignature,
            ("slotTypeConfigurations" .=) <$> _pstSlotTypeConfigurations,
            ("checksum" .=) <$> _pstChecksum,
            ("valueSelectionStrategy" .=) <$> _pstValueSelectionStrategy,
            ("createVersion" .=) <$> _pstCreateVersion,
            ("description" .=) <$> _pstDescription,
            ("enumerationValues" .=) <$> _pstEnumerationValues
          ]
      )

instance ToPath PutSlotType where
  toPath PutSlotType' {..} =
    mconcat ["/slottypes/", toBS _pstName, "/versions/$LATEST"]

instance ToQuery PutSlotType where
  toQuery = const mempty

-- | /See:/ 'putSlotTypeResponse' smart constructor.
data PutSlotTypeResponse = PutSlotTypeResponse'
  { _pstrsParentSlotTypeSignature ::
      !(Maybe Text),
    _pstrsSlotTypeConfigurations ::
      !(Maybe [SlotTypeConfiguration]),
    _pstrsChecksum :: !(Maybe Text),
    _pstrsValueSelectionStrategy ::
      !(Maybe SlotValueSelectionStrategy),
    _pstrsCreatedDate :: !(Maybe POSIX),
    _pstrsName :: !(Maybe Text),
    _pstrsVersion :: !(Maybe Text),
    _pstrsLastUpdatedDate :: !(Maybe POSIX),
    _pstrsCreateVersion :: !(Maybe Bool),
    _pstrsDescription :: !(Maybe Text),
    _pstrsEnumerationValues ::
      !(Maybe [EnumerationValue]),
    _pstrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutSlotTypeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pstrsParentSlotTypeSignature' - The built-in slot type used as the parent of the slot type.
--
-- * 'pstrsSlotTypeConfigurations' - Configuration information that extends the parent built-in slot type.
--
-- * 'pstrsChecksum' - Checksum of the @> LATEST@ version of the slot type.
--
-- * 'pstrsValueSelectionStrategy' - The slot resolution strategy that Amazon Lex uses to determine the value of the slot. For more information, see 'PutSlotType' .
--
-- * 'pstrsCreatedDate' - The date that the slot type was created.
--
-- * 'pstrsName' - The name of the slot type.
--
-- * 'pstrsVersion' - The version of the slot type. For a new slot type, the version is always @> LATEST@ .
--
-- * 'pstrsLastUpdatedDate' - The date that the slot type was updated. When you create a slot type, the creation date and last update date are the same.
--
-- * 'pstrsCreateVersion' - @True@ if a new version of the slot type was created. If the @createVersion@ field was not specified in the request, the @createVersion@ field is set to false in the response.
--
-- * 'pstrsDescription' - A description of the slot type.
--
-- * 'pstrsEnumerationValues' - A list of @EnumerationValue@ objects that defines the values that the slot type can take.
--
-- * 'pstrsResponseStatus' - -- | The response status code.
putSlotTypeResponse ::
  -- | 'pstrsResponseStatus'
  Int ->
  PutSlotTypeResponse
putSlotTypeResponse pResponseStatus_ =
  PutSlotTypeResponse'
    { _pstrsParentSlotTypeSignature = Nothing,
      _pstrsSlotTypeConfigurations = Nothing,
      _pstrsChecksum = Nothing,
      _pstrsValueSelectionStrategy = Nothing,
      _pstrsCreatedDate = Nothing,
      _pstrsName = Nothing,
      _pstrsVersion = Nothing,
      _pstrsLastUpdatedDate = Nothing,
      _pstrsCreateVersion = Nothing,
      _pstrsDescription = Nothing,
      _pstrsEnumerationValues = Nothing,
      _pstrsResponseStatus = pResponseStatus_
    }

-- | The built-in slot type used as the parent of the slot type.
pstrsParentSlotTypeSignature :: Lens' PutSlotTypeResponse (Maybe Text)
pstrsParentSlotTypeSignature = lens _pstrsParentSlotTypeSignature (\s a -> s {_pstrsParentSlotTypeSignature = a})

-- | Configuration information that extends the parent built-in slot type.
pstrsSlotTypeConfigurations :: Lens' PutSlotTypeResponse [SlotTypeConfiguration]
pstrsSlotTypeConfigurations = lens _pstrsSlotTypeConfigurations (\s a -> s {_pstrsSlotTypeConfigurations = a}) . _Default . _Coerce

-- | Checksum of the @> LATEST@ version of the slot type.
pstrsChecksum :: Lens' PutSlotTypeResponse (Maybe Text)
pstrsChecksum = lens _pstrsChecksum (\s a -> s {_pstrsChecksum = a})

-- | The slot resolution strategy that Amazon Lex uses to determine the value of the slot. For more information, see 'PutSlotType' .
pstrsValueSelectionStrategy :: Lens' PutSlotTypeResponse (Maybe SlotValueSelectionStrategy)
pstrsValueSelectionStrategy = lens _pstrsValueSelectionStrategy (\s a -> s {_pstrsValueSelectionStrategy = a})

-- | The date that the slot type was created.
pstrsCreatedDate :: Lens' PutSlotTypeResponse (Maybe UTCTime)
pstrsCreatedDate = lens _pstrsCreatedDate (\s a -> s {_pstrsCreatedDate = a}) . mapping _Time

-- | The name of the slot type.
pstrsName :: Lens' PutSlotTypeResponse (Maybe Text)
pstrsName = lens _pstrsName (\s a -> s {_pstrsName = a})

-- | The version of the slot type. For a new slot type, the version is always @> LATEST@ .
pstrsVersion :: Lens' PutSlotTypeResponse (Maybe Text)
pstrsVersion = lens _pstrsVersion (\s a -> s {_pstrsVersion = a})

-- | The date that the slot type was updated. When you create a slot type, the creation date and last update date are the same.
pstrsLastUpdatedDate :: Lens' PutSlotTypeResponse (Maybe UTCTime)
pstrsLastUpdatedDate = lens _pstrsLastUpdatedDate (\s a -> s {_pstrsLastUpdatedDate = a}) . mapping _Time

-- | @True@ if a new version of the slot type was created. If the @createVersion@ field was not specified in the request, the @createVersion@ field is set to false in the response.
pstrsCreateVersion :: Lens' PutSlotTypeResponse (Maybe Bool)
pstrsCreateVersion = lens _pstrsCreateVersion (\s a -> s {_pstrsCreateVersion = a})

-- | A description of the slot type.
pstrsDescription :: Lens' PutSlotTypeResponse (Maybe Text)
pstrsDescription = lens _pstrsDescription (\s a -> s {_pstrsDescription = a})

-- | A list of @EnumerationValue@ objects that defines the values that the slot type can take.
pstrsEnumerationValues :: Lens' PutSlotTypeResponse [EnumerationValue]
pstrsEnumerationValues = lens _pstrsEnumerationValues (\s a -> s {_pstrsEnumerationValues = a}) . _Default . _Coerce

-- | -- | The response status code.
pstrsResponseStatus :: Lens' PutSlotTypeResponse Int
pstrsResponseStatus = lens _pstrsResponseStatus (\s a -> s {_pstrsResponseStatus = a})

instance NFData PutSlotTypeResponse
