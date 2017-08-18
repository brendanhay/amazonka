{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.PutSlotType
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a custom slot type or replaces an existing custom slot type.
--
--
-- To create a custom slot type, specify a name for the slot type and a set of enumeration values, which are the values that a slot of this type can assume. For more information, see 'how-it-works' .
--
-- If you specify the name of an existing slot type, the fields in the request replace the existing values in the @> LATEST@ version of the slot type. Amazon Lex removes the fields that you don't provide in the request. If you don't specify required fields, Amazon Lex throws an exception.
--
-- This operation requires permissions for the @lex:PutSlotType@ action.
--
module Network.AWS.LexModels.PutSlotType
    (
    -- * Creating a Request
      putSlotType
    , PutSlotType
    -- * Request Lenses
    , pstChecksum
    , pstDescription
    , pstEnumerationValues
    , pstName

    -- * Destructuring the Response
    , putSlotTypeResponse
    , PutSlotTypeResponse
    -- * Response Lenses
    , pstrsChecksum
    , pstrsCreatedDate
    , pstrsName
    , pstrsVersion
    , pstrsLastUpdatedDate
    , pstrsDescription
    , pstrsEnumerationValues
    , pstrsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.LexModels.Types
import           Network.AWS.LexModels.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'putSlotType' smart constructor.
data PutSlotType = PutSlotType'
    { _pstChecksum          :: !(Maybe Text)
    , _pstDescription       :: !(Maybe Text)
    , _pstEnumerationValues :: !(Maybe (List1 EnumerationValue))
    , _pstName              :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutSlotType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pstChecksum' - Identifies a specific revision of the @> LATEST@ version. When you create a new slot type, leave the @checksum@ field blank. If you specify a checksum you get a @BadRequestException@ exception. When you want to update a slot type, set the @checksum@ field to the checksum of the most recent revision of the @> LATEST@ version. If you don't specify the @checksum@ field, or if the checksum does not match the @> LATEST@ version, you get a @PreconditionFailedException@ exception.
--
-- * 'pstDescription' - A description of the slot type.
--
-- * 'pstEnumerationValues' - A list of @EnumerationValue@ objects that defines the values that the slot type can take.
--
-- * 'pstName' - The name of the slot type. The name is /not/ case sensitive.  The name can't match a built-in slot type name, or a built-in slot type name with "AMAZON." removed. For example, because there is a built-in slot type called @AMAZON.DATE@ , you can't create a custom slot type called @DATE@ . For a list of built-in slot types, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/slot-type-reference Slot Type Reference> in the /Alexa Skills Kit/ .
putSlotType
    :: Text -- ^ 'pstName'
    -> PutSlotType
putSlotType pName_ =
    PutSlotType'
    { _pstChecksum = Nothing
    , _pstDescription = Nothing
    , _pstEnumerationValues = Nothing
    , _pstName = pName_
    }

-- | Identifies a specific revision of the @> LATEST@ version. When you create a new slot type, leave the @checksum@ field blank. If you specify a checksum you get a @BadRequestException@ exception. When you want to update a slot type, set the @checksum@ field to the checksum of the most recent revision of the @> LATEST@ version. If you don't specify the @checksum@ field, or if the checksum does not match the @> LATEST@ version, you get a @PreconditionFailedException@ exception.
pstChecksum :: Lens' PutSlotType (Maybe Text)
pstChecksum = lens _pstChecksum (\ s a -> s{_pstChecksum = a});

-- | A description of the slot type.
pstDescription :: Lens' PutSlotType (Maybe Text)
pstDescription = lens _pstDescription (\ s a -> s{_pstDescription = a});

-- | A list of @EnumerationValue@ objects that defines the values that the slot type can take.
pstEnumerationValues :: Lens' PutSlotType (Maybe (NonEmpty EnumerationValue))
pstEnumerationValues = lens _pstEnumerationValues (\ s a -> s{_pstEnumerationValues = a}) . mapping _List1;

-- | The name of the slot type. The name is /not/ case sensitive.  The name can't match a built-in slot type name, or a built-in slot type name with "AMAZON." removed. For example, because there is a built-in slot type called @AMAZON.DATE@ , you can't create a custom slot type called @DATE@ . For a list of built-in slot types, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/slot-type-reference Slot Type Reference> in the /Alexa Skills Kit/ .
pstName :: Lens' PutSlotType Text
pstName = lens _pstName (\ s a -> s{_pstName = a});

instance AWSRequest PutSlotType where
        type Rs PutSlotType = PutSlotTypeResponse
        request = putJSON lexModels
        response
          = receiveJSON
              (\ s h x ->
                 PutSlotTypeResponse' <$>
                   (x .?> "checksum") <*> (x .?> "createdDate") <*>
                     (x .?> "name")
                     <*> (x .?> "version")
                     <*> (x .?> "lastUpdatedDate")
                     <*> (x .?> "description")
                     <*> (x .?> "enumerationValues")
                     <*> (pure (fromEnum s)))

instance Hashable PutSlotType

instance NFData PutSlotType

instance ToHeaders PutSlotType where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutSlotType where
        toJSON PutSlotType'{..}
          = object
              (catMaybes
                 [("checksum" .=) <$> _pstChecksum,
                  ("description" .=) <$> _pstDescription,
                  ("enumerationValues" .=) <$> _pstEnumerationValues])

instance ToPath PutSlotType where
        toPath PutSlotType'{..}
          = mconcat
              ["/slottypes/", toBS _pstName, "/versions/$LATEST"]

instance ToQuery PutSlotType where
        toQuery = const mempty

-- | /See:/ 'putSlotTypeResponse' smart constructor.
data PutSlotTypeResponse = PutSlotTypeResponse'
    { _pstrsChecksum          :: !(Maybe Text)
    , _pstrsCreatedDate       :: !(Maybe POSIX)
    , _pstrsName              :: !(Maybe Text)
    , _pstrsVersion           :: !(Maybe Text)
    , _pstrsLastUpdatedDate   :: !(Maybe POSIX)
    , _pstrsDescription       :: !(Maybe Text)
    , _pstrsEnumerationValues :: !(Maybe (List1 EnumerationValue))
    , _pstrsResponseStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutSlotTypeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pstrsChecksum' - Checksum of the @> LATEST@ version of the slot type.
--
-- * 'pstrsCreatedDate' - The date that the slot type was created.
--
-- * 'pstrsName' - The name of the slot type.
--
-- * 'pstrsVersion' - The version of the slot type. For a new slot type, the version is always @> LATEST@ .
--
-- * 'pstrsLastUpdatedDate' - The date that the slot type was updated. When you create a slot type, the creation date and last update date are the same.
--
-- * 'pstrsDescription' - A description of the slot type.
--
-- * 'pstrsEnumerationValues' - A list of @EnumerationValue@ objects that defines the values that the slot type can take.
--
-- * 'pstrsResponseStatus' - -- | The response status code.
putSlotTypeResponse
    :: Int -- ^ 'pstrsResponseStatus'
    -> PutSlotTypeResponse
putSlotTypeResponse pResponseStatus_ =
    PutSlotTypeResponse'
    { _pstrsChecksum = Nothing
    , _pstrsCreatedDate = Nothing
    , _pstrsName = Nothing
    , _pstrsVersion = Nothing
    , _pstrsLastUpdatedDate = Nothing
    , _pstrsDescription = Nothing
    , _pstrsEnumerationValues = Nothing
    , _pstrsResponseStatus = pResponseStatus_
    }

-- | Checksum of the @> LATEST@ version of the slot type.
pstrsChecksum :: Lens' PutSlotTypeResponse (Maybe Text)
pstrsChecksum = lens _pstrsChecksum (\ s a -> s{_pstrsChecksum = a});

-- | The date that the slot type was created.
pstrsCreatedDate :: Lens' PutSlotTypeResponse (Maybe UTCTime)
pstrsCreatedDate = lens _pstrsCreatedDate (\ s a -> s{_pstrsCreatedDate = a}) . mapping _Time;

-- | The name of the slot type.
pstrsName :: Lens' PutSlotTypeResponse (Maybe Text)
pstrsName = lens _pstrsName (\ s a -> s{_pstrsName = a});

-- | The version of the slot type. For a new slot type, the version is always @> LATEST@ .
pstrsVersion :: Lens' PutSlotTypeResponse (Maybe Text)
pstrsVersion = lens _pstrsVersion (\ s a -> s{_pstrsVersion = a});

-- | The date that the slot type was updated. When you create a slot type, the creation date and last update date are the same.
pstrsLastUpdatedDate :: Lens' PutSlotTypeResponse (Maybe UTCTime)
pstrsLastUpdatedDate = lens _pstrsLastUpdatedDate (\ s a -> s{_pstrsLastUpdatedDate = a}) . mapping _Time;

-- | A description of the slot type.
pstrsDescription :: Lens' PutSlotTypeResponse (Maybe Text)
pstrsDescription = lens _pstrsDescription (\ s a -> s{_pstrsDescription = a});

-- | A list of @EnumerationValue@ objects that defines the values that the slot type can take.
pstrsEnumerationValues :: Lens' PutSlotTypeResponse (Maybe (NonEmpty EnumerationValue))
pstrsEnumerationValues = lens _pstrsEnumerationValues (\ s a -> s{_pstrsEnumerationValues = a}) . mapping _List1;

-- | -- | The response status code.
pstrsResponseStatus :: Lens' PutSlotTypeResponse Int
pstrsResponseStatus = lens _pstrsResponseStatus (\ s a -> s{_pstrsResponseStatus = a});

instance NFData PutSlotTypeResponse
