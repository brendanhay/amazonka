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
-- Module      : Network.AWS.LexModels.CreateSlotTypeVersion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new version of a slot type based on the @> LATEST@ version of the specified slot type. If the @> LATEST@ version of this resource has not changed since the last version that you created, Amazon Lex doesn't create a new version. It returns the last version that you created.
--
--
-- When you create a version of a slot type, Amazon Lex sets the version to 1. Subsequent versions increment by 1. For more information, see 'versioning-intro' .
--
-- This operation requires permissions for the @lex:CreateSlotTypeVersion@ action.
--
module Network.AWS.LexModels.CreateSlotTypeVersion
    (
    -- * Creating a Request
      createSlotTypeVersion
    , CreateSlotTypeVersion
    -- * Request Lenses
    , cstvChecksum
    , cstvName

    -- * Destructuring the Response
    , createSlotTypeVersionResponse
    , CreateSlotTypeVersionResponse
    -- * Response Lenses
    , cstvrsChecksum
    , cstvrsValueSelectionStrategy
    , cstvrsCreatedDate
    , cstvrsName
    , cstvrsVersion
    , cstvrsLastUpdatedDate
    , cstvrsDescription
    , cstvrsEnumerationValues
    , cstvrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.LexModels.Types
import Network.AWS.LexModels.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createSlotTypeVersion' smart constructor.
data CreateSlotTypeVersion = CreateSlotTypeVersion'
  { _cstvChecksum :: !(Maybe Text)
  , _cstvName     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateSlotTypeVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cstvChecksum' - Checksum for the @> LATEST@ version of the slot type that you want to publish. If you specify a checksum and the @> LATEST@ version of the slot type has a different checksum, Amazon Lex returns a @PreconditionFailedException@ exception and doesn't publish the new version. If you don't specify a checksum, Amazon Lex publishes the @> LATEST@ version.
--
-- * 'cstvName' - The name of the slot type that you want to create a new version for. The name is case sensitive.
createSlotTypeVersion
    :: Text -- ^ 'cstvName'
    -> CreateSlotTypeVersion
createSlotTypeVersion pName_ =
  CreateSlotTypeVersion' {_cstvChecksum = Nothing, _cstvName = pName_}


-- | Checksum for the @> LATEST@ version of the slot type that you want to publish. If you specify a checksum and the @> LATEST@ version of the slot type has a different checksum, Amazon Lex returns a @PreconditionFailedException@ exception and doesn't publish the new version. If you don't specify a checksum, Amazon Lex publishes the @> LATEST@ version.
cstvChecksum :: Lens' CreateSlotTypeVersion (Maybe Text)
cstvChecksum = lens _cstvChecksum (\ s a -> s{_cstvChecksum = a})

-- | The name of the slot type that you want to create a new version for. The name is case sensitive.
cstvName :: Lens' CreateSlotTypeVersion Text
cstvName = lens _cstvName (\ s a -> s{_cstvName = a})

instance AWSRequest CreateSlotTypeVersion where
        type Rs CreateSlotTypeVersion =
             CreateSlotTypeVersionResponse
        request = postJSON lexModels
        response
          = receiveJSON
              (\ s h x ->
                 CreateSlotTypeVersionResponse' <$>
                   (x .?> "checksum") <*>
                     (x .?> "valueSelectionStrategy")
                     <*> (x .?> "createdDate")
                     <*> (x .?> "name")
                     <*> (x .?> "version")
                     <*> (x .?> "lastUpdatedDate")
                     <*> (x .?> "description")
                     <*> (x .?> "enumerationValues")
                     <*> (pure (fromEnum s)))

instance Hashable CreateSlotTypeVersion where

instance NFData CreateSlotTypeVersion where

instance ToHeaders CreateSlotTypeVersion where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateSlotTypeVersion where
        toJSON CreateSlotTypeVersion'{..}
          = object
              (catMaybes [("checksum" .=) <$> _cstvChecksum])

instance ToPath CreateSlotTypeVersion where
        toPath CreateSlotTypeVersion'{..}
          = mconcat
              ["/slottypes/", toBS _cstvName, "/versions"]

instance ToQuery CreateSlotTypeVersion where
        toQuery = const mempty

-- | /See:/ 'createSlotTypeVersionResponse' smart constructor.
data CreateSlotTypeVersionResponse = CreateSlotTypeVersionResponse'
  { _cstvrsChecksum               :: !(Maybe Text)
  , _cstvrsValueSelectionStrategy :: !(Maybe SlotValueSelectionStrategy)
  , _cstvrsCreatedDate            :: !(Maybe POSIX)
  , _cstvrsName                   :: !(Maybe Text)
  , _cstvrsVersion                :: !(Maybe Text)
  , _cstvrsLastUpdatedDate        :: !(Maybe POSIX)
  , _cstvrsDescription            :: !(Maybe Text)
  , _cstvrsEnumerationValues      :: !(Maybe (List1 EnumerationValue))
  , _cstvrsResponseStatus         :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateSlotTypeVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cstvrsChecksum' - Checksum of the @> LATEST@ version of the slot type.
--
-- * 'cstvrsValueSelectionStrategy' - The strategy that Amazon Lex uses to determine the value of the slot. For more information, see 'PutSlotType' .
--
-- * 'cstvrsCreatedDate' - The date that the slot type was created.
--
-- * 'cstvrsName' - The name of the slot type.
--
-- * 'cstvrsVersion' - The version assigned to the new slot type version.
--
-- * 'cstvrsLastUpdatedDate' - The date that the slot type was updated. When you create a resource, the creation date and last update date are the same.
--
-- * 'cstvrsDescription' - A description of the slot type.
--
-- * 'cstvrsEnumerationValues' - A list of @EnumerationValue@ objects that defines the values that the slot type can take.
--
-- * 'cstvrsResponseStatus' - -- | The response status code.
createSlotTypeVersionResponse
    :: Int -- ^ 'cstvrsResponseStatus'
    -> CreateSlotTypeVersionResponse
createSlotTypeVersionResponse pResponseStatus_ =
  CreateSlotTypeVersionResponse'
    { _cstvrsChecksum = Nothing
    , _cstvrsValueSelectionStrategy = Nothing
    , _cstvrsCreatedDate = Nothing
    , _cstvrsName = Nothing
    , _cstvrsVersion = Nothing
    , _cstvrsLastUpdatedDate = Nothing
    , _cstvrsDescription = Nothing
    , _cstvrsEnumerationValues = Nothing
    , _cstvrsResponseStatus = pResponseStatus_
    }


-- | Checksum of the @> LATEST@ version of the slot type.
cstvrsChecksum :: Lens' CreateSlotTypeVersionResponse (Maybe Text)
cstvrsChecksum = lens _cstvrsChecksum (\ s a -> s{_cstvrsChecksum = a})

-- | The strategy that Amazon Lex uses to determine the value of the slot. For more information, see 'PutSlotType' .
cstvrsValueSelectionStrategy :: Lens' CreateSlotTypeVersionResponse (Maybe SlotValueSelectionStrategy)
cstvrsValueSelectionStrategy = lens _cstvrsValueSelectionStrategy (\ s a -> s{_cstvrsValueSelectionStrategy = a})

-- | The date that the slot type was created.
cstvrsCreatedDate :: Lens' CreateSlotTypeVersionResponse (Maybe UTCTime)
cstvrsCreatedDate = lens _cstvrsCreatedDate (\ s a -> s{_cstvrsCreatedDate = a}) . mapping _Time

-- | The name of the slot type.
cstvrsName :: Lens' CreateSlotTypeVersionResponse (Maybe Text)
cstvrsName = lens _cstvrsName (\ s a -> s{_cstvrsName = a})

-- | The version assigned to the new slot type version.
cstvrsVersion :: Lens' CreateSlotTypeVersionResponse (Maybe Text)
cstvrsVersion = lens _cstvrsVersion (\ s a -> s{_cstvrsVersion = a})

-- | The date that the slot type was updated. When you create a resource, the creation date and last update date are the same.
cstvrsLastUpdatedDate :: Lens' CreateSlotTypeVersionResponse (Maybe UTCTime)
cstvrsLastUpdatedDate = lens _cstvrsLastUpdatedDate (\ s a -> s{_cstvrsLastUpdatedDate = a}) . mapping _Time

-- | A description of the slot type.
cstvrsDescription :: Lens' CreateSlotTypeVersionResponse (Maybe Text)
cstvrsDescription = lens _cstvrsDescription (\ s a -> s{_cstvrsDescription = a})

-- | A list of @EnumerationValue@ objects that defines the values that the slot type can take.
cstvrsEnumerationValues :: Lens' CreateSlotTypeVersionResponse (Maybe (NonEmpty EnumerationValue))
cstvrsEnumerationValues = lens _cstvrsEnumerationValues (\ s a -> s{_cstvrsEnumerationValues = a}) . mapping _List1

-- | -- | The response status code.
cstvrsResponseStatus :: Lens' CreateSlotTypeVersionResponse Int
cstvrsResponseStatus = lens _cstvrsResponseStatus (\ s a -> s{_cstvrsResponseStatus = a})

instance NFData CreateSlotTypeVersionResponse where
