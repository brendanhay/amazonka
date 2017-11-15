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
-- Module      : Network.AWS.LexModels.GetSlotType
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific version of a slot type. In addition to specifying the slot type name, you must specify the slot type version.
--
--
-- This operation requires permissions for the @lex:GetSlotType@ action.
--
module Network.AWS.LexModels.GetSlotType
    (
    -- * Creating a Request
      getSlotType
    , GetSlotType
    -- * Request Lenses
    , gstName
    , gstVersion

    -- * Destructuring the Response
    , getSlotTypeResponse
    , GetSlotTypeResponse
    -- * Response Lenses
    , getrsChecksum
    , getrsValueSelectionStrategy
    , getrsCreatedDate
    , getrsName
    , getrsVersion
    , getrsLastUpdatedDate
    , getrsDescription
    , getrsEnumerationValues
    , getrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.LexModels.Types
import Network.AWS.LexModels.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getSlotType' smart constructor.
data GetSlotType = GetSlotType'
  { _gstName    :: !Text
  , _gstVersion :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSlotType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gstName' - The name of the slot type. The name is case sensitive.
--
-- * 'gstVersion' - The version of the slot type.
getSlotType
    :: Text -- ^ 'gstName'
    -> Text -- ^ 'gstVersion'
    -> GetSlotType
getSlotType pName_ pVersion_ =
  GetSlotType' {_gstName = pName_, _gstVersion = pVersion_}


-- | The name of the slot type. The name is case sensitive.
gstName :: Lens' GetSlotType Text
gstName = lens _gstName (\ s a -> s{_gstName = a});

-- | The version of the slot type.
gstVersion :: Lens' GetSlotType Text
gstVersion = lens _gstVersion (\ s a -> s{_gstVersion = a});

instance AWSRequest GetSlotType where
        type Rs GetSlotType = GetSlotTypeResponse
        request = get lexModels
        response
          = receiveJSON
              (\ s h x ->
                 GetSlotTypeResponse' <$>
                   (x .?> "checksum") <*>
                     (x .?> "valueSelectionStrategy")
                     <*> (x .?> "createdDate")
                     <*> (x .?> "name")
                     <*> (x .?> "version")
                     <*> (x .?> "lastUpdatedDate")
                     <*> (x .?> "description")
                     <*> (x .?> "enumerationValues")
                     <*> (pure (fromEnum s)))

instance Hashable GetSlotType where

instance NFData GetSlotType where

instance ToHeaders GetSlotType where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetSlotType where
        toPath GetSlotType'{..}
          = mconcat
              ["/slottypes/", toBS _gstName, "/versions/",
               toBS _gstVersion]

instance ToQuery GetSlotType where
        toQuery = const mempty

-- | /See:/ 'getSlotTypeResponse' smart constructor.
data GetSlotTypeResponse = GetSlotTypeResponse'
  { _getrsChecksum               :: !(Maybe Text)
  , _getrsValueSelectionStrategy :: !(Maybe SlotValueSelectionStrategy)
  , _getrsCreatedDate            :: !(Maybe POSIX)
  , _getrsName                   :: !(Maybe Text)
  , _getrsVersion                :: !(Maybe Text)
  , _getrsLastUpdatedDate        :: !(Maybe POSIX)
  , _getrsDescription            :: !(Maybe Text)
  , _getrsEnumerationValues      :: !(Maybe (List1 EnumerationValue))
  , _getrsResponseStatus         :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSlotTypeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'getrsChecksum' - Checksum of the @> LATEST@ version of the slot type.
--
-- * 'getrsValueSelectionStrategy' - The strategy that Amazon Lex uses to determine the value of the slot. For more information, see 'PutSlotType' .
--
-- * 'getrsCreatedDate' - The date that the slot type was created.
--
-- * 'getrsName' - The name of the slot type.
--
-- * 'getrsVersion' - The version of the slot type.
--
-- * 'getrsLastUpdatedDate' - The date that the slot type was updated. When you create a resource, the creation date and last update date are the same.
--
-- * 'getrsDescription' - A description of the slot type.
--
-- * 'getrsEnumerationValues' - A list of @EnumerationValue@ objects that defines the values that the slot type can take.
--
-- * 'getrsResponseStatus' - -- | The response status code.
getSlotTypeResponse
    :: Int -- ^ 'getrsResponseStatus'
    -> GetSlotTypeResponse
getSlotTypeResponse pResponseStatus_ =
  GetSlotTypeResponse'
  { _getrsChecksum = Nothing
  , _getrsValueSelectionStrategy = Nothing
  , _getrsCreatedDate = Nothing
  , _getrsName = Nothing
  , _getrsVersion = Nothing
  , _getrsLastUpdatedDate = Nothing
  , _getrsDescription = Nothing
  , _getrsEnumerationValues = Nothing
  , _getrsResponseStatus = pResponseStatus_
  }


-- | Checksum of the @> LATEST@ version of the slot type.
getrsChecksum :: Lens' GetSlotTypeResponse (Maybe Text)
getrsChecksum = lens _getrsChecksum (\ s a -> s{_getrsChecksum = a});

-- | The strategy that Amazon Lex uses to determine the value of the slot. For more information, see 'PutSlotType' .
getrsValueSelectionStrategy :: Lens' GetSlotTypeResponse (Maybe SlotValueSelectionStrategy)
getrsValueSelectionStrategy = lens _getrsValueSelectionStrategy (\ s a -> s{_getrsValueSelectionStrategy = a});

-- | The date that the slot type was created.
getrsCreatedDate :: Lens' GetSlotTypeResponse (Maybe UTCTime)
getrsCreatedDate = lens _getrsCreatedDate (\ s a -> s{_getrsCreatedDate = a}) . mapping _Time;

-- | The name of the slot type.
getrsName :: Lens' GetSlotTypeResponse (Maybe Text)
getrsName = lens _getrsName (\ s a -> s{_getrsName = a});

-- | The version of the slot type.
getrsVersion :: Lens' GetSlotTypeResponse (Maybe Text)
getrsVersion = lens _getrsVersion (\ s a -> s{_getrsVersion = a});

-- | The date that the slot type was updated. When you create a resource, the creation date and last update date are the same.
getrsLastUpdatedDate :: Lens' GetSlotTypeResponse (Maybe UTCTime)
getrsLastUpdatedDate = lens _getrsLastUpdatedDate (\ s a -> s{_getrsLastUpdatedDate = a}) . mapping _Time;

-- | A description of the slot type.
getrsDescription :: Lens' GetSlotTypeResponse (Maybe Text)
getrsDescription = lens _getrsDescription (\ s a -> s{_getrsDescription = a});

-- | A list of @EnumerationValue@ objects that defines the values that the slot type can take.
getrsEnumerationValues :: Lens' GetSlotTypeResponse (Maybe (NonEmpty EnumerationValue))
getrsEnumerationValues = lens _getrsEnumerationValues (\ s a -> s{_getrsEnumerationValues = a}) . mapping _List1;

-- | -- | The response status code.
getrsResponseStatus :: Lens' GetSlotTypeResponse Int
getrsResponseStatus = lens _getrsResponseStatus (\ s a -> s{_getrsResponseStatus = a});

instance NFData GetSlotTypeResponse where
