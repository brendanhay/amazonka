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
-- Copyright   : (c) 2013-2018 Brendan Hay
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
    , gstsrsChecksum
    , gstsrsValueSelectionStrategy
    , gstsrsCreatedDate
    , gstsrsName
    , gstsrsVersion
    , gstsrsLastUpdatedDate
    , gstsrsDescription
    , gstsrsEnumerationValues
    , gstsrsResponseStatus
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
gstName = lens _gstName (\ s a -> s{_gstName = a})

-- | The version of the slot type.
gstVersion :: Lens' GetSlotType Text
gstVersion = lens _gstVersion (\ s a -> s{_gstVersion = a})

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
  { _gstsrsChecksum               :: !(Maybe Text)
  , _gstsrsValueSelectionStrategy :: !(Maybe SlotValueSelectionStrategy)
  , _gstsrsCreatedDate            :: !(Maybe POSIX)
  , _gstsrsName                   :: !(Maybe Text)
  , _gstsrsVersion                :: !(Maybe Text)
  , _gstsrsLastUpdatedDate        :: !(Maybe POSIX)
  , _gstsrsDescription            :: !(Maybe Text)
  , _gstsrsEnumerationValues      :: !(Maybe (List1 EnumerationValue))
  , _gstsrsResponseStatus         :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSlotTypeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gstsrsChecksum' - Checksum of the @> LATEST@ version of the slot type.
--
-- * 'gstsrsValueSelectionStrategy' - The strategy that Amazon Lex uses to determine the value of the slot. For more information, see 'PutSlotType' .
--
-- * 'gstsrsCreatedDate' - The date that the slot type was created.
--
-- * 'gstsrsName' - The name of the slot type.
--
-- * 'gstsrsVersion' - The version of the slot type.
--
-- * 'gstsrsLastUpdatedDate' - The date that the slot type was updated. When you create a resource, the creation date and last update date are the same.
--
-- * 'gstsrsDescription' - A description of the slot type.
--
-- * 'gstsrsEnumerationValues' - A list of @EnumerationValue@ objects that defines the values that the slot type can take.
--
-- * 'gstsrsResponseStatus' - -- | The response status code.
getSlotTypeResponse
    :: Int -- ^ 'gstsrsResponseStatus'
    -> GetSlotTypeResponse
getSlotTypeResponse pResponseStatus_ =
  GetSlotTypeResponse'
    { _gstsrsChecksum = Nothing
    , _gstsrsValueSelectionStrategy = Nothing
    , _gstsrsCreatedDate = Nothing
    , _gstsrsName = Nothing
    , _gstsrsVersion = Nothing
    , _gstsrsLastUpdatedDate = Nothing
    , _gstsrsDescription = Nothing
    , _gstsrsEnumerationValues = Nothing
    , _gstsrsResponseStatus = pResponseStatus_
    }


-- | Checksum of the @> LATEST@ version of the slot type.
gstsrsChecksum :: Lens' GetSlotTypeResponse (Maybe Text)
gstsrsChecksum = lens _gstsrsChecksum (\ s a -> s{_gstsrsChecksum = a})

-- | The strategy that Amazon Lex uses to determine the value of the slot. For more information, see 'PutSlotType' .
gstsrsValueSelectionStrategy :: Lens' GetSlotTypeResponse (Maybe SlotValueSelectionStrategy)
gstsrsValueSelectionStrategy = lens _gstsrsValueSelectionStrategy (\ s a -> s{_gstsrsValueSelectionStrategy = a})

-- | The date that the slot type was created.
gstsrsCreatedDate :: Lens' GetSlotTypeResponse (Maybe UTCTime)
gstsrsCreatedDate = lens _gstsrsCreatedDate (\ s a -> s{_gstsrsCreatedDate = a}) . mapping _Time

-- | The name of the slot type.
gstsrsName :: Lens' GetSlotTypeResponse (Maybe Text)
gstsrsName = lens _gstsrsName (\ s a -> s{_gstsrsName = a})

-- | The version of the slot type.
gstsrsVersion :: Lens' GetSlotTypeResponse (Maybe Text)
gstsrsVersion = lens _gstsrsVersion (\ s a -> s{_gstsrsVersion = a})

-- | The date that the slot type was updated. When you create a resource, the creation date and last update date are the same.
gstsrsLastUpdatedDate :: Lens' GetSlotTypeResponse (Maybe UTCTime)
gstsrsLastUpdatedDate = lens _gstsrsLastUpdatedDate (\ s a -> s{_gstsrsLastUpdatedDate = a}) . mapping _Time

-- | A description of the slot type.
gstsrsDescription :: Lens' GetSlotTypeResponse (Maybe Text)
gstsrsDescription = lens _gstsrsDescription (\ s a -> s{_gstsrsDescription = a})

-- | A list of @EnumerationValue@ objects that defines the values that the slot type can take.
gstsrsEnumerationValues :: Lens' GetSlotTypeResponse (Maybe (NonEmpty EnumerationValue))
gstsrsEnumerationValues = lens _gstsrsEnumerationValues (\ s a -> s{_gstsrsEnumerationValues = a}) . mapping _List1

-- | -- | The response status code.
gstsrsResponseStatus :: Lens' GetSlotTypeResponse Int
gstsrsResponseStatus = lens _gstsrsResponseStatus (\ s a -> s{_gstsrsResponseStatus = a})

instance NFData GetSlotTypeResponse where
