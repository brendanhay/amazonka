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
-- Module      : Network.AWS.SSM.ListInventoryEntries
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A list of inventory items returned by the request.
--
--
module Network.AWS.SSM.ListInventoryEntries
    (
    -- * Creating a Request
      listInventoryEntries
    , ListInventoryEntries
    -- * Request Lenses
    , lieFilters
    , lieNextToken
    , lieMaxResults
    , lieInstanceId
    , lieTypeName

    -- * Destructuring the Response
    , listInventoryEntriesResponse
    , ListInventoryEntriesResponse
    -- * Response Lenses
    , liersInstanceId
    , liersTypeName
    , liersEntries
    , liersSchemaVersion
    , liersCaptureTime
    , liersNextToken
    , liersResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'listInventoryEntries' smart constructor.
data ListInventoryEntries = ListInventoryEntries'
  { _lieFilters    :: !(Maybe (List1 InventoryFilter))
  , _lieNextToken  :: !(Maybe Text)
  , _lieMaxResults :: !(Maybe Nat)
  , _lieInstanceId :: !Text
  , _lieTypeName   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListInventoryEntries' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lieFilters' - One or more filters. Use a filter to return a more specific list of results.
--
-- * 'lieNextToken' - The token for the next set of items to return. (You received this token from a previous call.)
--
-- * 'lieMaxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- * 'lieInstanceId' - The instance ID for which you want inventory information.
--
-- * 'lieTypeName' - The type of inventory item for which you want information.
listInventoryEntries
    :: Text -- ^ 'lieInstanceId'
    -> Text -- ^ 'lieTypeName'
    -> ListInventoryEntries
listInventoryEntries pInstanceId_ pTypeName_ =
  ListInventoryEntries'
    { _lieFilters = Nothing
    , _lieNextToken = Nothing
    , _lieMaxResults = Nothing
    , _lieInstanceId = pInstanceId_
    , _lieTypeName = pTypeName_
    }


-- | One or more filters. Use a filter to return a more specific list of results.
lieFilters :: Lens' ListInventoryEntries (Maybe (NonEmpty InventoryFilter))
lieFilters = lens _lieFilters (\ s a -> s{_lieFilters = a}) . mapping _List1

-- | The token for the next set of items to return. (You received this token from a previous call.)
lieNextToken :: Lens' ListInventoryEntries (Maybe Text)
lieNextToken = lens _lieNextToken (\ s a -> s{_lieNextToken = a})

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
lieMaxResults :: Lens' ListInventoryEntries (Maybe Natural)
lieMaxResults = lens _lieMaxResults (\ s a -> s{_lieMaxResults = a}) . mapping _Nat

-- | The instance ID for which you want inventory information.
lieInstanceId :: Lens' ListInventoryEntries Text
lieInstanceId = lens _lieInstanceId (\ s a -> s{_lieInstanceId = a})

-- | The type of inventory item for which you want information.
lieTypeName :: Lens' ListInventoryEntries Text
lieTypeName = lens _lieTypeName (\ s a -> s{_lieTypeName = a})

instance AWSRequest ListInventoryEntries where
        type Rs ListInventoryEntries =
             ListInventoryEntriesResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 ListInventoryEntriesResponse' <$>
                   (x .?> "InstanceId") <*> (x .?> "TypeName") <*>
                     (x .?> "Entries" .!@ mempty)
                     <*> (x .?> "SchemaVersion")
                     <*> (x .?> "CaptureTime")
                     <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListInventoryEntries where

instance NFData ListInventoryEntries where

instance ToHeaders ListInventoryEntries where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.ListInventoryEntries" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListInventoryEntries where
        toJSON ListInventoryEntries'{..}
          = object
              (catMaybes
                 [("Filters" .=) <$> _lieFilters,
                  ("NextToken" .=) <$> _lieNextToken,
                  ("MaxResults" .=) <$> _lieMaxResults,
                  Just ("InstanceId" .= _lieInstanceId),
                  Just ("TypeName" .= _lieTypeName)])

instance ToPath ListInventoryEntries where
        toPath = const "/"

instance ToQuery ListInventoryEntries where
        toQuery = const mempty

-- | /See:/ 'listInventoryEntriesResponse' smart constructor.
data ListInventoryEntriesResponse = ListInventoryEntriesResponse'
  { _liersInstanceId     :: !(Maybe Text)
  , _liersTypeName       :: !(Maybe Text)
  , _liersEntries        :: !(Maybe [Map Text Text])
  , _liersSchemaVersion  :: !(Maybe Text)
  , _liersCaptureTime    :: !(Maybe Text)
  , _liersNextToken      :: !(Maybe Text)
  , _liersResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListInventoryEntriesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'liersInstanceId' - The instance ID targeted by the request to query inventory information.
--
-- * 'liersTypeName' - The type of inventory item returned by the request.
--
-- * 'liersEntries' - A list of inventory items on the instance(s).
--
-- * 'liersSchemaVersion' - The inventory schema version used by the instance(s).
--
-- * 'liersCaptureTime' - The time that inventory information was collected for the instance(s).
--
-- * 'liersNextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- * 'liersResponseStatus' - -- | The response status code.
listInventoryEntriesResponse
    :: Int -- ^ 'liersResponseStatus'
    -> ListInventoryEntriesResponse
listInventoryEntriesResponse pResponseStatus_ =
  ListInventoryEntriesResponse'
    { _liersInstanceId = Nothing
    , _liersTypeName = Nothing
    , _liersEntries = Nothing
    , _liersSchemaVersion = Nothing
    , _liersCaptureTime = Nothing
    , _liersNextToken = Nothing
    , _liersResponseStatus = pResponseStatus_
    }


-- | The instance ID targeted by the request to query inventory information.
liersInstanceId :: Lens' ListInventoryEntriesResponse (Maybe Text)
liersInstanceId = lens _liersInstanceId (\ s a -> s{_liersInstanceId = a})

-- | The type of inventory item returned by the request.
liersTypeName :: Lens' ListInventoryEntriesResponse (Maybe Text)
liersTypeName = lens _liersTypeName (\ s a -> s{_liersTypeName = a})

-- | A list of inventory items on the instance(s).
liersEntries :: Lens' ListInventoryEntriesResponse [HashMap Text Text]
liersEntries = lens _liersEntries (\ s a -> s{_liersEntries = a}) . _Default . _Coerce

-- | The inventory schema version used by the instance(s).
liersSchemaVersion :: Lens' ListInventoryEntriesResponse (Maybe Text)
liersSchemaVersion = lens _liersSchemaVersion (\ s a -> s{_liersSchemaVersion = a})

-- | The time that inventory information was collected for the instance(s).
liersCaptureTime :: Lens' ListInventoryEntriesResponse (Maybe Text)
liersCaptureTime = lens _liersCaptureTime (\ s a -> s{_liersCaptureTime = a})

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
liersNextToken :: Lens' ListInventoryEntriesResponse (Maybe Text)
liersNextToken = lens _liersNextToken (\ s a -> s{_liersNextToken = a})

-- | -- | The response status code.
liersResponseStatus :: Lens' ListInventoryEntriesResponse Int
liersResponseStatus = lens _liersResponseStatus (\ s a -> s{_liersResponseStatus = a})

instance NFData ListInventoryEntriesResponse where
