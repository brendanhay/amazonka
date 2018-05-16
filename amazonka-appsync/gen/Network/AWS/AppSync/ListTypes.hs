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
-- Module      : Network.AWS.AppSync.ListTypes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the types for a given API.
--
--
module Network.AWS.AppSync.ListTypes
    (
    -- * Creating a Request
      listTypes
    , ListTypes
    -- * Request Lenses
    , ltNextToken
    , ltMaxResults
    , ltApiId
    , ltFormat

    -- * Destructuring the Response
    , listTypesResponse
    , ListTypesResponse
    -- * Response Lenses
    , ltrsTypes
    , ltrsNextToken
    , ltrsResponseStatus
    ) where

import Network.AWS.AppSync.Types
import Network.AWS.AppSync.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listTypes' smart constructor.
data ListTypes = ListTypes'
  { _ltNextToken  :: !(Maybe Text)
  , _ltMaxResults :: !(Maybe Nat)
  , _ltApiId      :: !Text
  , _ltFormat     :: !TypeDefinitionFormat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTypes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltNextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- * 'ltMaxResults' - The maximum number of results you want the request to return.
--
-- * 'ltApiId' - The API ID.
--
-- * 'ltFormat' - The type format: SDL or JSON.
listTypes
    :: Text -- ^ 'ltApiId'
    -> TypeDefinitionFormat -- ^ 'ltFormat'
    -> ListTypes
listTypes pApiId_ pFormat_ =
  ListTypes'
    { _ltNextToken = Nothing
    , _ltMaxResults = Nothing
    , _ltApiId = pApiId_
    , _ltFormat = pFormat_
    }


-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
ltNextToken :: Lens' ListTypes (Maybe Text)
ltNextToken = lens _ltNextToken (\ s a -> s{_ltNextToken = a})

-- | The maximum number of results you want the request to return.
ltMaxResults :: Lens' ListTypes (Maybe Natural)
ltMaxResults = lens _ltMaxResults (\ s a -> s{_ltMaxResults = a}) . mapping _Nat

-- | The API ID.
ltApiId :: Lens' ListTypes Text
ltApiId = lens _ltApiId (\ s a -> s{_ltApiId = a})

-- | The type format: SDL or JSON.
ltFormat :: Lens' ListTypes TypeDefinitionFormat
ltFormat = lens _ltFormat (\ s a -> s{_ltFormat = a})

instance AWSRequest ListTypes where
        type Rs ListTypes = ListTypesResponse
        request = get appSync
        response
          = receiveJSON
              (\ s h x ->
                 ListTypesResponse' <$>
                   (x .?> "types" .!@ mempty) <*> (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListTypes where

instance NFData ListTypes where

instance ToHeaders ListTypes where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListTypes where
        toPath ListTypes'{..}
          = mconcat ["/v1/apis/", toBS _ltApiId, "/types"]

instance ToQuery ListTypes where
        toQuery ListTypes'{..}
          = mconcat
              ["nextToken" =: _ltNextToken,
               "maxResults" =: _ltMaxResults, "format" =: _ltFormat]

-- | /See:/ 'listTypesResponse' smart constructor.
data ListTypesResponse = ListTypesResponse'
  { _ltrsTypes          :: !(Maybe [Type])
  , _ltrsNextToken      :: !(Maybe Text)
  , _ltrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTypesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltrsTypes' - The @Type@ objects.
--
-- * 'ltrsNextToken' - An identifier to be passed in the next request to this operation to return the next set of items in the list.
--
-- * 'ltrsResponseStatus' - -- | The response status code.
listTypesResponse
    :: Int -- ^ 'ltrsResponseStatus'
    -> ListTypesResponse
listTypesResponse pResponseStatus_ =
  ListTypesResponse'
    { _ltrsTypes = Nothing
    , _ltrsNextToken = Nothing
    , _ltrsResponseStatus = pResponseStatus_
    }


-- | The @Type@ objects.
ltrsTypes :: Lens' ListTypesResponse [Type]
ltrsTypes = lens _ltrsTypes (\ s a -> s{_ltrsTypes = a}) . _Default . _Coerce

-- | An identifier to be passed in the next request to this operation to return the next set of items in the list.
ltrsNextToken :: Lens' ListTypesResponse (Maybe Text)
ltrsNextToken = lens _ltrsNextToken (\ s a -> s{_ltrsNextToken = a})

-- | -- | The response status code.
ltrsResponseStatus :: Lens' ListTypesResponse Int
ltrsResponseStatus = lens _ltrsResponseStatus (\ s a -> s{_ltrsResponseStatus = a})

instance NFData ListTypesResponse where
