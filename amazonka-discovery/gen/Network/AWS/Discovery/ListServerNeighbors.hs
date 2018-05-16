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
-- Module      : Network.AWS.Discovery.ListServerNeighbors
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of servers that are one network hop away from a specified server.
--
--
module Network.AWS.Discovery.ListServerNeighbors
    (
    -- * Creating a Request
      listServerNeighbors
    , ListServerNeighbors
    -- * Request Lenses
    , lsnPortInformationNeeded
    , lsnNeighborConfigurationIds
    , lsnNextToken
    , lsnMaxResults
    , lsnConfigurationId

    -- * Destructuring the Response
    , listServerNeighborsResponse
    , ListServerNeighborsResponse
    -- * Response Lenses
    , lsnrsNextToken
    , lsnrsKnownDependencyCount
    , lsnrsResponseStatus
    , lsnrsNeighbors
    ) where

import Network.AWS.Discovery.Types
import Network.AWS.Discovery.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listServerNeighbors' smart constructor.
data ListServerNeighbors = ListServerNeighbors'
  { _lsnPortInformationNeeded    :: !(Maybe Bool)
  , _lsnNeighborConfigurationIds :: !(Maybe [Text])
  , _lsnNextToken                :: !(Maybe Text)
  , _lsnMaxResults               :: !(Maybe Int)
  , _lsnConfigurationId          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListServerNeighbors' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsnPortInformationNeeded' - Flag to indicate if port and protocol information is needed as part of the response.
--
-- * 'lsnNeighborConfigurationIds' - List of configuration IDs to test for one-hop-away.
--
-- * 'lsnNextToken' - Token to retrieve the next set of results. For example, if you previously specified 100 IDs for @ListServerNeighborsRequest$neighborConfigurationIds@ but set @ListServerNeighborsRequest$maxResults@ to 10, you received a set of 10 results along with a token. Use that token in this query to get the next set of 10.
--
-- * 'lsnMaxResults' - Maximum number of results to return in a single page of output.
--
-- * 'lsnConfigurationId' - Configuration ID of the server for which neighbors are being listed.
listServerNeighbors
    :: Text -- ^ 'lsnConfigurationId'
    -> ListServerNeighbors
listServerNeighbors pConfigurationId_ =
  ListServerNeighbors'
    { _lsnPortInformationNeeded = Nothing
    , _lsnNeighborConfigurationIds = Nothing
    , _lsnNextToken = Nothing
    , _lsnMaxResults = Nothing
    , _lsnConfigurationId = pConfigurationId_
    }


-- | Flag to indicate if port and protocol information is needed as part of the response.
lsnPortInformationNeeded :: Lens' ListServerNeighbors (Maybe Bool)
lsnPortInformationNeeded = lens _lsnPortInformationNeeded (\ s a -> s{_lsnPortInformationNeeded = a})

-- | List of configuration IDs to test for one-hop-away.
lsnNeighborConfigurationIds :: Lens' ListServerNeighbors [Text]
lsnNeighborConfigurationIds = lens _lsnNeighborConfigurationIds (\ s a -> s{_lsnNeighborConfigurationIds = a}) . _Default . _Coerce

-- | Token to retrieve the next set of results. For example, if you previously specified 100 IDs for @ListServerNeighborsRequest$neighborConfigurationIds@ but set @ListServerNeighborsRequest$maxResults@ to 10, you received a set of 10 results along with a token. Use that token in this query to get the next set of 10.
lsnNextToken :: Lens' ListServerNeighbors (Maybe Text)
lsnNextToken = lens _lsnNextToken (\ s a -> s{_lsnNextToken = a})

-- | Maximum number of results to return in a single page of output.
lsnMaxResults :: Lens' ListServerNeighbors (Maybe Int)
lsnMaxResults = lens _lsnMaxResults (\ s a -> s{_lsnMaxResults = a})

-- | Configuration ID of the server for which neighbors are being listed.
lsnConfigurationId :: Lens' ListServerNeighbors Text
lsnConfigurationId = lens _lsnConfigurationId (\ s a -> s{_lsnConfigurationId = a})

instance AWSRequest ListServerNeighbors where
        type Rs ListServerNeighbors =
             ListServerNeighborsResponse
        request = postJSON discovery
        response
          = receiveJSON
              (\ s h x ->
                 ListServerNeighborsResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "knownDependencyCount")
                     <*> (pure (fromEnum s))
                     <*> (x .?> "neighbors" .!@ mempty))

instance Hashable ListServerNeighbors where

instance NFData ListServerNeighbors where

instance ToHeaders ListServerNeighbors where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSPoseidonService_V2015_11_01.ListServerNeighbors"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListServerNeighbors where
        toJSON ListServerNeighbors'{..}
          = object
              (catMaybes
                 [("portInformationNeeded" .=) <$>
                    _lsnPortInformationNeeded,
                  ("neighborConfigurationIds" .=) <$>
                    _lsnNeighborConfigurationIds,
                  ("nextToken" .=) <$> _lsnNextToken,
                  ("maxResults" .=) <$> _lsnMaxResults,
                  Just ("configurationId" .= _lsnConfigurationId)])

instance ToPath ListServerNeighbors where
        toPath = const "/"

instance ToQuery ListServerNeighbors where
        toQuery = const mempty

-- | /See:/ 'listServerNeighborsResponse' smart constructor.
data ListServerNeighborsResponse = ListServerNeighborsResponse'
  { _lsnrsNextToken            :: !(Maybe Text)
  , _lsnrsKnownDependencyCount :: !(Maybe Integer)
  , _lsnrsResponseStatus       :: !Int
  , _lsnrsNeighbors            :: ![NeighborConnectionDetail]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListServerNeighborsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsnrsNextToken' - Token to retrieve the next set of results. For example, if you specified 100 IDs for @ListServerNeighborsRequest$neighborConfigurationIds@ but set @ListServerNeighborsRequest$maxResults@ to 10, you received a set of 10 results along with this token. Use this token in the next query to retrieve the next set of 10.
--
-- * 'lsnrsKnownDependencyCount' - Count of distinct servers that are one hop away from the given server.
--
-- * 'lsnrsResponseStatus' - -- | The response status code.
--
-- * 'lsnrsNeighbors' - List of distinct servers that are one hop away from the given server.
listServerNeighborsResponse
    :: Int -- ^ 'lsnrsResponseStatus'
    -> ListServerNeighborsResponse
listServerNeighborsResponse pResponseStatus_ =
  ListServerNeighborsResponse'
    { _lsnrsNextToken = Nothing
    , _lsnrsKnownDependencyCount = Nothing
    , _lsnrsResponseStatus = pResponseStatus_
    , _lsnrsNeighbors = mempty
    }


-- | Token to retrieve the next set of results. For example, if you specified 100 IDs for @ListServerNeighborsRequest$neighborConfigurationIds@ but set @ListServerNeighborsRequest$maxResults@ to 10, you received a set of 10 results along with this token. Use this token in the next query to retrieve the next set of 10.
lsnrsNextToken :: Lens' ListServerNeighborsResponse (Maybe Text)
lsnrsNextToken = lens _lsnrsNextToken (\ s a -> s{_lsnrsNextToken = a})

-- | Count of distinct servers that are one hop away from the given server.
lsnrsKnownDependencyCount :: Lens' ListServerNeighborsResponse (Maybe Integer)
lsnrsKnownDependencyCount = lens _lsnrsKnownDependencyCount (\ s a -> s{_lsnrsKnownDependencyCount = a})

-- | -- | The response status code.
lsnrsResponseStatus :: Lens' ListServerNeighborsResponse Int
lsnrsResponseStatus = lens _lsnrsResponseStatus (\ s a -> s{_lsnrsResponseStatus = a})

-- | List of distinct servers that are one hop away from the given server.
lsnrsNeighbors :: Lens' ListServerNeighborsResponse [NeighborConnectionDetail]
lsnrsNeighbors = lens _lsnrsNeighbors (\ s a -> s{_lsnrsNeighbors = a}) . _Coerce

instance NFData ListServerNeighborsResponse where
