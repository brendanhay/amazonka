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
-- Module      : Network.AWS.IoT.SearchIndex
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The query search index.
--
--
module Network.AWS.IoT.SearchIndex
    (
    -- * Creating a Request
      searchIndex
    , SearchIndex
    -- * Request Lenses
    , siQueryVersion
    , siNextToken
    , siMaxResults
    , siIndexName
    , siQueryString

    -- * Destructuring the Response
    , searchIndexResponse
    , SearchIndexResponse
    -- * Response Lenses
    , sirsNextToken
    , sirsThings
    , sirsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'searchIndex' smart constructor.
data SearchIndex = SearchIndex'
  { _siQueryVersion :: !(Maybe Text)
  , _siNextToken    :: !(Maybe Text)
  , _siMaxResults   :: !(Maybe Nat)
  , _siIndexName    :: !(Maybe Text)
  , _siQueryString  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SearchIndex' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'siQueryVersion' - The query version.
--
-- * 'siNextToken' - The token used to get the next set of results, or __null__ if there are no additional results.
--
-- * 'siMaxResults' - The maximum number of results to return at one time.
--
-- * 'siIndexName' - The search index name.
--
-- * 'siQueryString' - The search query string.
searchIndex
    :: Text -- ^ 'siQueryString'
    -> SearchIndex
searchIndex pQueryString_ =
  SearchIndex'
    { _siQueryVersion = Nothing
    , _siNextToken = Nothing
    , _siMaxResults = Nothing
    , _siIndexName = Nothing
    , _siQueryString = pQueryString_
    }


-- | The query version.
siQueryVersion :: Lens' SearchIndex (Maybe Text)
siQueryVersion = lens _siQueryVersion (\ s a -> s{_siQueryVersion = a})

-- | The token used to get the next set of results, or __null__ if there are no additional results.
siNextToken :: Lens' SearchIndex (Maybe Text)
siNextToken = lens _siNextToken (\ s a -> s{_siNextToken = a})

-- | The maximum number of results to return at one time.
siMaxResults :: Lens' SearchIndex (Maybe Natural)
siMaxResults = lens _siMaxResults (\ s a -> s{_siMaxResults = a}) . mapping _Nat

-- | The search index name.
siIndexName :: Lens' SearchIndex (Maybe Text)
siIndexName = lens _siIndexName (\ s a -> s{_siIndexName = a})

-- | The search query string.
siQueryString :: Lens' SearchIndex Text
siQueryString = lens _siQueryString (\ s a -> s{_siQueryString = a})

instance AWSRequest SearchIndex where
        type Rs SearchIndex = SearchIndexResponse
        request = postJSON ioT
        response
          = receiveJSON
              (\ s h x ->
                 SearchIndexResponse' <$>
                   (x .?> "nextToken") <*> (x .?> "things" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable SearchIndex where

instance NFData SearchIndex where

instance ToHeaders SearchIndex where
        toHeaders = const mempty

instance ToJSON SearchIndex where
        toJSON SearchIndex'{..}
          = object
              (catMaybes
                 [("queryVersion" .=) <$> _siQueryVersion,
                  ("nextToken" .=) <$> _siNextToken,
                  ("maxResults" .=) <$> _siMaxResults,
                  ("indexName" .=) <$> _siIndexName,
                  Just ("queryString" .= _siQueryString)])

instance ToPath SearchIndex where
        toPath = const "/indices/search"

instance ToQuery SearchIndex where
        toQuery = const mempty

-- | /See:/ 'searchIndexResponse' smart constructor.
data SearchIndexResponse = SearchIndexResponse'
  { _sirsNextToken      :: !(Maybe Text)
  , _sirsThings         :: !(Maybe [ThingDocument])
  , _sirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SearchIndexResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sirsNextToken' - The token used to get the next set of results, or __null__ if there are no additional results.
--
-- * 'sirsThings' - The things that match the search query.
--
-- * 'sirsResponseStatus' - -- | The response status code.
searchIndexResponse
    :: Int -- ^ 'sirsResponseStatus'
    -> SearchIndexResponse
searchIndexResponse pResponseStatus_ =
  SearchIndexResponse'
    { _sirsNextToken = Nothing
    , _sirsThings = Nothing
    , _sirsResponseStatus = pResponseStatus_
    }


-- | The token used to get the next set of results, or __null__ if there are no additional results.
sirsNextToken :: Lens' SearchIndexResponse (Maybe Text)
sirsNextToken = lens _sirsNextToken (\ s a -> s{_sirsNextToken = a})

-- | The things that match the search query.
sirsThings :: Lens' SearchIndexResponse [ThingDocument]
sirsThings = lens _sirsThings (\ s a -> s{_sirsThings = a}) . _Default . _Coerce

-- | -- | The response status code.
sirsResponseStatus :: Lens' SearchIndexResponse Int
sirsResponseStatus = lens _sirsResponseStatus (\ s a -> s{_sirsResponseStatus = a})

instance NFData SearchIndexResponse where
