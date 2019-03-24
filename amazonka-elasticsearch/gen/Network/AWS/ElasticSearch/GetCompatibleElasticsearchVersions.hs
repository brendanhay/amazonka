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
-- Module      : Network.AWS.ElasticSearch.GetCompatibleElasticsearchVersions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of upgrade compatible Elastisearch versions. You can optionally pass a @'DomainName' @ to get all upgrade compatible Elasticsearch versions for that specific domain.
--
--
module Network.AWS.ElasticSearch.GetCompatibleElasticsearchVersions
    (
    -- * Creating a Request
      getCompatibleElasticsearchVersions
    , GetCompatibleElasticsearchVersions
    -- * Request Lenses
    , gcevDomainName

    -- * Destructuring the Response
    , getCompatibleElasticsearchVersionsResponse
    , GetCompatibleElasticsearchVersionsResponse
    -- * Response Lenses
    , gcevrsCompatibleElasticsearchVersions
    , gcevrsResponseStatus
    ) where

import Network.AWS.ElasticSearch.Types
import Network.AWS.ElasticSearch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for request parameters to @'GetCompatibleElasticsearchVersions' @ operation.
--
--
--
-- /See:/ 'getCompatibleElasticsearchVersions' smart constructor.
newtype GetCompatibleElasticsearchVersions = GetCompatibleElasticsearchVersions'
  { _gcevDomainName :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCompatibleElasticsearchVersions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcevDomainName' - Undocumented member.
getCompatibleElasticsearchVersions
    :: GetCompatibleElasticsearchVersions
getCompatibleElasticsearchVersions =
  GetCompatibleElasticsearchVersions' {_gcevDomainName = Nothing}


-- | Undocumented member.
gcevDomainName :: Lens' GetCompatibleElasticsearchVersions (Maybe Text)
gcevDomainName = lens _gcevDomainName (\ s a -> s{_gcevDomainName = a})

instance AWSRequest
           GetCompatibleElasticsearchVersions
         where
        type Rs GetCompatibleElasticsearchVersions =
             GetCompatibleElasticsearchVersionsResponse
        request = get elasticSearch
        response
          = receiveJSON
              (\ s h x ->
                 GetCompatibleElasticsearchVersionsResponse' <$>
                   (x .?> "CompatibleElasticsearchVersions" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetCompatibleElasticsearchVersions
         where

instance NFData GetCompatibleElasticsearchVersions
         where

instance ToHeaders GetCompatibleElasticsearchVersions
         where
        toHeaders = const mempty

instance ToPath GetCompatibleElasticsearchVersions
         where
        toPath = const "/2015-01-01/es/compatibleVersions"

instance ToQuery GetCompatibleElasticsearchVersions
         where
        toQuery GetCompatibleElasticsearchVersions'{..}
          = mconcat ["domainName" =: _gcevDomainName]

-- | Container for response returned by @'GetCompatibleElasticsearchVersions' @ operation.
--
--
--
-- /See:/ 'getCompatibleElasticsearchVersionsResponse' smart constructor.
data GetCompatibleElasticsearchVersionsResponse = GetCompatibleElasticsearchVersionsResponse'
  { _gcevrsCompatibleElasticsearchVersions :: !(Maybe [CompatibleVersionsMap])
  , _gcevrsResponseStatus                  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCompatibleElasticsearchVersionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcevrsCompatibleElasticsearchVersions' - A map of compatible Elasticsearch versions returned as part of the @'GetCompatibleElasticsearchVersions' @ operation.
--
-- * 'gcevrsResponseStatus' - -- | The response status code.
getCompatibleElasticsearchVersionsResponse
    :: Int -- ^ 'gcevrsResponseStatus'
    -> GetCompatibleElasticsearchVersionsResponse
getCompatibleElasticsearchVersionsResponse pResponseStatus_ =
  GetCompatibleElasticsearchVersionsResponse'
    { _gcevrsCompatibleElasticsearchVersions = Nothing
    , _gcevrsResponseStatus = pResponseStatus_
    }


-- | A map of compatible Elasticsearch versions returned as part of the @'GetCompatibleElasticsearchVersions' @ operation.
gcevrsCompatibleElasticsearchVersions :: Lens' GetCompatibleElasticsearchVersionsResponse [CompatibleVersionsMap]
gcevrsCompatibleElasticsearchVersions = lens _gcevrsCompatibleElasticsearchVersions (\ s a -> s{_gcevrsCompatibleElasticsearchVersions = a}) . _Default . _Coerce

-- | -- | The response status code.
gcevrsResponseStatus :: Lens' GetCompatibleElasticsearchVersionsResponse Int
gcevrsResponseStatus = lens _gcevrsResponseStatus (\ s a -> s{_gcevrsResponseStatus = a})

instance NFData
           GetCompatibleElasticsearchVersionsResponse
         where
