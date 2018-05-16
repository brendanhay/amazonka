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
-- Module      : Network.AWS.ElasticSearch.DeleteElasticsearchDomain
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes the specified Elasticsearch domain and all of its data. Once a domain is deleted, it cannot be recovered.
--
--
module Network.AWS.ElasticSearch.DeleteElasticsearchDomain
    (
    -- * Creating a Request
      deleteElasticsearchDomain
    , DeleteElasticsearchDomain
    -- * Request Lenses
    , dDomainName

    -- * Destructuring the Response
    , deleteElasticsearchDomainResponse
    , DeleteElasticsearchDomainResponse
    -- * Response Lenses
    , delrsDomainStatus
    , delrsResponseStatus
    ) where

import Network.AWS.ElasticSearch.Types
import Network.AWS.ElasticSearch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the @'DeleteElasticsearchDomain' @ operation. Specifies the name of the Elasticsearch domain that you want to delete.
--
--
--
-- /See:/ 'deleteElasticsearchDomain' smart constructor.
newtype DeleteElasticsearchDomain = DeleteElasticsearchDomain'
  { _dDomainName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteElasticsearchDomain' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dDomainName' - The name of the Elasticsearch domain that you want to permanently delete.
deleteElasticsearchDomain
    :: Text -- ^ 'dDomainName'
    -> DeleteElasticsearchDomain
deleteElasticsearchDomain pDomainName_ =
  DeleteElasticsearchDomain' {_dDomainName = pDomainName_}


-- | The name of the Elasticsearch domain that you want to permanently delete.
dDomainName :: Lens' DeleteElasticsearchDomain Text
dDomainName = lens _dDomainName (\ s a -> s{_dDomainName = a})

instance AWSRequest DeleteElasticsearchDomain where
        type Rs DeleteElasticsearchDomain =
             DeleteElasticsearchDomainResponse
        request = delete elasticSearch
        response
          = receiveJSON
              (\ s h x ->
                 DeleteElasticsearchDomainResponse' <$>
                   (x .?> "DomainStatus") <*> (pure (fromEnum s)))

instance Hashable DeleteElasticsearchDomain where

instance NFData DeleteElasticsearchDomain where

instance ToHeaders DeleteElasticsearchDomain where
        toHeaders = const mempty

instance ToPath DeleteElasticsearchDomain where
        toPath DeleteElasticsearchDomain'{..}
          = mconcat
              ["/2015-01-01/es/domain/", toBS _dDomainName]

instance ToQuery DeleteElasticsearchDomain where
        toQuery = const mempty

-- | The result of a @DeleteElasticsearchDomain@ request. Contains the status of the pending deletion, or no status if the domain and all of its resources have been deleted.
--
--
--
-- /See:/ 'deleteElasticsearchDomainResponse' smart constructor.
data DeleteElasticsearchDomainResponse = DeleteElasticsearchDomainResponse'
  { _delrsDomainStatus   :: !(Maybe ElasticsearchDomainStatus)
  , _delrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteElasticsearchDomainResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delrsDomainStatus' - The status of the Elasticsearch domain being deleted.
--
-- * 'delrsResponseStatus' - -- | The response status code.
deleteElasticsearchDomainResponse
    :: Int -- ^ 'delrsResponseStatus'
    -> DeleteElasticsearchDomainResponse
deleteElasticsearchDomainResponse pResponseStatus_ =
  DeleteElasticsearchDomainResponse'
    {_delrsDomainStatus = Nothing, _delrsResponseStatus = pResponseStatus_}


-- | The status of the Elasticsearch domain being deleted.
delrsDomainStatus :: Lens' DeleteElasticsearchDomainResponse (Maybe ElasticsearchDomainStatus)
delrsDomainStatus = lens _delrsDomainStatus (\ s a -> s{_delrsDomainStatus = a})

-- | -- | The response status code.
delrsResponseStatus :: Lens' DeleteElasticsearchDomainResponse Int
delrsResponseStatus = lens _delrsResponseStatus (\ s a -> s{_delrsResponseStatus = a})

instance NFData DeleteElasticsearchDomainResponse
         where
