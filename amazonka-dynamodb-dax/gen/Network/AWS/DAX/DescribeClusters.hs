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
-- Module      : Network.AWS.DAX.DescribeClusters
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all provisioned DAX clusters if no cluster identifier is specified, or about a specific DAX cluster if a cluster identifier is supplied.
--
--
-- If the cluster is in the CREATING state, only cluster level information will be displayed until all of the nodes are successfully provisioned.
--
-- If the cluster is in the DELETING state, only cluster level information will be displayed.
--
-- If nodes are currently being added to the DAX cluster, node endpoint information and creation time for the additional nodes will not be displayed until they are completely provisioned. When the DAX cluster state is /available/ , the cluster is ready for use.
--
-- If nodes are currently being removed from the DAX cluster, no endpoint information for the removed nodes is displayed.
--
module Network.AWS.DAX.DescribeClusters
    (
    -- * Creating a Request
      describeClusters
    , DescribeClusters
    -- * Request Lenses
    , dcClusterNames
    , dcNextToken
    , dcMaxResults

    -- * Destructuring the Response
    , describeClustersResponse
    , DescribeClustersResponse
    -- * Response Lenses
    , dcrsNextToken
    , dcrsClusters
    , dcrsResponseStatus
    ) where

import Network.AWS.DAX.Types
import Network.AWS.DAX.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeClusters' smart constructor.
data DescribeClusters = DescribeClusters'
  { _dcClusterNames :: !(Maybe [Text])
  , _dcNextToken    :: !(Maybe Text)
  , _dcMaxResults   :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeClusters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcClusterNames' - The names of the DAX clusters being described.
--
-- * 'dcNextToken' - An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
--
-- * 'dcMaxResults' - The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved. The value for @MaxResults@ must be between 20 and 100.
describeClusters
    :: DescribeClusters
describeClusters =
  DescribeClusters'
    {_dcClusterNames = Nothing, _dcNextToken = Nothing, _dcMaxResults = Nothing}


-- | The names of the DAX clusters being described.
dcClusterNames :: Lens' DescribeClusters [Text]
dcClusterNames = lens _dcClusterNames (\ s a -> s{_dcClusterNames = a}) . _Default . _Coerce

-- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
dcNextToken :: Lens' DescribeClusters (Maybe Text)
dcNextToken = lens _dcNextToken (\ s a -> s{_dcNextToken = a})

-- | The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved. The value for @MaxResults@ must be between 20 and 100.
dcMaxResults :: Lens' DescribeClusters (Maybe Int)
dcMaxResults = lens _dcMaxResults (\ s a -> s{_dcMaxResults = a})

instance AWSRequest DescribeClusters where
        type Rs DescribeClusters = DescribeClustersResponse
        request = postJSON dax
        response
          = receiveJSON
              (\ s h x ->
                 DescribeClustersResponse' <$>
                   (x .?> "NextToken") <*> (x .?> "Clusters" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeClusters where

instance NFData DescribeClusters where

instance ToHeaders DescribeClusters where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonDAXV3.DescribeClusters" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeClusters where
        toJSON DescribeClusters'{..}
          = object
              (catMaybes
                 [("ClusterNames" .=) <$> _dcClusterNames,
                  ("NextToken" .=) <$> _dcNextToken,
                  ("MaxResults" .=) <$> _dcMaxResults])

instance ToPath DescribeClusters where
        toPath = const "/"

instance ToQuery DescribeClusters where
        toQuery = const mempty

-- | /See:/ 'describeClustersResponse' smart constructor.
data DescribeClustersResponse = DescribeClustersResponse'
  { _dcrsNextToken      :: !(Maybe Text)
  , _dcrsClusters       :: !(Maybe [Cluster])
  , _dcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeClustersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcrsNextToken' - Provides an identifier to allow retrieval of paginated results.
--
-- * 'dcrsClusters' - The descriptions of your DAX clusters, in response to a /DescribeClusters/ request.
--
-- * 'dcrsResponseStatus' - -- | The response status code.
describeClustersResponse
    :: Int -- ^ 'dcrsResponseStatus'
    -> DescribeClustersResponse
describeClustersResponse pResponseStatus_ =
  DescribeClustersResponse'
    { _dcrsNextToken = Nothing
    , _dcrsClusters = Nothing
    , _dcrsResponseStatus = pResponseStatus_
    }


-- | Provides an identifier to allow retrieval of paginated results.
dcrsNextToken :: Lens' DescribeClustersResponse (Maybe Text)
dcrsNextToken = lens _dcrsNextToken (\ s a -> s{_dcrsNextToken = a})

-- | The descriptions of your DAX clusters, in response to a /DescribeClusters/ request.
dcrsClusters :: Lens' DescribeClustersResponse [Cluster]
dcrsClusters = lens _dcrsClusters (\ s a -> s{_dcrsClusters = a}) . _Default . _Coerce

-- | -- | The response status code.
dcrsResponseStatus :: Lens' DescribeClustersResponse Int
dcrsResponseStatus = lens _dcrsResponseStatus (\ s a -> s{_dcrsResponseStatus = a})

instance NFData DescribeClustersResponse where
