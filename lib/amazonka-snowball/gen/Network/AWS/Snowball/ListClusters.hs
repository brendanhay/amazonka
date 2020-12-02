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
-- Module      : Network.AWS.Snowball.ListClusters
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of @ClusterListEntry@ objects of the specified length. Each @ClusterListEntry@ object contains a cluster's state, a cluster's ID, and other important status information.
--
--
module Network.AWS.Snowball.ListClusters
    (
    -- * Creating a Request
      listClusters
    , ListClusters
    -- * Request Lenses
    , lcNextToken
    , lcMaxResults

    -- * Destructuring the Response
    , listClustersResponse
    , ListClustersResponse
    -- * Response Lenses
    , lcrsClusterListEntries
    , lcrsNextToken
    , lcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Snowball.Types
import Network.AWS.Snowball.Types.Product

-- | /See:/ 'listClusters' smart constructor.
data ListClusters = ListClusters'
  { _lcNextToken  :: !(Maybe Text)
  , _lcMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListClusters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcNextToken' - HTTP requests are stateless. To identify what object comes "next" in the list of @ClusterListEntry@ objects, you have the option of specifying @NextToken@ as the starting point for your returned list.
--
-- * 'lcMaxResults' - The number of @ClusterListEntry@ objects to return.
listClusters
    :: ListClusters
listClusters = ListClusters' {_lcNextToken = Nothing, _lcMaxResults = Nothing}


-- | HTTP requests are stateless. To identify what object comes "next" in the list of @ClusterListEntry@ objects, you have the option of specifying @NextToken@ as the starting point for your returned list.
lcNextToken :: Lens' ListClusters (Maybe Text)
lcNextToken = lens _lcNextToken (\ s a -> s{_lcNextToken = a})

-- | The number of @ClusterListEntry@ objects to return.
lcMaxResults :: Lens' ListClusters (Maybe Natural)
lcMaxResults = lens _lcMaxResults (\ s a -> s{_lcMaxResults = a}) . mapping _Nat

instance AWSRequest ListClusters where
        type Rs ListClusters = ListClustersResponse
        request = postJSON snowball
        response
          = receiveJSON
              (\ s h x ->
                 ListClustersResponse' <$>
                   (x .?> "ClusterListEntries" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListClusters where

instance NFData ListClusters where

instance ToHeaders ListClusters where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSIESnowballJobManagementService.ListClusters" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListClusters where
        toJSON ListClusters'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lcNextToken,
                  ("MaxResults" .=) <$> _lcMaxResults])

instance ToPath ListClusters where
        toPath = const "/"

instance ToQuery ListClusters where
        toQuery = const mempty

-- | /See:/ 'listClustersResponse' smart constructor.
data ListClustersResponse = ListClustersResponse'
  { _lcrsClusterListEntries :: !(Maybe [ClusterListEntry])
  , _lcrsNextToken          :: !(Maybe Text)
  , _lcrsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListClustersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcrsClusterListEntries' - Each @ClusterListEntry@ object contains a cluster's state, a cluster's ID, and other important status information.
--
-- * 'lcrsNextToken' - HTTP requests are stateless. If you use the automatically generated @NextToken@ value in your next @ClusterListEntry@ call, your list of returned clusters will start from this point in the array.
--
-- * 'lcrsResponseStatus' - -- | The response status code.
listClustersResponse
    :: Int -- ^ 'lcrsResponseStatus'
    -> ListClustersResponse
listClustersResponse pResponseStatus_ =
  ListClustersResponse'
    { _lcrsClusterListEntries = Nothing
    , _lcrsNextToken = Nothing
    , _lcrsResponseStatus = pResponseStatus_
    }


-- | Each @ClusterListEntry@ object contains a cluster's state, a cluster's ID, and other important status information.
lcrsClusterListEntries :: Lens' ListClustersResponse [ClusterListEntry]
lcrsClusterListEntries = lens _lcrsClusterListEntries (\ s a -> s{_lcrsClusterListEntries = a}) . _Default . _Coerce

-- | HTTP requests are stateless. If you use the automatically generated @NextToken@ value in your next @ClusterListEntry@ call, your list of returned clusters will start from this point in the array.
lcrsNextToken :: Lens' ListClustersResponse (Maybe Text)
lcrsNextToken = lens _lcrsNextToken (\ s a -> s{_lcrsNextToken = a})

-- | -- | The response status code.
lcrsResponseStatus :: Lens' ListClustersResponse Int
lcrsResponseStatus = lens _lcrsResponseStatus (\ s a -> s{_lcrsResponseStatus = a})

instance NFData ListClustersResponse where
