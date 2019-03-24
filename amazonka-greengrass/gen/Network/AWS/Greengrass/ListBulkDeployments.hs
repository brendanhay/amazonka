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
-- Module      : Network.AWS.Greengrass.ListBulkDeployments
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of bulk deployments.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListBulkDeployments
    (
    -- * Creating a Request
      listBulkDeployments
    , ListBulkDeployments
    -- * Request Lenses
    , lbdNextToken
    , lbdMaxResults

    -- * Destructuring the Response
    , listBulkDeploymentsResponse
    , ListBulkDeploymentsResponse
    -- * Response Lenses
    , lbdrsNextToken
    , lbdrsBulkDeployments
    , lbdrsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listBulkDeployments' smart constructor.
data ListBulkDeployments = ListBulkDeployments'
  { _lbdNextToken  :: !(Maybe Text)
  , _lbdMaxResults :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListBulkDeployments' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbdNextToken' - The token for the next set of results, or ''null'' if there are no additional results.
--
-- * 'lbdMaxResults' - The maximum number of results to be returned per request.
listBulkDeployments
    :: ListBulkDeployments
listBulkDeployments =
  ListBulkDeployments' {_lbdNextToken = Nothing, _lbdMaxResults = Nothing}


-- | The token for the next set of results, or ''null'' if there are no additional results.
lbdNextToken :: Lens' ListBulkDeployments (Maybe Text)
lbdNextToken = lens _lbdNextToken (\ s a -> s{_lbdNextToken = a})

-- | The maximum number of results to be returned per request.
lbdMaxResults :: Lens' ListBulkDeployments (Maybe Text)
lbdMaxResults = lens _lbdMaxResults (\ s a -> s{_lbdMaxResults = a})

instance AWSPager ListBulkDeployments where
        page rq rs
          | stop (rs ^. lbdrsNextToken) = Nothing
          | stop (rs ^. lbdrsBulkDeployments) = Nothing
          | otherwise =
            Just $ rq & lbdNextToken .~ rs ^. lbdrsNextToken

instance AWSRequest ListBulkDeployments where
        type Rs ListBulkDeployments =
             ListBulkDeploymentsResponse
        request = get greengrass
        response
          = receiveJSON
              (\ s h x ->
                 ListBulkDeploymentsResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "BulkDeployments" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListBulkDeployments where

instance NFData ListBulkDeployments where

instance ToHeaders ListBulkDeployments where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListBulkDeployments where
        toPath = const "/greengrass/bulk/deployments"

instance ToQuery ListBulkDeployments where
        toQuery ListBulkDeployments'{..}
          = mconcat
              ["NextToken" =: _lbdNextToken,
               "MaxResults" =: _lbdMaxResults]

-- | /See:/ 'listBulkDeploymentsResponse' smart constructor.
data ListBulkDeploymentsResponse = ListBulkDeploymentsResponse'
  { _lbdrsNextToken       :: !(Maybe Text)
  , _lbdrsBulkDeployments :: !(Maybe [BulkDeployment])
  , _lbdrsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListBulkDeploymentsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbdrsNextToken' - The token for the next set of results, or ''null'' if there are no additional results.
--
-- * 'lbdrsBulkDeployments' - A list of bulk deployments.
--
-- * 'lbdrsResponseStatus' - -- | The response status code.
listBulkDeploymentsResponse
    :: Int -- ^ 'lbdrsResponseStatus'
    -> ListBulkDeploymentsResponse
listBulkDeploymentsResponse pResponseStatus_ =
  ListBulkDeploymentsResponse'
    { _lbdrsNextToken = Nothing
    , _lbdrsBulkDeployments = Nothing
    , _lbdrsResponseStatus = pResponseStatus_
    }


-- | The token for the next set of results, or ''null'' if there are no additional results.
lbdrsNextToken :: Lens' ListBulkDeploymentsResponse (Maybe Text)
lbdrsNextToken = lens _lbdrsNextToken (\ s a -> s{_lbdrsNextToken = a})

-- | A list of bulk deployments.
lbdrsBulkDeployments :: Lens' ListBulkDeploymentsResponse [BulkDeployment]
lbdrsBulkDeployments = lens _lbdrsBulkDeployments (\ s a -> s{_lbdrsBulkDeployments = a}) . _Default . _Coerce

-- | -- | The response status code.
lbdrsResponseStatus :: Lens' ListBulkDeploymentsResponse Int
lbdrsResponseStatus = lens _lbdrsResponseStatus (\ s a -> s{_lbdrsResponseStatus = a})

instance NFData ListBulkDeploymentsResponse where
