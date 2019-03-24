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
-- Module      : Network.AWS.Greengrass.ListDeployments
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a history of deployments for the group.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListDeployments
    (
    -- * Creating a Request
      listDeployments
    , ListDeployments
    -- * Request Lenses
    , ldsNextToken
    , ldsMaxResults
    , ldsGroupId

    -- * Destructuring the Response
    , listDeploymentsResponse
    , ListDeploymentsResponse
    -- * Response Lenses
    , ldrsNextToken
    , ldrsDeployments
    , ldrsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listDeployments' smart constructor.
data ListDeployments = ListDeployments'
  { _ldsNextToken  :: !(Maybe Text)
  , _ldsMaxResults :: !(Maybe Text)
  , _ldsGroupId    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDeployments' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldsNextToken' - The token for the next set of results, or ''null'' if there are no additional results.
--
-- * 'ldsMaxResults' - The maximum number of results to be returned per request.
--
-- * 'ldsGroupId' - The ID of the Greengrass group.
listDeployments
    :: Text -- ^ 'ldsGroupId'
    -> ListDeployments
listDeployments pGroupId_ =
  ListDeployments'
    {_ldsNextToken = Nothing, _ldsMaxResults = Nothing, _ldsGroupId = pGroupId_}


-- | The token for the next set of results, or ''null'' if there are no additional results.
ldsNextToken :: Lens' ListDeployments (Maybe Text)
ldsNextToken = lens _ldsNextToken (\ s a -> s{_ldsNextToken = a})

-- | The maximum number of results to be returned per request.
ldsMaxResults :: Lens' ListDeployments (Maybe Text)
ldsMaxResults = lens _ldsMaxResults (\ s a -> s{_ldsMaxResults = a})

-- | The ID of the Greengrass group.
ldsGroupId :: Lens' ListDeployments Text
ldsGroupId = lens _ldsGroupId (\ s a -> s{_ldsGroupId = a})

instance AWSPager ListDeployments where
        page rq rs
          | stop (rs ^. ldrsNextToken) = Nothing
          | stop (rs ^. ldrsDeployments) = Nothing
          | otherwise =
            Just $ rq & ldsNextToken .~ rs ^. ldrsNextToken

instance AWSRequest ListDeployments where
        type Rs ListDeployments = ListDeploymentsResponse
        request = get greengrass
        response
          = receiveJSON
              (\ s h x ->
                 ListDeploymentsResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "Deployments" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListDeployments where

instance NFData ListDeployments where

instance ToHeaders ListDeployments where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListDeployments where
        toPath ListDeployments'{..}
          = mconcat
              ["/greengrass/groups/", toBS _ldsGroupId,
               "/deployments"]

instance ToQuery ListDeployments where
        toQuery ListDeployments'{..}
          = mconcat
              ["NextToken" =: _ldsNextToken,
               "MaxResults" =: _ldsMaxResults]

-- | /See:/ 'listDeploymentsResponse' smart constructor.
data ListDeploymentsResponse = ListDeploymentsResponse'
  { _ldrsNextToken      :: !(Maybe Text)
  , _ldrsDeployments    :: !(Maybe [Deployment])
  , _ldrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDeploymentsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldrsNextToken' - The token for the next set of results, or ''null'' if there are no additional results.
--
-- * 'ldrsDeployments' - A list of deployments for the requested groups.
--
-- * 'ldrsResponseStatus' - -- | The response status code.
listDeploymentsResponse
    :: Int -- ^ 'ldrsResponseStatus'
    -> ListDeploymentsResponse
listDeploymentsResponse pResponseStatus_ =
  ListDeploymentsResponse'
    { _ldrsNextToken = Nothing
    , _ldrsDeployments = Nothing
    , _ldrsResponseStatus = pResponseStatus_
    }


-- | The token for the next set of results, or ''null'' if there are no additional results.
ldrsNextToken :: Lens' ListDeploymentsResponse (Maybe Text)
ldrsNextToken = lens _ldrsNextToken (\ s a -> s{_ldrsNextToken = a})

-- | A list of deployments for the requested groups.
ldrsDeployments :: Lens' ListDeploymentsResponse [Deployment]
ldrsDeployments = lens _ldrsDeployments (\ s a -> s{_ldrsDeployments = a}) . _Default . _Coerce

-- | -- | The response status code.
ldrsResponseStatus :: Lens' ListDeploymentsResponse Int
ldrsResponseStatus = lens _ldrsResponseStatus (\ s a -> s{_ldrsResponseStatus = a})

instance NFData ListDeploymentsResponse where
