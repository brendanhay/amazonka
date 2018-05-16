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
module Network.AWS.Greengrass.ListDeployments
    (
    -- * Creating a Request
      listDeployments
    , ListDeployments
    -- * Request Lenses
    , lNextToken
    , lMaxResults
    , lGroupId

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
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listDeployments' smart constructor.
data ListDeployments = ListDeployments'
  { _lNextToken  :: !(Maybe Text)
  , _lMaxResults :: !(Maybe Text)
  , _lGroupId    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDeployments' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lNextToken' - The token for the next set of results, or ''null'' if there are no additional results.
--
-- * 'lMaxResults' - The maximum number of results to be returned per request.
--
-- * 'lGroupId' - The ID of the AWS Greengrass group.
listDeployments
    :: Text -- ^ 'lGroupId'
    -> ListDeployments
listDeployments pGroupId_ =
  ListDeployments'
    {_lNextToken = Nothing, _lMaxResults = Nothing, _lGroupId = pGroupId_}


-- | The token for the next set of results, or ''null'' if there are no additional results.
lNextToken :: Lens' ListDeployments (Maybe Text)
lNextToken = lens _lNextToken (\ s a -> s{_lNextToken = a})

-- | The maximum number of results to be returned per request.
lMaxResults :: Lens' ListDeployments (Maybe Text)
lMaxResults = lens _lMaxResults (\ s a -> s{_lMaxResults = a})

-- | The ID of the AWS Greengrass group.
lGroupId :: Lens' ListDeployments Text
lGroupId = lens _lGroupId (\ s a -> s{_lGroupId = a})

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
              ["/greengrass/groups/", toBS _lGroupId,
               "/deployments"]

instance ToQuery ListDeployments where
        toQuery ListDeployments'{..}
          = mconcat
              ["NextToken" =: _lNextToken,
               "MaxResults" =: _lMaxResults]

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
