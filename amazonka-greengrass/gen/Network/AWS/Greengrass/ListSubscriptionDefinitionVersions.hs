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
-- Module      : Network.AWS.Greengrass.ListSubscriptionDefinitionVersions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of a subscription definition.
module Network.AWS.Greengrass.ListSubscriptionDefinitionVersions
    (
    -- * Creating a Request
      listSubscriptionDefinitionVersions
    , ListSubscriptionDefinitionVersions
    -- * Request Lenses
    , lsdvNextToken
    , lsdvMaxResults
    , lsdvSubscriptionDefinitionId

    -- * Destructuring the Response
    , listSubscriptionDefinitionVersionsResponse
    , ListSubscriptionDefinitionVersionsResponse
    -- * Response Lenses
    , lsdvrsVersions
    , lsdvrsNextToken
    , lsdvrsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listSubscriptionDefinitionVersions' smart constructor.
data ListSubscriptionDefinitionVersions = ListSubscriptionDefinitionVersions'
  { _lsdvNextToken                :: !(Maybe Text)
  , _lsdvMaxResults               :: !(Maybe Text)
  , _lsdvSubscriptionDefinitionId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListSubscriptionDefinitionVersions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsdvNextToken' - The token for the next set of results, or ''null'' if there are no additional results.
--
-- * 'lsdvMaxResults' - The maximum number of results to be returned per request.
--
-- * 'lsdvSubscriptionDefinitionId' - The ID of the subscription definition.
listSubscriptionDefinitionVersions
    :: Text -- ^ 'lsdvSubscriptionDefinitionId'
    -> ListSubscriptionDefinitionVersions
listSubscriptionDefinitionVersions pSubscriptionDefinitionId_ =
  ListSubscriptionDefinitionVersions'
    { _lsdvNextToken = Nothing
    , _lsdvMaxResults = Nothing
    , _lsdvSubscriptionDefinitionId = pSubscriptionDefinitionId_
    }


-- | The token for the next set of results, or ''null'' if there are no additional results.
lsdvNextToken :: Lens' ListSubscriptionDefinitionVersions (Maybe Text)
lsdvNextToken = lens _lsdvNextToken (\ s a -> s{_lsdvNextToken = a})

-- | The maximum number of results to be returned per request.
lsdvMaxResults :: Lens' ListSubscriptionDefinitionVersions (Maybe Text)
lsdvMaxResults = lens _lsdvMaxResults (\ s a -> s{_lsdvMaxResults = a})

-- | The ID of the subscription definition.
lsdvSubscriptionDefinitionId :: Lens' ListSubscriptionDefinitionVersions Text
lsdvSubscriptionDefinitionId = lens _lsdvSubscriptionDefinitionId (\ s a -> s{_lsdvSubscriptionDefinitionId = a})

instance AWSRequest
           ListSubscriptionDefinitionVersions
         where
        type Rs ListSubscriptionDefinitionVersions =
             ListSubscriptionDefinitionVersionsResponse
        request = get greengrass
        response
          = receiveJSON
              (\ s h x ->
                 ListSubscriptionDefinitionVersionsResponse' <$>
                   (x .?> "Versions" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListSubscriptionDefinitionVersions
         where

instance NFData ListSubscriptionDefinitionVersions
         where

instance ToHeaders ListSubscriptionDefinitionVersions
         where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListSubscriptionDefinitionVersions
         where
        toPath ListSubscriptionDefinitionVersions'{..}
          = mconcat
              ["/greengrass/definition/subscriptions/",
               toBS _lsdvSubscriptionDefinitionId, "/versions"]

instance ToQuery ListSubscriptionDefinitionVersions
         where
        toQuery ListSubscriptionDefinitionVersions'{..}
          = mconcat
              ["NextToken" =: _lsdvNextToken,
               "MaxResults" =: _lsdvMaxResults]

-- | /See:/ 'listSubscriptionDefinitionVersionsResponse' smart constructor.
data ListSubscriptionDefinitionVersionsResponse = ListSubscriptionDefinitionVersionsResponse'
  { _lsdvrsVersions       :: !(Maybe [VersionInformation])
  , _lsdvrsNextToken      :: !(Maybe Text)
  , _lsdvrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListSubscriptionDefinitionVersionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsdvrsVersions' - Information about a version.
--
-- * 'lsdvrsNextToken' - The token for the next set of results, or ''null'' if there are no additional results.
--
-- * 'lsdvrsResponseStatus' - -- | The response status code.
listSubscriptionDefinitionVersionsResponse
    :: Int -- ^ 'lsdvrsResponseStatus'
    -> ListSubscriptionDefinitionVersionsResponse
listSubscriptionDefinitionVersionsResponse pResponseStatus_ =
  ListSubscriptionDefinitionVersionsResponse'
    { _lsdvrsVersions = Nothing
    , _lsdvrsNextToken = Nothing
    , _lsdvrsResponseStatus = pResponseStatus_
    }


-- | Information about a version.
lsdvrsVersions :: Lens' ListSubscriptionDefinitionVersionsResponse [VersionInformation]
lsdvrsVersions = lens _lsdvrsVersions (\ s a -> s{_lsdvrsVersions = a}) . _Default . _Coerce

-- | The token for the next set of results, or ''null'' if there are no additional results.
lsdvrsNextToken :: Lens' ListSubscriptionDefinitionVersionsResponse (Maybe Text)
lsdvrsNextToken = lens _lsdvrsNextToken (\ s a -> s{_lsdvrsNextToken = a})

-- | -- | The response status code.
lsdvrsResponseStatus :: Lens' ListSubscriptionDefinitionVersionsResponse Int
lsdvrsResponseStatus = lens _lsdvrsResponseStatus (\ s a -> s{_lsdvrsResponseStatus = a})

instance NFData
           ListSubscriptionDefinitionVersionsResponse
         where
