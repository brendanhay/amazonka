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
-- Module      : Network.AWS.Greengrass.ListConnectorDefinitionVersions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of a connector definition, which are containers for connectors. Connectors run on the Greengrass core and contain built-in integration with local infrastructure, device protocols, AWS, and other cloud services.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListConnectorDefinitionVersions
    (
    -- * Creating a Request
      listConnectorDefinitionVersions
    , ListConnectorDefinitionVersions
    -- * Request Lenses
    , lcdvNextToken
    , lcdvMaxResults
    , lcdvConnectorDefinitionId

    -- * Destructuring the Response
    , listConnectorDefinitionVersionsResponse
    , ListConnectorDefinitionVersionsResponse
    -- * Response Lenses
    , lcdvsrsVersions
    , lcdvsrsNextToken
    , lcdvsrsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listConnectorDefinitionVersions' smart constructor.
data ListConnectorDefinitionVersions = ListConnectorDefinitionVersions'
  { _lcdvNextToken             :: !(Maybe Text)
  , _lcdvMaxResults            :: !(Maybe Text)
  , _lcdvConnectorDefinitionId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListConnectorDefinitionVersions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcdvNextToken' - The token for the next set of results, or ''null'' if there are no additional results.
--
-- * 'lcdvMaxResults' - The maximum number of results to be returned per request.
--
-- * 'lcdvConnectorDefinitionId' - The ID of the connector definition.
listConnectorDefinitionVersions
    :: Text -- ^ 'lcdvConnectorDefinitionId'
    -> ListConnectorDefinitionVersions
listConnectorDefinitionVersions pConnectorDefinitionId_ =
  ListConnectorDefinitionVersions'
    { _lcdvNextToken = Nothing
    , _lcdvMaxResults = Nothing
    , _lcdvConnectorDefinitionId = pConnectorDefinitionId_
    }


-- | The token for the next set of results, or ''null'' if there are no additional results.
lcdvNextToken :: Lens' ListConnectorDefinitionVersions (Maybe Text)
lcdvNextToken = lens _lcdvNextToken (\ s a -> s{_lcdvNextToken = a})

-- | The maximum number of results to be returned per request.
lcdvMaxResults :: Lens' ListConnectorDefinitionVersions (Maybe Text)
lcdvMaxResults = lens _lcdvMaxResults (\ s a -> s{_lcdvMaxResults = a})

-- | The ID of the connector definition.
lcdvConnectorDefinitionId :: Lens' ListConnectorDefinitionVersions Text
lcdvConnectorDefinitionId = lens _lcdvConnectorDefinitionId (\ s a -> s{_lcdvConnectorDefinitionId = a})

instance AWSPager ListConnectorDefinitionVersions
         where
        page rq rs
          | stop (rs ^. lcdvsrsNextToken) = Nothing
          | stop (rs ^. lcdvsrsVersions) = Nothing
          | otherwise =
            Just $ rq & lcdvNextToken .~ rs ^. lcdvsrsNextToken

instance AWSRequest ListConnectorDefinitionVersions
         where
        type Rs ListConnectorDefinitionVersions =
             ListConnectorDefinitionVersionsResponse
        request = get greengrass
        response
          = receiveJSON
              (\ s h x ->
                 ListConnectorDefinitionVersionsResponse' <$>
                   (x .?> "Versions" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListConnectorDefinitionVersions
         where

instance NFData ListConnectorDefinitionVersions where

instance ToHeaders ListConnectorDefinitionVersions
         where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListConnectorDefinitionVersions where
        toPath ListConnectorDefinitionVersions'{..}
          = mconcat
              ["/greengrass/definition/connectors/",
               toBS _lcdvConnectorDefinitionId, "/versions"]

instance ToQuery ListConnectorDefinitionVersions
         where
        toQuery ListConnectorDefinitionVersions'{..}
          = mconcat
              ["NextToken" =: _lcdvNextToken,
               "MaxResults" =: _lcdvMaxResults]

-- | /See:/ 'listConnectorDefinitionVersionsResponse' smart constructor.
data ListConnectorDefinitionVersionsResponse = ListConnectorDefinitionVersionsResponse'
  { _lcdvsrsVersions       :: !(Maybe [VersionInformation])
  , _lcdvsrsNextToken      :: !(Maybe Text)
  , _lcdvsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListConnectorDefinitionVersionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcdvsrsVersions' - Information about a version.
--
-- * 'lcdvsrsNextToken' - The token for the next set of results, or ''null'' if there are no additional results.
--
-- * 'lcdvsrsResponseStatus' - -- | The response status code.
listConnectorDefinitionVersionsResponse
    :: Int -- ^ 'lcdvsrsResponseStatus'
    -> ListConnectorDefinitionVersionsResponse
listConnectorDefinitionVersionsResponse pResponseStatus_ =
  ListConnectorDefinitionVersionsResponse'
    { _lcdvsrsVersions = Nothing
    , _lcdvsrsNextToken = Nothing
    , _lcdvsrsResponseStatus = pResponseStatus_
    }


-- | Information about a version.
lcdvsrsVersions :: Lens' ListConnectorDefinitionVersionsResponse [VersionInformation]
lcdvsrsVersions = lens _lcdvsrsVersions (\ s a -> s{_lcdvsrsVersions = a}) . _Default . _Coerce

-- | The token for the next set of results, or ''null'' if there are no additional results.
lcdvsrsNextToken :: Lens' ListConnectorDefinitionVersionsResponse (Maybe Text)
lcdvsrsNextToken = lens _lcdvsrsNextToken (\ s a -> s{_lcdvsrsNextToken = a})

-- | -- | The response status code.
lcdvsrsResponseStatus :: Lens' ListConnectorDefinitionVersionsResponse Int
lcdvsrsResponseStatus = lens _lcdvsrsResponseStatus (\ s a -> s{_lcdvsrsResponseStatus = a})

instance NFData
           ListConnectorDefinitionVersionsResponse
         where
