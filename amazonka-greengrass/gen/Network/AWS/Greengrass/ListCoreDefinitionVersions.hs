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
-- Module      : Network.AWS.Greengrass.ListCoreDefinitionVersions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of a core definition.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListCoreDefinitionVersions
    (
    -- * Creating a Request
      listCoreDefinitionVersions
    , ListCoreDefinitionVersions
    -- * Request Lenses
    , lcdvsNextToken
    , lcdvsMaxResults
    , lcdvsCoreDefinitionId

    -- * Destructuring the Response
    , listCoreDefinitionVersionsResponse
    , ListCoreDefinitionVersionsResponse
    -- * Response Lenses
    , lcdvrsVersions
    , lcdvrsNextToken
    , lcdvrsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listCoreDefinitionVersions' smart constructor.
data ListCoreDefinitionVersions = ListCoreDefinitionVersions'
  { _lcdvsNextToken        :: !(Maybe Text)
  , _lcdvsMaxResults       :: !(Maybe Text)
  , _lcdvsCoreDefinitionId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListCoreDefinitionVersions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcdvsNextToken' - The token for the next set of results, or ''null'' if there are no additional results.
--
-- * 'lcdvsMaxResults' - The maximum number of results to be returned per request.
--
-- * 'lcdvsCoreDefinitionId' - The ID of the core definition.
listCoreDefinitionVersions
    :: Text -- ^ 'lcdvsCoreDefinitionId'
    -> ListCoreDefinitionVersions
listCoreDefinitionVersions pCoreDefinitionId_ =
  ListCoreDefinitionVersions'
    { _lcdvsNextToken = Nothing
    , _lcdvsMaxResults = Nothing
    , _lcdvsCoreDefinitionId = pCoreDefinitionId_
    }


-- | The token for the next set of results, or ''null'' if there are no additional results.
lcdvsNextToken :: Lens' ListCoreDefinitionVersions (Maybe Text)
lcdvsNextToken = lens _lcdvsNextToken (\ s a -> s{_lcdvsNextToken = a})

-- | The maximum number of results to be returned per request.
lcdvsMaxResults :: Lens' ListCoreDefinitionVersions (Maybe Text)
lcdvsMaxResults = lens _lcdvsMaxResults (\ s a -> s{_lcdvsMaxResults = a})

-- | The ID of the core definition.
lcdvsCoreDefinitionId :: Lens' ListCoreDefinitionVersions Text
lcdvsCoreDefinitionId = lens _lcdvsCoreDefinitionId (\ s a -> s{_lcdvsCoreDefinitionId = a})

instance AWSPager ListCoreDefinitionVersions where
        page rq rs
          | stop (rs ^. lcdvrsNextToken) = Nothing
          | stop (rs ^. lcdvrsVersions) = Nothing
          | otherwise =
            Just $ rq & lcdvsNextToken .~ rs ^. lcdvrsNextToken

instance AWSRequest ListCoreDefinitionVersions where
        type Rs ListCoreDefinitionVersions =
             ListCoreDefinitionVersionsResponse
        request = get greengrass
        response
          = receiveJSON
              (\ s h x ->
                 ListCoreDefinitionVersionsResponse' <$>
                   (x .?> "Versions" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListCoreDefinitionVersions where

instance NFData ListCoreDefinitionVersions where

instance ToHeaders ListCoreDefinitionVersions where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListCoreDefinitionVersions where
        toPath ListCoreDefinitionVersions'{..}
          = mconcat
              ["/greengrass/definition/cores/",
               toBS _lcdvsCoreDefinitionId, "/versions"]

instance ToQuery ListCoreDefinitionVersions where
        toQuery ListCoreDefinitionVersions'{..}
          = mconcat
              ["NextToken" =: _lcdvsNextToken,
               "MaxResults" =: _lcdvsMaxResults]

-- | /See:/ 'listCoreDefinitionVersionsResponse' smart constructor.
data ListCoreDefinitionVersionsResponse = ListCoreDefinitionVersionsResponse'
  { _lcdvrsVersions       :: !(Maybe [VersionInformation])
  , _lcdvrsNextToken      :: !(Maybe Text)
  , _lcdvrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListCoreDefinitionVersionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcdvrsVersions' - Information about a version.
--
-- * 'lcdvrsNextToken' - The token for the next set of results, or ''null'' if there are no additional results.
--
-- * 'lcdvrsResponseStatus' - -- | The response status code.
listCoreDefinitionVersionsResponse
    :: Int -- ^ 'lcdvrsResponseStatus'
    -> ListCoreDefinitionVersionsResponse
listCoreDefinitionVersionsResponse pResponseStatus_ =
  ListCoreDefinitionVersionsResponse'
    { _lcdvrsVersions = Nothing
    , _lcdvrsNextToken = Nothing
    , _lcdvrsResponseStatus = pResponseStatus_
    }


-- | Information about a version.
lcdvrsVersions :: Lens' ListCoreDefinitionVersionsResponse [VersionInformation]
lcdvrsVersions = lens _lcdvrsVersions (\ s a -> s{_lcdvrsVersions = a}) . _Default . _Coerce

-- | The token for the next set of results, or ''null'' if there are no additional results.
lcdvrsNextToken :: Lens' ListCoreDefinitionVersionsResponse (Maybe Text)
lcdvrsNextToken = lens _lcdvrsNextToken (\ s a -> s{_lcdvrsNextToken = a})

-- | -- | The response status code.
lcdvrsResponseStatus :: Lens' ListCoreDefinitionVersionsResponse Int
lcdvrsResponseStatus = lens _lcdvrsResponseStatus (\ s a -> s{_lcdvrsResponseStatus = a})

instance NFData ListCoreDefinitionVersionsResponse
         where
