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
-- Module      : Network.AWS.Greengrass.ListResourceDefinitionVersions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of a resource definition.
module Network.AWS.Greengrass.ListResourceDefinitionVersions
    (
    -- * Creating a Request
      listResourceDefinitionVersions
    , ListResourceDefinitionVersions
    -- * Request Lenses
    , lrdvNextToken
    , lrdvMaxResults
    , lrdvResourceDefinitionId

    -- * Destructuring the Response
    , listResourceDefinitionVersionsResponse
    , ListResourceDefinitionVersionsResponse
    -- * Response Lenses
    , lrdvrsVersions
    , lrdvrsNextToken
    , lrdvrsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listResourceDefinitionVersions' smart constructor.
data ListResourceDefinitionVersions = ListResourceDefinitionVersions'
  { _lrdvNextToken            :: !(Maybe Text)
  , _lrdvMaxResults           :: !(Maybe Text)
  , _lrdvResourceDefinitionId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListResourceDefinitionVersions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrdvNextToken' - The token for the next set of results, or ''null'' if there are no additional results.
--
-- * 'lrdvMaxResults' - The maximum number of results to be returned per request.
--
-- * 'lrdvResourceDefinitionId' - The ID of the resource definition.
listResourceDefinitionVersions
    :: Text -- ^ 'lrdvResourceDefinitionId'
    -> ListResourceDefinitionVersions
listResourceDefinitionVersions pResourceDefinitionId_ =
  ListResourceDefinitionVersions'
    { _lrdvNextToken = Nothing
    , _lrdvMaxResults = Nothing
    , _lrdvResourceDefinitionId = pResourceDefinitionId_
    }


-- | The token for the next set of results, or ''null'' if there are no additional results.
lrdvNextToken :: Lens' ListResourceDefinitionVersions (Maybe Text)
lrdvNextToken = lens _lrdvNextToken (\ s a -> s{_lrdvNextToken = a})

-- | The maximum number of results to be returned per request.
lrdvMaxResults :: Lens' ListResourceDefinitionVersions (Maybe Text)
lrdvMaxResults = lens _lrdvMaxResults (\ s a -> s{_lrdvMaxResults = a})

-- | The ID of the resource definition.
lrdvResourceDefinitionId :: Lens' ListResourceDefinitionVersions Text
lrdvResourceDefinitionId = lens _lrdvResourceDefinitionId (\ s a -> s{_lrdvResourceDefinitionId = a})

instance AWSRequest ListResourceDefinitionVersions
         where
        type Rs ListResourceDefinitionVersions =
             ListResourceDefinitionVersionsResponse
        request = get greengrass
        response
          = receiveJSON
              (\ s h x ->
                 ListResourceDefinitionVersionsResponse' <$>
                   (x .?> "Versions" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListResourceDefinitionVersions
         where

instance NFData ListResourceDefinitionVersions where

instance ToHeaders ListResourceDefinitionVersions
         where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListResourceDefinitionVersions where
        toPath ListResourceDefinitionVersions'{..}
          = mconcat
              ["/greengrass/definition/resources/",
               toBS _lrdvResourceDefinitionId, "/versions"]

instance ToQuery ListResourceDefinitionVersions where
        toQuery ListResourceDefinitionVersions'{..}
          = mconcat
              ["NextToken" =: _lrdvNextToken,
               "MaxResults" =: _lrdvMaxResults]

-- | /See:/ 'listResourceDefinitionVersionsResponse' smart constructor.
data ListResourceDefinitionVersionsResponse = ListResourceDefinitionVersionsResponse'
  { _lrdvrsVersions       :: !(Maybe [VersionInformation])
  , _lrdvrsNextToken      :: !(Maybe Text)
  , _lrdvrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListResourceDefinitionVersionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrdvrsVersions' - Information about a version.
--
-- * 'lrdvrsNextToken' - The token for the next set of results, or ''null'' if there are no additional results.
--
-- * 'lrdvrsResponseStatus' - -- | The response status code.
listResourceDefinitionVersionsResponse
    :: Int -- ^ 'lrdvrsResponseStatus'
    -> ListResourceDefinitionVersionsResponse
listResourceDefinitionVersionsResponse pResponseStatus_ =
  ListResourceDefinitionVersionsResponse'
    { _lrdvrsVersions = Nothing
    , _lrdvrsNextToken = Nothing
    , _lrdvrsResponseStatus = pResponseStatus_
    }


-- | Information about a version.
lrdvrsVersions :: Lens' ListResourceDefinitionVersionsResponse [VersionInformation]
lrdvrsVersions = lens _lrdvrsVersions (\ s a -> s{_lrdvrsVersions = a}) . _Default . _Coerce

-- | The token for the next set of results, or ''null'' if there are no additional results.
lrdvrsNextToken :: Lens' ListResourceDefinitionVersionsResponse (Maybe Text)
lrdvrsNextToken = lens _lrdvrsNextToken (\ s a -> s{_lrdvrsNextToken = a})

-- | -- | The response status code.
lrdvrsResponseStatus :: Lens' ListResourceDefinitionVersionsResponse Int
lrdvrsResponseStatus = lens _lrdvrsResponseStatus (\ s a -> s{_lrdvrsResponseStatus = a})

instance NFData
           ListResourceDefinitionVersionsResponse
         where
