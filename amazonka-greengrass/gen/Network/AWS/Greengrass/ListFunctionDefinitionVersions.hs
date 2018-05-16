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
-- Module      : Network.AWS.Greengrass.ListFunctionDefinitionVersions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of a Lambda function definition.
module Network.AWS.Greengrass.ListFunctionDefinitionVersions
    (
    -- * Creating a Request
      listFunctionDefinitionVersions
    , ListFunctionDefinitionVersions
    -- * Request Lenses
    , lfdvNextToken
    , lfdvMaxResults
    , lfdvFunctionDefinitionId

    -- * Destructuring the Response
    , listFunctionDefinitionVersionsResponse
    , ListFunctionDefinitionVersionsResponse
    -- * Response Lenses
    , lfdvrsVersions
    , lfdvrsNextToken
    , lfdvrsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listFunctionDefinitionVersions' smart constructor.
data ListFunctionDefinitionVersions = ListFunctionDefinitionVersions'
  { _lfdvNextToken            :: !(Maybe Text)
  , _lfdvMaxResults           :: !(Maybe Text)
  , _lfdvFunctionDefinitionId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListFunctionDefinitionVersions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfdvNextToken' - The token for the next set of results, or ''null'' if there are no additional results.
--
-- * 'lfdvMaxResults' - The maximum number of results to be returned per request.
--
-- * 'lfdvFunctionDefinitionId' - The ID of the Lambda function definition.
listFunctionDefinitionVersions
    :: Text -- ^ 'lfdvFunctionDefinitionId'
    -> ListFunctionDefinitionVersions
listFunctionDefinitionVersions pFunctionDefinitionId_ =
  ListFunctionDefinitionVersions'
    { _lfdvNextToken = Nothing
    , _lfdvMaxResults = Nothing
    , _lfdvFunctionDefinitionId = pFunctionDefinitionId_
    }


-- | The token for the next set of results, or ''null'' if there are no additional results.
lfdvNextToken :: Lens' ListFunctionDefinitionVersions (Maybe Text)
lfdvNextToken = lens _lfdvNextToken (\ s a -> s{_lfdvNextToken = a})

-- | The maximum number of results to be returned per request.
lfdvMaxResults :: Lens' ListFunctionDefinitionVersions (Maybe Text)
lfdvMaxResults = lens _lfdvMaxResults (\ s a -> s{_lfdvMaxResults = a})

-- | The ID of the Lambda function definition.
lfdvFunctionDefinitionId :: Lens' ListFunctionDefinitionVersions Text
lfdvFunctionDefinitionId = lens _lfdvFunctionDefinitionId (\ s a -> s{_lfdvFunctionDefinitionId = a})

instance AWSRequest ListFunctionDefinitionVersions
         where
        type Rs ListFunctionDefinitionVersions =
             ListFunctionDefinitionVersionsResponse
        request = get greengrass
        response
          = receiveJSON
              (\ s h x ->
                 ListFunctionDefinitionVersionsResponse' <$>
                   (x .?> "Versions" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListFunctionDefinitionVersions
         where

instance NFData ListFunctionDefinitionVersions where

instance ToHeaders ListFunctionDefinitionVersions
         where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListFunctionDefinitionVersions where
        toPath ListFunctionDefinitionVersions'{..}
          = mconcat
              ["/greengrass/definition/functions/",
               toBS _lfdvFunctionDefinitionId, "/versions"]

instance ToQuery ListFunctionDefinitionVersions where
        toQuery ListFunctionDefinitionVersions'{..}
          = mconcat
              ["NextToken" =: _lfdvNextToken,
               "MaxResults" =: _lfdvMaxResults]

-- | /See:/ 'listFunctionDefinitionVersionsResponse' smart constructor.
data ListFunctionDefinitionVersionsResponse = ListFunctionDefinitionVersionsResponse'
  { _lfdvrsVersions       :: !(Maybe [VersionInformation])
  , _lfdvrsNextToken      :: !(Maybe Text)
  , _lfdvrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListFunctionDefinitionVersionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfdvrsVersions' - Information about a version.
--
-- * 'lfdvrsNextToken' - The token for the next set of results, or ''null'' if there are no additional results.
--
-- * 'lfdvrsResponseStatus' - -- | The response status code.
listFunctionDefinitionVersionsResponse
    :: Int -- ^ 'lfdvrsResponseStatus'
    -> ListFunctionDefinitionVersionsResponse
listFunctionDefinitionVersionsResponse pResponseStatus_ =
  ListFunctionDefinitionVersionsResponse'
    { _lfdvrsVersions = Nothing
    , _lfdvrsNextToken = Nothing
    , _lfdvrsResponseStatus = pResponseStatus_
    }


-- | Information about a version.
lfdvrsVersions :: Lens' ListFunctionDefinitionVersionsResponse [VersionInformation]
lfdvrsVersions = lens _lfdvrsVersions (\ s a -> s{_lfdvrsVersions = a}) . _Default . _Coerce

-- | The token for the next set of results, or ''null'' if there are no additional results.
lfdvrsNextToken :: Lens' ListFunctionDefinitionVersionsResponse (Maybe Text)
lfdvrsNextToken = lens _lfdvrsNextToken (\ s a -> s{_lfdvrsNextToken = a})

-- | -- | The response status code.
lfdvrsResponseStatus :: Lens' ListFunctionDefinitionVersionsResponse Int
lfdvrsResponseStatus = lens _lfdvrsResponseStatus (\ s a -> s{_lfdvrsResponseStatus = a})

instance NFData
           ListFunctionDefinitionVersionsResponse
         where
