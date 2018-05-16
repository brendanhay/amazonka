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
-- Module      : Network.AWS.CodeStar.ListResources
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists resources associated with a project in AWS CodeStar.
--
--
module Network.AWS.CodeStar.ListResources
    (
    -- * Creating a Request
      listResources
    , ListResources
    -- * Request Lenses
    , lrNextToken
    , lrMaxResults
    , lrProjectId

    -- * Destructuring the Response
    , listResourcesResponse
    , ListResourcesResponse
    -- * Response Lenses
    , lrrsResources
    , lrrsNextToken
    , lrrsResponseStatus
    ) where

import Network.AWS.CodeStar.Types
import Network.AWS.CodeStar.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listResources' smart constructor.
data ListResources = ListResources'
  { _lrNextToken  :: !(Maybe Text)
  , _lrMaxResults :: !(Maybe Nat)
  , _lrProjectId  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListResources' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrNextToken' - The continuation token for the next set of results, if the results cannot be returned in one response.
--
-- * 'lrMaxResults' - The maximum amount of data that can be contained in a single set of results.
--
-- * 'lrProjectId' - The ID of the project.
listResources
    :: Text -- ^ 'lrProjectId'
    -> ListResources
listResources pProjectId_ =
  ListResources'
    { _lrNextToken = Nothing
    , _lrMaxResults = Nothing
    , _lrProjectId = pProjectId_
    }


-- | The continuation token for the next set of results, if the results cannot be returned in one response.
lrNextToken :: Lens' ListResources (Maybe Text)
lrNextToken = lens _lrNextToken (\ s a -> s{_lrNextToken = a})

-- | The maximum amount of data that can be contained in a single set of results.
lrMaxResults :: Lens' ListResources (Maybe Natural)
lrMaxResults = lens _lrMaxResults (\ s a -> s{_lrMaxResults = a}) . mapping _Nat

-- | The ID of the project.
lrProjectId :: Lens' ListResources Text
lrProjectId = lens _lrProjectId (\ s a -> s{_lrProjectId = a})

instance AWSRequest ListResources where
        type Rs ListResources = ListResourcesResponse
        request = postJSON codeStar
        response
          = receiveJSON
              (\ s h x ->
                 ListResourcesResponse' <$>
                   (x .?> "resources" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListResources where

instance NFData ListResources where

instance ToHeaders ListResources where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeStar_20170419.ListResources" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListResources where
        toJSON ListResources'{..}
          = object
              (catMaybes
                 [("nextToken" .=) <$> _lrNextToken,
                  ("maxResults" .=) <$> _lrMaxResults,
                  Just ("projectId" .= _lrProjectId)])

instance ToPath ListResources where
        toPath = const "/"

instance ToQuery ListResources where
        toQuery = const mempty

-- | /See:/ 'listResourcesResponse' smart constructor.
data ListResourcesResponse = ListResourcesResponse'
  { _lrrsResources      :: !(Maybe [Resource])
  , _lrrsNextToken      :: !(Maybe Text)
  , _lrrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListResourcesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrrsResources' - An array of resources associated with the project.
--
-- * 'lrrsNextToken' - The continuation token to use when requesting the next set of results, if there are more results to be returned.
--
-- * 'lrrsResponseStatus' - -- | The response status code.
listResourcesResponse
    :: Int -- ^ 'lrrsResponseStatus'
    -> ListResourcesResponse
listResourcesResponse pResponseStatus_ =
  ListResourcesResponse'
    { _lrrsResources = Nothing
    , _lrrsNextToken = Nothing
    , _lrrsResponseStatus = pResponseStatus_
    }


-- | An array of resources associated with the project.
lrrsResources :: Lens' ListResourcesResponse [Resource]
lrrsResources = lens _lrrsResources (\ s a -> s{_lrrsResources = a}) . _Default . _Coerce

-- | The continuation token to use when requesting the next set of results, if there are more results to be returned.
lrrsNextToken :: Lens' ListResourcesResponse (Maybe Text)
lrrsNextToken = lens _lrrsNextToken (\ s a -> s{_lrrsNextToken = a})

-- | -- | The response status code.
lrrsResponseStatus :: Lens' ListResourcesResponse Int
lrrsResponseStatus = lens _lrrsResponseStatus (\ s a -> s{_lrrsResponseStatus = a})

instance NFData ListResourcesResponse where
