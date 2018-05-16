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
-- Module      : Network.AWS.WorkMail.ListResources
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns summaries of the organization's resources.
--
--
--
-- This operation returns paginated results.
module Network.AWS.WorkMail.ListResources
    (
    -- * Creating a Request
      listResources
    , ListResources
    -- * Request Lenses
    , lrNextToken
    , lrMaxResults
    , lrOrganizationId

    -- * Destructuring the Response
    , listResourcesResponse
    , ListResourcesResponse
    -- * Response Lenses
    , lrrsResources
    , lrrsNextToken
    , lrrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkMail.Types
import Network.AWS.WorkMail.Types.Product

-- | /See:/ 'listResources' smart constructor.
data ListResources = ListResources'
  { _lrNextToken      :: !(Maybe Text)
  , _lrMaxResults     :: !(Maybe Nat)
  , _lrOrganizationId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListResources' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrNextToken' - The token to use to retrieve the next page of results. The first call does not contain any tokens.
--
-- * 'lrMaxResults' - The maximum number of results to return in a single call.
--
-- * 'lrOrganizationId' - The identifier for the organization under which the resources exist.
listResources
    :: Text -- ^ 'lrOrganizationId'
    -> ListResources
listResources pOrganizationId_ =
  ListResources'
    { _lrNextToken = Nothing
    , _lrMaxResults = Nothing
    , _lrOrganizationId = pOrganizationId_
    }


-- | The token to use to retrieve the next page of results. The first call does not contain any tokens.
lrNextToken :: Lens' ListResources (Maybe Text)
lrNextToken = lens _lrNextToken (\ s a -> s{_lrNextToken = a})

-- | The maximum number of results to return in a single call.
lrMaxResults :: Lens' ListResources (Maybe Natural)
lrMaxResults = lens _lrMaxResults (\ s a -> s{_lrMaxResults = a}) . mapping _Nat

-- | The identifier for the organization under which the resources exist.
lrOrganizationId :: Lens' ListResources Text
lrOrganizationId = lens _lrOrganizationId (\ s a -> s{_lrOrganizationId = a})

instance AWSPager ListResources where
        page rq rs
          | stop (rs ^. lrrsNextToken) = Nothing
          | stop (rs ^. lrrsResources) = Nothing
          | otherwise =
            Just $ rq & lrNextToken .~ rs ^. lrrsNextToken

instance AWSRequest ListResources where
        type Rs ListResources = ListResourcesResponse
        request = postJSON workMail
        response
          = receiveJSON
              (\ s h x ->
                 ListResourcesResponse' <$>
                   (x .?> "Resources" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListResources where

instance NFData ListResources where

instance ToHeaders ListResources where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("WorkMailService.ListResources" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListResources where
        toJSON ListResources'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lrNextToken,
                  ("MaxResults" .=) <$> _lrMaxResults,
                  Just ("OrganizationId" .= _lrOrganizationId)])

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
-- * 'lrrsResources' - One page of the organization's resource representation.
--
-- * 'lrrsNextToken' - The token used to paginate through all the organization's resources. While results are still available, it has an associated value. When the last page is reached, the token is empty.
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


-- | One page of the organization's resource representation.
lrrsResources :: Lens' ListResourcesResponse [Resource]
lrrsResources = lens _lrrsResources (\ s a -> s{_lrrsResources = a}) . _Default . _Coerce

-- | The token used to paginate through all the organization's resources. While results are still available, it has an associated value. When the last page is reached, the token is empty.
lrrsNextToken :: Lens' ListResourcesResponse (Maybe Text)
lrrsNextToken = lens _lrrsNextToken (\ s a -> s{_lrrsNextToken = a})

-- | -- | The response status code.
lrrsResponseStatus :: Lens' ListResourcesResponse Int
lrrsResponseStatus = lens _lrrsResponseStatus (\ s a -> s{_lrrsResponseStatus = a})

instance NFData ListResourcesResponse where
