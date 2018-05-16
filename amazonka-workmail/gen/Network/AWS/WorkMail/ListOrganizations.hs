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
-- Module      : Network.AWS.WorkMail.ListOrganizations
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns summaries of the customer's non-deleted organizations.
--
--
--
-- This operation returns paginated results.
module Network.AWS.WorkMail.ListOrganizations
    (
    -- * Creating a Request
      listOrganizations
    , ListOrganizations
    -- * Request Lenses
    , loNextToken
    , loMaxResults

    -- * Destructuring the Response
    , listOrganizationsResponse
    , ListOrganizationsResponse
    -- * Response Lenses
    , lorsNextToken
    , lorsOrganizationSummaries
    , lorsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkMail.Types
import Network.AWS.WorkMail.Types.Product

-- | /See:/ 'listOrganizations' smart constructor.
data ListOrganizations = ListOrganizations'
  { _loNextToken  :: !(Maybe Text)
  , _loMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListOrganizations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'loNextToken' - The token to use to retrieve the next page of results. The first call does not contain any tokens.
--
-- * 'loMaxResults' - The maximum number of results to return in a single call.
listOrganizations
    :: ListOrganizations
listOrganizations =
  ListOrganizations' {_loNextToken = Nothing, _loMaxResults = Nothing}


-- | The token to use to retrieve the next page of results. The first call does not contain any tokens.
loNextToken :: Lens' ListOrganizations (Maybe Text)
loNextToken = lens _loNextToken (\ s a -> s{_loNextToken = a})

-- | The maximum number of results to return in a single call.
loMaxResults :: Lens' ListOrganizations (Maybe Natural)
loMaxResults = lens _loMaxResults (\ s a -> s{_loMaxResults = a}) . mapping _Nat

instance AWSPager ListOrganizations where
        page rq rs
          | stop (rs ^. lorsNextToken) = Nothing
          | stop (rs ^. lorsOrganizationSummaries) = Nothing
          | otherwise =
            Just $ rq & loNextToken .~ rs ^. lorsNextToken

instance AWSRequest ListOrganizations where
        type Rs ListOrganizations = ListOrganizationsResponse
        request = postJSON workMail
        response
          = receiveJSON
              (\ s h x ->
                 ListOrganizationsResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "OrganizationSummaries" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListOrganizations where

instance NFData ListOrganizations where

instance ToHeaders ListOrganizations where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("WorkMailService.ListOrganizations" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListOrganizations where
        toJSON ListOrganizations'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _loNextToken,
                  ("MaxResults" .=) <$> _loMaxResults])

instance ToPath ListOrganizations where
        toPath = const "/"

instance ToQuery ListOrganizations where
        toQuery = const mempty

-- | /See:/ 'listOrganizationsResponse' smart constructor.
data ListOrganizationsResponse = ListOrganizationsResponse'
  { _lorsNextToken             :: !(Maybe Text)
  , _lorsOrganizationSummaries :: !(Maybe [OrganizationSummary])
  , _lorsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListOrganizationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lorsNextToken' - The token to use to retrieve the next page of results. The value is "null" when there are no more results to return.
--
-- * 'lorsOrganizationSummaries' - The overview of owned organizations presented as a list of organization summaries.
--
-- * 'lorsResponseStatus' - -- | The response status code.
listOrganizationsResponse
    :: Int -- ^ 'lorsResponseStatus'
    -> ListOrganizationsResponse
listOrganizationsResponse pResponseStatus_ =
  ListOrganizationsResponse'
    { _lorsNextToken = Nothing
    , _lorsOrganizationSummaries = Nothing
    , _lorsResponseStatus = pResponseStatus_
    }


-- | The token to use to retrieve the next page of results. The value is "null" when there are no more results to return.
lorsNextToken :: Lens' ListOrganizationsResponse (Maybe Text)
lorsNextToken = lens _lorsNextToken (\ s a -> s{_lorsNextToken = a})

-- | The overview of owned organizations presented as a list of organization summaries.
lorsOrganizationSummaries :: Lens' ListOrganizationsResponse [OrganizationSummary]
lorsOrganizationSummaries = lens _lorsOrganizationSummaries (\ s a -> s{_lorsOrganizationSummaries = a}) . _Default . _Coerce

-- | -- | The response status code.
lorsResponseStatus :: Lens' ListOrganizationsResponse Int
lorsResponseStatus = lens _lorsResponseStatus (\ s a -> s{_lorsResponseStatus = a})

instance NFData ListOrganizationsResponse where
