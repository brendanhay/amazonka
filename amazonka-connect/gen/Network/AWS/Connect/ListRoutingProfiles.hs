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
-- Module      : Network.AWS.Connect.ListRoutingProfiles
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of @RoutingProfileSummary@ objects that includes information about the routing profiles in your instance.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListRoutingProfiles
    (
    -- * Creating a Request
      listRoutingProfiles
    , ListRoutingProfiles
    -- * Request Lenses
    , lrpNextToken
    , lrpMaxResults
    , lrpInstanceId

    -- * Destructuring the Response
    , listRoutingProfilesResponse
    , ListRoutingProfilesResponse
    -- * Response Lenses
    , lrprsRoutingProfileSummaryList
    , lrprsNextToken
    , lrprsResponseStatus
    ) where

import Network.AWS.Connect.Types
import Network.AWS.Connect.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listRoutingProfiles' smart constructor.
data ListRoutingProfiles = ListRoutingProfiles'
  { _lrpNextToken  :: !(Maybe Text)
  , _lrpMaxResults :: !(Maybe Nat)
  , _lrpInstanceId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListRoutingProfiles' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrpNextToken' - The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- * 'lrpMaxResults' - The maximum number of routing profiles to return in the response.
--
-- * 'lrpInstanceId' - The identifier for your Amazon Connect instance. To find the ID of your instance, open the AWS console and select Amazon Connect. Select the alias of the instance in the Instance alias column. The instance ID is displayed in the Overview section of your instance settings. For example, the instance ID is the set of characters at the end of the instance ARN, after instance/, such as 10a4c4eb-f57e-4d4c-b602-bf39176ced07.
listRoutingProfiles
    :: Text -- ^ 'lrpInstanceId'
    -> ListRoutingProfiles
listRoutingProfiles pInstanceId_ =
  ListRoutingProfiles'
    { _lrpNextToken = Nothing
    , _lrpMaxResults = Nothing
    , _lrpInstanceId = pInstanceId_
    }


-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
lrpNextToken :: Lens' ListRoutingProfiles (Maybe Text)
lrpNextToken = lens _lrpNextToken (\ s a -> s{_lrpNextToken = a})

-- | The maximum number of routing profiles to return in the response.
lrpMaxResults :: Lens' ListRoutingProfiles (Maybe Natural)
lrpMaxResults = lens _lrpMaxResults (\ s a -> s{_lrpMaxResults = a}) . mapping _Nat

-- | The identifier for your Amazon Connect instance. To find the ID of your instance, open the AWS console and select Amazon Connect. Select the alias of the instance in the Instance alias column. The instance ID is displayed in the Overview section of your instance settings. For example, the instance ID is the set of characters at the end of the instance ARN, after instance/, such as 10a4c4eb-f57e-4d4c-b602-bf39176ced07.
lrpInstanceId :: Lens' ListRoutingProfiles Text
lrpInstanceId = lens _lrpInstanceId (\ s a -> s{_lrpInstanceId = a})

instance AWSPager ListRoutingProfiles where
        page rq rs
          | stop (rs ^. lrprsNextToken) = Nothing
          | stop (rs ^. lrprsRoutingProfileSummaryList) =
            Nothing
          | otherwise =
            Just $ rq & lrpNextToken .~ rs ^. lrprsNextToken

instance AWSRequest ListRoutingProfiles where
        type Rs ListRoutingProfiles =
             ListRoutingProfilesResponse
        request = get connect
        response
          = receiveJSON
              (\ s h x ->
                 ListRoutingProfilesResponse' <$>
                   (x .?> "RoutingProfileSummaryList" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListRoutingProfiles where

instance NFData ListRoutingProfiles where

instance ToHeaders ListRoutingProfiles where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListRoutingProfiles where
        toPath ListRoutingProfiles'{..}
          = mconcat
              ["/routing-profiles-summary/", toBS _lrpInstanceId]

instance ToQuery ListRoutingProfiles where
        toQuery ListRoutingProfiles'{..}
          = mconcat
              ["nextToken" =: _lrpNextToken,
               "maxResults" =: _lrpMaxResults]

-- | /See:/ 'listRoutingProfilesResponse' smart constructor.
data ListRoutingProfilesResponse = ListRoutingProfilesResponse'
  { _lrprsRoutingProfileSummaryList :: !(Maybe [RoutingProfileSummary])
  , _lrprsNextToken                 :: !(Maybe Text)
  , _lrprsResponseStatus            :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListRoutingProfilesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrprsRoutingProfileSummaryList' - An array of @RoutingProfileSummary@ objects that include the ARN, Id, and Name of the routing profile.
--
-- * 'lrprsNextToken' - A string returned in the response. Use the value returned in the response as the value of the NextToken in a subsequent request to retrieve the next set of results.
--
-- * 'lrprsResponseStatus' - -- | The response status code.
listRoutingProfilesResponse
    :: Int -- ^ 'lrprsResponseStatus'
    -> ListRoutingProfilesResponse
listRoutingProfilesResponse pResponseStatus_ =
  ListRoutingProfilesResponse'
    { _lrprsRoutingProfileSummaryList = Nothing
    , _lrprsNextToken = Nothing
    , _lrprsResponseStatus = pResponseStatus_
    }


-- | An array of @RoutingProfileSummary@ objects that include the ARN, Id, and Name of the routing profile.
lrprsRoutingProfileSummaryList :: Lens' ListRoutingProfilesResponse [RoutingProfileSummary]
lrprsRoutingProfileSummaryList = lens _lrprsRoutingProfileSummaryList (\ s a -> s{_lrprsRoutingProfileSummaryList = a}) . _Default . _Coerce

-- | A string returned in the response. Use the value returned in the response as the value of the NextToken in a subsequent request to retrieve the next set of results.
lrprsNextToken :: Lens' ListRoutingProfilesResponse (Maybe Text)
lrprsNextToken = lens _lrprsNextToken (\ s a -> s{_lrprsNextToken = a})

-- | -- | The response status code.
lrprsResponseStatus :: Lens' ListRoutingProfilesResponse Int
lrprsResponseStatus = lens _lrprsResponseStatus (\ s a -> s{_lrprsResponseStatus = a})

instance NFData ListRoutingProfilesResponse where
