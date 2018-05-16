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
-- Module      : Network.AWS.MechanicalTurk.ListReviewableHITs
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @ListReviewableHITs@ operation retrieves the HITs with Status equal to Reviewable or Status equal to Reviewing that belong to the Requester calling the operation.
--
--
--
-- This operation returns paginated results.
module Network.AWS.MechanicalTurk.ListReviewableHITs
    (
    -- * Creating a Request
      listReviewableHITs
    , ListReviewableHITs
    -- * Request Lenses
    , lrhitStatus
    , lrhitHITTypeId
    , lrhitNextToken
    , lrhitMaxResults

    -- * Destructuring the Response
    , listReviewableHITsResponse
    , ListReviewableHITsResponse
    -- * Response Lenses
    , lrhitrsNextToken
    , lrhitrsNumResults
    , lrhitrsHITs
    , lrhitrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MechanicalTurk.Types
import Network.AWS.MechanicalTurk.Types.Product
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listReviewableHITs' smart constructor.
data ListReviewableHITs = ListReviewableHITs'
  { _lrhitStatus     :: !(Maybe ReviewableHITStatus)
  , _lrhitHITTypeId  :: !(Maybe Text)
  , _lrhitNextToken  :: !(Maybe Text)
  , _lrhitMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListReviewableHITs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrhitStatus' - Can be either @Reviewable@ or @Reviewing@ . Reviewable is the default value.
--
-- * 'lrhitHITTypeId' - The ID of the HIT type of the HITs to consider for the query. If not specified, all HITs for the Reviewer are considered
--
-- * 'lrhitNextToken' - Pagination Token
--
-- * 'lrhitMaxResults' - Limit the number of results returned.
listReviewableHITs
    :: ListReviewableHITs
listReviewableHITs =
  ListReviewableHITs'
    { _lrhitStatus = Nothing
    , _lrhitHITTypeId = Nothing
    , _lrhitNextToken = Nothing
    , _lrhitMaxResults = Nothing
    }


-- | Can be either @Reviewable@ or @Reviewing@ . Reviewable is the default value.
lrhitStatus :: Lens' ListReviewableHITs (Maybe ReviewableHITStatus)
lrhitStatus = lens _lrhitStatus (\ s a -> s{_lrhitStatus = a})

-- | The ID of the HIT type of the HITs to consider for the query. If not specified, all HITs for the Reviewer are considered
lrhitHITTypeId :: Lens' ListReviewableHITs (Maybe Text)
lrhitHITTypeId = lens _lrhitHITTypeId (\ s a -> s{_lrhitHITTypeId = a})

-- | Pagination Token
lrhitNextToken :: Lens' ListReviewableHITs (Maybe Text)
lrhitNextToken = lens _lrhitNextToken (\ s a -> s{_lrhitNextToken = a})

-- | Limit the number of results returned.
lrhitMaxResults :: Lens' ListReviewableHITs (Maybe Natural)
lrhitMaxResults = lens _lrhitMaxResults (\ s a -> s{_lrhitMaxResults = a}) . mapping _Nat

instance AWSPager ListReviewableHITs where
        page rq rs
          | stop (rs ^. lrhitrsNextToken) = Nothing
          | stop (rs ^. lrhitrsHITs) = Nothing
          | otherwise =
            Just $ rq & lrhitNextToken .~ rs ^. lrhitrsNextToken

instance AWSRequest ListReviewableHITs where
        type Rs ListReviewableHITs =
             ListReviewableHITsResponse
        request = postJSON mechanicalTurk
        response
          = receiveJSON
              (\ s h x ->
                 ListReviewableHITsResponse' <$>
                   (x .?> "NextToken") <*> (x .?> "NumResults") <*>
                     (x .?> "HITs" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListReviewableHITs where

instance NFData ListReviewableHITs where

instance ToHeaders ListReviewableHITs where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("MTurkRequesterServiceV20170117.ListReviewableHITs"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListReviewableHITs where
        toJSON ListReviewableHITs'{..}
          = object
              (catMaybes
                 [("Status" .=) <$> _lrhitStatus,
                  ("HITTypeId" .=) <$> _lrhitHITTypeId,
                  ("NextToken" .=) <$> _lrhitNextToken,
                  ("MaxResults" .=) <$> _lrhitMaxResults])

instance ToPath ListReviewableHITs where
        toPath = const "/"

instance ToQuery ListReviewableHITs where
        toQuery = const mempty

-- | /See:/ 'listReviewableHITsResponse' smart constructor.
data ListReviewableHITsResponse = ListReviewableHITsResponse'
  { _lrhitrsNextToken      :: !(Maybe Text)
  , _lrhitrsNumResults     :: !(Maybe Int)
  , _lrhitrsHITs           :: !(Maybe [HIT])
  , _lrhitrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListReviewableHITsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrhitrsNextToken' - Undocumented member.
--
-- * 'lrhitrsNumResults' - The number of HITs on this page in the filtered results list, equivalent to the number of HITs being returned by this call.
--
-- * 'lrhitrsHITs' - The list of HIT elements returned by the query.
--
-- * 'lrhitrsResponseStatus' - -- | The response status code.
listReviewableHITsResponse
    :: Int -- ^ 'lrhitrsResponseStatus'
    -> ListReviewableHITsResponse
listReviewableHITsResponse pResponseStatus_ =
  ListReviewableHITsResponse'
    { _lrhitrsNextToken = Nothing
    , _lrhitrsNumResults = Nothing
    , _lrhitrsHITs = Nothing
    , _lrhitrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
lrhitrsNextToken :: Lens' ListReviewableHITsResponse (Maybe Text)
lrhitrsNextToken = lens _lrhitrsNextToken (\ s a -> s{_lrhitrsNextToken = a})

-- | The number of HITs on this page in the filtered results list, equivalent to the number of HITs being returned by this call.
lrhitrsNumResults :: Lens' ListReviewableHITsResponse (Maybe Int)
lrhitrsNumResults = lens _lrhitrsNumResults (\ s a -> s{_lrhitrsNumResults = a})

-- | The list of HIT elements returned by the query.
lrhitrsHITs :: Lens' ListReviewableHITsResponse [HIT]
lrhitrsHITs = lens _lrhitrsHITs (\ s a -> s{_lrhitrsHITs = a}) . _Default . _Coerce

-- | -- | The response status code.
lrhitrsResponseStatus :: Lens' ListReviewableHITsResponse Int
lrhitrsResponseStatus = lens _lrhitrsResponseStatus (\ s a -> s{_lrhitrsResponseStatus = a})

instance NFData ListReviewableHITsResponse where
