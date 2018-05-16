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
-- Module      : Network.AWS.MechanicalTurk.ListQualificationRequests
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @ListQualificationRequests@ operation retrieves requests for Qualifications of a particular Qualification type. The owner of the Qualification type calls this operation to poll for pending requests, and accepts them using the AcceptQualification operation.
--
--
--
-- This operation returns paginated results.
module Network.AWS.MechanicalTurk.ListQualificationRequests
    (
    -- * Creating a Request
      listQualificationRequests
    , ListQualificationRequests
    -- * Request Lenses
    , lqrNextToken
    , lqrQualificationTypeId
    , lqrMaxResults

    -- * Destructuring the Response
    , listQualificationRequestsResponse
    , ListQualificationRequestsResponse
    -- * Response Lenses
    , lqrrsQualificationRequests
    , lqrrsNextToken
    , lqrrsNumResults
    , lqrrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MechanicalTurk.Types
import Network.AWS.MechanicalTurk.Types.Product
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listQualificationRequests' smart constructor.
data ListQualificationRequests = ListQualificationRequests'
  { _lqrNextToken           :: !(Maybe Text)
  , _lqrQualificationTypeId :: !(Maybe Text)
  , _lqrMaxResults          :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListQualificationRequests' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lqrNextToken' - Undocumented member.
--
-- * 'lqrQualificationTypeId' - The ID of the QualificationType.
--
-- * 'lqrMaxResults' - The maximum number of results to return in a single call.
listQualificationRequests
    :: ListQualificationRequests
listQualificationRequests =
  ListQualificationRequests'
    { _lqrNextToken = Nothing
    , _lqrQualificationTypeId = Nothing
    , _lqrMaxResults = Nothing
    }


-- | Undocumented member.
lqrNextToken :: Lens' ListQualificationRequests (Maybe Text)
lqrNextToken = lens _lqrNextToken (\ s a -> s{_lqrNextToken = a})

-- | The ID of the QualificationType.
lqrQualificationTypeId :: Lens' ListQualificationRequests (Maybe Text)
lqrQualificationTypeId = lens _lqrQualificationTypeId (\ s a -> s{_lqrQualificationTypeId = a})

-- | The maximum number of results to return in a single call.
lqrMaxResults :: Lens' ListQualificationRequests (Maybe Natural)
lqrMaxResults = lens _lqrMaxResults (\ s a -> s{_lqrMaxResults = a}) . mapping _Nat

instance AWSPager ListQualificationRequests where
        page rq rs
          | stop (rs ^. lqrrsNextToken) = Nothing
          | stop (rs ^. lqrrsQualificationRequests) = Nothing
          | otherwise =
            Just $ rq & lqrNextToken .~ rs ^. lqrrsNextToken

instance AWSRequest ListQualificationRequests where
        type Rs ListQualificationRequests =
             ListQualificationRequestsResponse
        request = postJSON mechanicalTurk
        response
          = receiveJSON
              (\ s h x ->
                 ListQualificationRequestsResponse' <$>
                   (x .?> "QualificationRequests" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (x .?> "NumResults")
                     <*> (pure (fromEnum s)))

instance Hashable ListQualificationRequests where

instance NFData ListQualificationRequests where

instance ToHeaders ListQualificationRequests where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("MTurkRequesterServiceV20170117.ListQualificationRequests"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListQualificationRequests where
        toJSON ListQualificationRequests'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lqrNextToken,
                  ("QualificationTypeId" .=) <$>
                    _lqrQualificationTypeId,
                  ("MaxResults" .=) <$> _lqrMaxResults])

instance ToPath ListQualificationRequests where
        toPath = const "/"

instance ToQuery ListQualificationRequests where
        toQuery = const mempty

-- | /See:/ 'listQualificationRequestsResponse' smart constructor.
data ListQualificationRequestsResponse = ListQualificationRequestsResponse'
  { _lqrrsQualificationRequests :: !(Maybe [QualificationRequest])
  , _lqrrsNextToken             :: !(Maybe Text)
  , _lqrrsNumResults            :: !(Maybe Int)
  , _lqrrsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListQualificationRequestsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lqrrsQualificationRequests' - The Qualification request. The response includes one QualificationRequest element for each Qualification request returned by the query.
--
-- * 'lqrrsNextToken' - Undocumented member.
--
-- * 'lqrrsNumResults' - The number of Qualification requests on this page in the filtered results list, equivalent to the number of Qualification requests being returned by this call.
--
-- * 'lqrrsResponseStatus' - -- | The response status code.
listQualificationRequestsResponse
    :: Int -- ^ 'lqrrsResponseStatus'
    -> ListQualificationRequestsResponse
listQualificationRequestsResponse pResponseStatus_ =
  ListQualificationRequestsResponse'
    { _lqrrsQualificationRequests = Nothing
    , _lqrrsNextToken = Nothing
    , _lqrrsNumResults = Nothing
    , _lqrrsResponseStatus = pResponseStatus_
    }


-- | The Qualification request. The response includes one QualificationRequest element for each Qualification request returned by the query.
lqrrsQualificationRequests :: Lens' ListQualificationRequestsResponse [QualificationRequest]
lqrrsQualificationRequests = lens _lqrrsQualificationRequests (\ s a -> s{_lqrrsQualificationRequests = a}) . _Default . _Coerce

-- | Undocumented member.
lqrrsNextToken :: Lens' ListQualificationRequestsResponse (Maybe Text)
lqrrsNextToken = lens _lqrrsNextToken (\ s a -> s{_lqrrsNextToken = a})

-- | The number of Qualification requests on this page in the filtered results list, equivalent to the number of Qualification requests being returned by this call.
lqrrsNumResults :: Lens' ListQualificationRequestsResponse (Maybe Int)
lqrrsNumResults = lens _lqrrsNumResults (\ s a -> s{_lqrrsNumResults = a})

-- | -- | The response status code.
lqrrsResponseStatus :: Lens' ListQualificationRequestsResponse Int
lqrrsResponseStatus = lens _lqrrsResponseStatus (\ s a -> s{_lqrrsResponseStatus = a})

instance NFData ListQualificationRequestsResponse
         where
