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
-- Module      : Network.AWS.MechanicalTurk.ListBonusPayments
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @ListBonusPayments@ operation retrieves the amounts of bonuses you have paid to Workers for a given HIT or assignment.
--
--
--
-- This operation returns paginated results.
module Network.AWS.MechanicalTurk.ListBonusPayments
    (
    -- * Creating a Request
      listBonusPayments
    , ListBonusPayments
    -- * Request Lenses
    , lbpNextToken
    , lbpHITId
    , lbpAssignmentId
    , lbpMaxResults

    -- * Destructuring the Response
    , listBonusPaymentsResponse
    , ListBonusPaymentsResponse
    -- * Response Lenses
    , lbprsBonusPayments
    , lbprsNextToken
    , lbprsNumResults
    , lbprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MechanicalTurk.Types
import Network.AWS.MechanicalTurk.Types.Product
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listBonusPayments' smart constructor.
data ListBonusPayments = ListBonusPayments'
  { _lbpNextToken    :: !(Maybe Text)
  , _lbpHITId        :: !(Maybe Text)
  , _lbpAssignmentId :: !(Maybe Text)
  , _lbpMaxResults   :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListBonusPayments' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbpNextToken' - Pagination token
--
-- * 'lbpHITId' - The ID of the HIT associated with the bonus payments to retrieve. If not specified, all bonus payments for all assignments for the given HIT are returned. Either the HITId parameter or the AssignmentId parameter must be specified
--
-- * 'lbpAssignmentId' - The ID of the assignment associated with the bonus payments to retrieve. If specified, only bonus payments for the given assignment are returned. Either the HITId parameter or the AssignmentId parameter must be specified
--
-- * 'lbpMaxResults' - Undocumented member.
listBonusPayments
    :: ListBonusPayments
listBonusPayments =
  ListBonusPayments'
    { _lbpNextToken = Nothing
    , _lbpHITId = Nothing
    , _lbpAssignmentId = Nothing
    , _lbpMaxResults = Nothing
    }


-- | Pagination token
lbpNextToken :: Lens' ListBonusPayments (Maybe Text)
lbpNextToken = lens _lbpNextToken (\ s a -> s{_lbpNextToken = a})

-- | The ID of the HIT associated with the bonus payments to retrieve. If not specified, all bonus payments for all assignments for the given HIT are returned. Either the HITId parameter or the AssignmentId parameter must be specified
lbpHITId :: Lens' ListBonusPayments (Maybe Text)
lbpHITId = lens _lbpHITId (\ s a -> s{_lbpHITId = a})

-- | The ID of the assignment associated with the bonus payments to retrieve. If specified, only bonus payments for the given assignment are returned. Either the HITId parameter or the AssignmentId parameter must be specified
lbpAssignmentId :: Lens' ListBonusPayments (Maybe Text)
lbpAssignmentId = lens _lbpAssignmentId (\ s a -> s{_lbpAssignmentId = a})

-- | Undocumented member.
lbpMaxResults :: Lens' ListBonusPayments (Maybe Natural)
lbpMaxResults = lens _lbpMaxResults (\ s a -> s{_lbpMaxResults = a}) . mapping _Nat

instance AWSPager ListBonusPayments where
        page rq rs
          | stop (rs ^. lbprsNextToken) = Nothing
          | stop (rs ^. lbprsBonusPayments) = Nothing
          | otherwise =
            Just $ rq & lbpNextToken .~ rs ^. lbprsNextToken

instance AWSRequest ListBonusPayments where
        type Rs ListBonusPayments = ListBonusPaymentsResponse
        request = postJSON mechanicalTurk
        response
          = receiveJSON
              (\ s h x ->
                 ListBonusPaymentsResponse' <$>
                   (x .?> "BonusPayments" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (x .?> "NumResults")
                     <*> (pure (fromEnum s)))

instance Hashable ListBonusPayments where

instance NFData ListBonusPayments where

instance ToHeaders ListBonusPayments where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("MTurkRequesterServiceV20170117.ListBonusPayments"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListBonusPayments where
        toJSON ListBonusPayments'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lbpNextToken,
                  ("HITId" .=) <$> _lbpHITId,
                  ("AssignmentId" .=) <$> _lbpAssignmentId,
                  ("MaxResults" .=) <$> _lbpMaxResults])

instance ToPath ListBonusPayments where
        toPath = const "/"

instance ToQuery ListBonusPayments where
        toQuery = const mempty

-- | /See:/ 'listBonusPaymentsResponse' smart constructor.
data ListBonusPaymentsResponse = ListBonusPaymentsResponse'
  { _lbprsBonusPayments  :: !(Maybe [BonusPayment])
  , _lbprsNextToken      :: !(Maybe Text)
  , _lbprsNumResults     :: !(Maybe Int)
  , _lbprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListBonusPaymentsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbprsBonusPayments' - A successful request to the ListBonusPayments operation returns a list of BonusPayment objects.
--
-- * 'lbprsNextToken' - Undocumented member.
--
-- * 'lbprsNumResults' - The number of bonus payments on this page in the filtered results list, equivalent to the number of bonus payments being returned by this call.
--
-- * 'lbprsResponseStatus' - -- | The response status code.
listBonusPaymentsResponse
    :: Int -- ^ 'lbprsResponseStatus'
    -> ListBonusPaymentsResponse
listBonusPaymentsResponse pResponseStatus_ =
  ListBonusPaymentsResponse'
    { _lbprsBonusPayments = Nothing
    , _lbprsNextToken = Nothing
    , _lbprsNumResults = Nothing
    , _lbprsResponseStatus = pResponseStatus_
    }


-- | A successful request to the ListBonusPayments operation returns a list of BonusPayment objects.
lbprsBonusPayments :: Lens' ListBonusPaymentsResponse [BonusPayment]
lbprsBonusPayments = lens _lbprsBonusPayments (\ s a -> s{_lbprsBonusPayments = a}) . _Default . _Coerce

-- | Undocumented member.
lbprsNextToken :: Lens' ListBonusPaymentsResponse (Maybe Text)
lbprsNextToken = lens _lbprsNextToken (\ s a -> s{_lbprsNextToken = a})

-- | The number of bonus payments on this page in the filtered results list, equivalent to the number of bonus payments being returned by this call.
lbprsNumResults :: Lens' ListBonusPaymentsResponse (Maybe Int)
lbprsNumResults = lens _lbprsNumResults (\ s a -> s{_lbprsNumResults = a})

-- | -- | The response status code.
lbprsResponseStatus :: Lens' ListBonusPaymentsResponse Int
lbprsResponseStatus = lens _lbprsResponseStatus (\ s a -> s{_lbprsResponseStatus = a})

instance NFData ListBonusPaymentsResponse where
