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
-- Module      : Network.AWS.Support.DescribeCommunications
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns communications (and attachments) for one or more support cases.
-- You can use the 'AfterTime' and 'BeforeTime' parameters to filter by
-- date. You can use the 'CaseId' parameter to restrict the results to a
-- particular case.
--
-- Case data is available for 12 months after creation. If a case was
-- created more than 12 months ago, a request for data might cause an
-- error.
--
-- You can use the 'MaxResults' and 'NextToken' parameters to control the
-- pagination of the result set. Set 'MaxResults' to the number of cases
-- you want displayed on each page, and use 'NextToken' to specify the
-- resumption of pagination.
--
-- /See:/ <http://docs.aws.amazon.com/awssupport/latest/APIReference/API_DescribeCommunications.html AWS API Reference> for DescribeCommunications.
--
-- This operation returns paginated results.
module Network.AWS.Support.DescribeCommunications
    (
    -- * Creating a Request
      describeCommunications
    , DescribeCommunications
    -- * Request Lenses
    , dAfterTime
    , dBeforeTime
    , dNextToken
    , dMaxResults
    , dCaseId

    -- * Destructuring the Response
    , describeCommunicationsResponse
    , DescribeCommunicationsResponse
    -- * Response Lenses
    , dcrsNextToken
    , dcrsCommunications
    , dcrsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Support.Types
import           Network.AWS.Support.Types.Product

-- | /See:/ 'describeCommunications' smart constructor.
data DescribeCommunications = DescribeCommunications'
    { _dAfterTime  :: !(Maybe Text)
    , _dBeforeTime :: !(Maybe Text)
    , _dNextToken  :: !(Maybe Text)
    , _dMaxResults :: !(Maybe Nat)
    , _dCaseId     :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeCommunications' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dAfterTime'
--
-- * 'dBeforeTime'
--
-- * 'dNextToken'
--
-- * 'dMaxResults'
--
-- * 'dCaseId'
describeCommunications
    :: Text -- ^ 'dCaseId'
    -> DescribeCommunications
describeCommunications pCaseId_ =
    DescribeCommunications'
    { _dAfterTime = Nothing
    , _dBeforeTime = Nothing
    , _dNextToken = Nothing
    , _dMaxResults = Nothing
    , _dCaseId = pCaseId_
    }

-- | The start date for a filtered date search on support case
-- communications. Case communications are available for 12 months after
-- creation.
dAfterTime :: Lens' DescribeCommunications (Maybe Text)
dAfterTime = lens _dAfterTime (\ s a -> s{_dAfterTime = a});

-- | The end date for a filtered date search on support case communications.
-- Case communications are available for 12 months after creation.
dBeforeTime :: Lens' DescribeCommunications (Maybe Text)
dBeforeTime = lens _dBeforeTime (\ s a -> s{_dBeforeTime = a});

-- | A resumption point for pagination.
dNextToken :: Lens' DescribeCommunications (Maybe Text)
dNextToken = lens _dNextToken (\ s a -> s{_dNextToken = a});

-- | The maximum number of results to return before paginating.
dMaxResults :: Lens' DescribeCommunications (Maybe Natural)
dMaxResults = lens _dMaxResults (\ s a -> s{_dMaxResults = a}) . mapping _Nat;

-- | The AWS Support case ID requested or returned in the call. The case ID
-- is an alphanumeric string formatted as shown in this example:
-- case-/12345678910-2013-c4c1d2bf33c5cf47/
dCaseId :: Lens' DescribeCommunications Text
dCaseId = lens _dCaseId (\ s a -> s{_dCaseId = a});

instance AWSPager DescribeCommunications where
        page rq rs
          | stop (rs ^. dcrsNextToken) = Nothing
          | stop (rs ^. dcrsCommunications) = Nothing
          | otherwise =
            Just $ rq & dNextToken .~ rs ^. dcrsNextToken

instance AWSRequest DescribeCommunications where
        type Rs DescribeCommunications =
             DescribeCommunicationsResponse
        request = postJSON support
        response
          = receiveJSON
              (\ s h x ->
                 DescribeCommunicationsResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "communications" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeCommunications where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSSupport_20130415.DescribeCommunications" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeCommunications where
        toJSON DescribeCommunications'{..}
          = object
              (catMaybes
                 [("afterTime" .=) <$> _dAfterTime,
                  ("beforeTime" .=) <$> _dBeforeTime,
                  ("nextToken" .=) <$> _dNextToken,
                  ("maxResults" .=) <$> _dMaxResults,
                  Just ("caseId" .= _dCaseId)])

instance ToPath DescribeCommunications where
        toPath = const "/"

instance ToQuery DescribeCommunications where
        toQuery = const mempty

-- | The communications returned by the DescribeCommunications operation.
--
-- /See:/ 'describeCommunicationsResponse' smart constructor.
data DescribeCommunicationsResponse = DescribeCommunicationsResponse'
    { _dcrsNextToken      :: !(Maybe Text)
    , _dcrsCommunications :: !(Maybe [Communication])
    , _dcrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeCommunicationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcrsNextToken'
--
-- * 'dcrsCommunications'
--
-- * 'dcrsResponseStatus'
describeCommunicationsResponse
    :: Int -- ^ 'dcrsResponseStatus'
    -> DescribeCommunicationsResponse
describeCommunicationsResponse pResponseStatus_ =
    DescribeCommunicationsResponse'
    { _dcrsNextToken = Nothing
    , _dcrsCommunications = Nothing
    , _dcrsResponseStatus = pResponseStatus_
    }

-- | A resumption point for pagination.
dcrsNextToken :: Lens' DescribeCommunicationsResponse (Maybe Text)
dcrsNextToken = lens _dcrsNextToken (\ s a -> s{_dcrsNextToken = a});

-- | The communications for the case.
dcrsCommunications :: Lens' DescribeCommunicationsResponse [Communication]
dcrsCommunications = lens _dcrsCommunications (\ s a -> s{_dcrsCommunications = a}) . _Default . _Coerce;

-- | The response status code.
dcrsResponseStatus :: Lens' DescribeCommunicationsResponse Int
dcrsResponseStatus = lens _dcrsResponseStatus (\ s a -> s{_dcrsResponseStatus = a});
