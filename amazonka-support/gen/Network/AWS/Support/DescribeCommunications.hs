{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.DescribeCommunications
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns communications (and attachments) for one or more support cases.
-- You can use the @AfterTime@ and @BeforeTime@ parameters to filter by
-- date. You can use the @CaseId@ parameter to restrict the results to a
-- particular case.
--
-- Case data is available for 12 months after creation. If a case was
-- created more than 12 months ago, a request for data might cause an
-- error.
--
-- You can use the @MaxResults@ and @NextToken@ parameters to control the
-- pagination of the result set. Set @MaxResults@ to the number of cases
-- you want displayed on each page, and use @NextToken@ to specify the
-- resumption of pagination.
--
-- <http://docs.aws.amazon.com/awssupport/latest/APIReference/API_DescribeCommunications.html>
module Network.AWS.Support.DescribeCommunications
    (
    -- * Request
      DescribeCommunications
    -- ** Request constructor
    , describeCommunications
    -- ** Request lenses
    , dAfterTime
    , dNextToken
    , dBeforeTime
    , dMaxResults
    , dCaseId

    -- * Response
    , DescribeCommunicationsResponse
    -- ** Response constructor
    , describeCommunicationsResponse
    -- ** Response lenses
    , dcrsNextToken
    , dcrsCommunications
    , dcrsStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Support.Types

-- | /See:/ 'describeCommunications' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dAfterTime'
--
-- * 'dNextToken'
--
-- * 'dBeforeTime'
--
-- * 'dMaxResults'
--
-- * 'dCaseId'
data DescribeCommunications = DescribeCommunications'
    { _dAfterTime  :: !(Maybe Text)
    , _dNextToken  :: !(Maybe Text)
    , _dBeforeTime :: !(Maybe Text)
    , _dMaxResults :: !(Maybe Nat)
    , _dCaseId     :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeCommunications' smart constructor.
describeCommunications :: Text -> DescribeCommunications
describeCommunications pCaseId_ =
    DescribeCommunications'
    { _dAfterTime = Nothing
    , _dNextToken = Nothing
    , _dBeforeTime = Nothing
    , _dMaxResults = Nothing
    , _dCaseId = pCaseId_
    }

-- | The start date for a filtered date search on support case
-- communications. Case communications are available for 12 months after
-- creation.
dAfterTime :: Lens' DescribeCommunications (Maybe Text)
dAfterTime = lens _dAfterTime (\ s a -> s{_dAfterTime = a});

-- | A resumption point for pagination.
dNextToken :: Lens' DescribeCommunications (Maybe Text)
dNextToken = lens _dNextToken (\ s a -> s{_dNextToken = a});

-- | The end date for a filtered date search on support case communications.
-- Case communications are available for 12 months after creation.
dBeforeTime :: Lens' DescribeCommunications (Maybe Text)
dBeforeTime = lens _dBeforeTime (\ s a -> s{_dBeforeTime = a});

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
        type Sv DescribeCommunications = Support
        type Rs DescribeCommunications =
             DescribeCommunicationsResponse
        request = postJSON "DescribeCommunications"
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
              ["afterTime" .= _dAfterTime,
               "nextToken" .= _dNextToken,
               "beforeTime" .= _dBeforeTime,
               "maxResults" .= _dMaxResults, "caseId" .= _dCaseId]

instance ToPath DescribeCommunications where
        toPath = const "/"

instance ToQuery DescribeCommunications where
        toQuery = const mempty

-- | The communications returned by the DescribeCommunications operation.
--
-- /See:/ 'describeCommunicationsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcrsNextToken'
--
-- * 'dcrsCommunications'
--
-- * 'dcrsStatus'
data DescribeCommunicationsResponse = DescribeCommunicationsResponse'
    { _dcrsNextToken      :: !(Maybe Text)
    , _dcrsCommunications :: !(Maybe [Communication])
    , _dcrsStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeCommunicationsResponse' smart constructor.
describeCommunicationsResponse :: Int -> DescribeCommunicationsResponse
describeCommunicationsResponse pStatus_ =
    DescribeCommunicationsResponse'
    { _dcrsNextToken = Nothing
    , _dcrsCommunications = Nothing
    , _dcrsStatus = pStatus_
    }

-- | A resumption point for pagination.
dcrsNextToken :: Lens' DescribeCommunicationsResponse (Maybe Text)
dcrsNextToken = lens _dcrsNextToken (\ s a -> s{_dcrsNextToken = a});

-- | The communications for the case.
dcrsCommunications :: Lens' DescribeCommunicationsResponse [Communication]
dcrsCommunications = lens _dcrsCommunications (\ s a -> s{_dcrsCommunications = a}) . _Default;

-- | FIXME: Undocumented member.
dcrsStatus :: Lens' DescribeCommunicationsResponse Int
dcrsStatus = lens _dcrsStatus (\ s a -> s{_dcrsStatus = a});
