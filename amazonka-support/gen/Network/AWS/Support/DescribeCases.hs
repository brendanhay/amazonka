{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.DescribeCases
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of cases that you specify by passing one or more case
-- IDs. In addition, you can filter the cases by date by setting values for
-- the @AfterTime@ and @BeforeTime@ request parameters. You can set values
-- for the @IncludeResolvedCases@ and @IncludeCommunications@ request
-- parameters to control how much information is returned.
--
-- Case data is available for 12 months after creation. If a case was
-- created more than 12 months ago, a request for data might cause an
-- error.
--
-- The response returns the following in JSON format:
--
-- 1.  One or more CaseDetails data types.
-- 2.  One or more @NextToken@ values, which specify where to paginate the
--     returned records represented by the @CaseDetails@ objects.
--
-- <http://docs.aws.amazon.com/awssupport/latest/APIReference/API_DescribeCases.html>
module Network.AWS.Support.DescribeCases
    (
    -- * Request
      DescribeCases
    -- ** Request constructor
    , describeCases
    -- ** Request lenses
    , dcrqIncludeResolvedCases
    , dcrqCaseIdList
    , dcrqAfterTime
    , dcrqNextToken
    , dcrqBeforeTime
    , dcrqIncludeCommunications
    , dcrqDisplayId
    , dcrqLanguage
    , dcrqMaxResults

    -- * Response
    , DescribeCasesResponse
    -- ** Response constructor
    , describeCasesResponse
    -- ** Response lenses
    , dcrsCases
    , dcrsNextToken
    , dcrsStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Support.Types

-- | /See:/ 'describeCases' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcrqIncludeResolvedCases'
--
-- * 'dcrqCaseIdList'
--
-- * 'dcrqAfterTime'
--
-- * 'dcrqNextToken'
--
-- * 'dcrqBeforeTime'
--
-- * 'dcrqIncludeCommunications'
--
-- * 'dcrqDisplayId'
--
-- * 'dcrqLanguage'
--
-- * 'dcrqMaxResults'
data DescribeCases = DescribeCases'
    { _dcrqIncludeResolvedCases  :: !(Maybe Bool)
    , _dcrqCaseIdList            :: !(Maybe [Text])
    , _dcrqAfterTime             :: !(Maybe Text)
    , _dcrqNextToken             :: !(Maybe Text)
    , _dcrqBeforeTime            :: !(Maybe Text)
    , _dcrqIncludeCommunications :: !(Maybe Bool)
    , _dcrqDisplayId             :: !(Maybe Text)
    , _dcrqLanguage              :: !(Maybe Text)
    , _dcrqMaxResults            :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeCases' smart constructor.
describeCases :: DescribeCases
describeCases =
    DescribeCases'
    { _dcrqIncludeResolvedCases = Nothing
    , _dcrqCaseIdList = Nothing
    , _dcrqAfterTime = Nothing
    , _dcrqNextToken = Nothing
    , _dcrqBeforeTime = Nothing
    , _dcrqIncludeCommunications = Nothing
    , _dcrqDisplayId = Nothing
    , _dcrqLanguage = Nothing
    , _dcrqMaxResults = Nothing
    }

-- | Specifies whether resolved support cases should be included in the
-- DescribeCases results. The default is /false/.
dcrqIncludeResolvedCases :: Lens' DescribeCases (Maybe Bool)
dcrqIncludeResolvedCases = lens _dcrqIncludeResolvedCases (\ s a -> s{_dcrqIncludeResolvedCases = a});

-- | A list of ID numbers of the support cases you want returned. The maximum
-- number of cases is 100.
dcrqCaseIdList :: Lens' DescribeCases [Text]
dcrqCaseIdList = lens _dcrqCaseIdList (\ s a -> s{_dcrqCaseIdList = a}) . _Default;

-- | The start date for a filtered date search on support case
-- communications. Case communications are available for 12 months after
-- creation.
dcrqAfterTime :: Lens' DescribeCases (Maybe Text)
dcrqAfterTime = lens _dcrqAfterTime (\ s a -> s{_dcrqAfterTime = a});

-- | A resumption point for pagination.
dcrqNextToken :: Lens' DescribeCases (Maybe Text)
dcrqNextToken = lens _dcrqNextToken (\ s a -> s{_dcrqNextToken = a});

-- | The end date for a filtered date search on support case communications.
-- Case communications are available for 12 months after creation.
dcrqBeforeTime :: Lens' DescribeCases (Maybe Text)
dcrqBeforeTime = lens _dcrqBeforeTime (\ s a -> s{_dcrqBeforeTime = a});

-- | Specifies whether communications should be included in the DescribeCases
-- results. The default is /true/.
dcrqIncludeCommunications :: Lens' DescribeCases (Maybe Bool)
dcrqIncludeCommunications = lens _dcrqIncludeCommunications (\ s a -> s{_dcrqIncludeCommunications = a});

-- | The ID displayed for a case in the AWS Support Center user interface.
dcrqDisplayId :: Lens' DescribeCases (Maybe Text)
dcrqDisplayId = lens _dcrqDisplayId (\ s a -> s{_dcrqDisplayId = a});

-- | The ISO 639-1 code for the language in which AWS provides support. AWS
-- Support currently supports English (\"en\") and Japanese (\"ja\").
-- Language parameters must be passed explicitly for operations that take
-- them.
dcrqLanguage :: Lens' DescribeCases (Maybe Text)
dcrqLanguage = lens _dcrqLanguage (\ s a -> s{_dcrqLanguage = a});

-- | The maximum number of results to return before paginating.
dcrqMaxResults :: Lens' DescribeCases (Maybe Natural)
dcrqMaxResults = lens _dcrqMaxResults (\ s a -> s{_dcrqMaxResults = a}) . mapping _Nat;

instance AWSPager DescribeCases where
        page rq rs
          | stop (rs ^. dcrsNextToken) = Nothing
          | stop (rs ^. dcrsCases) = Nothing
          | otherwise =
            Just $ rq & dcrqNextToken .~ rs ^. dcrsNextToken

instance AWSRequest DescribeCases where
        type Sv DescribeCases = Support
        type Rs DescribeCases = DescribeCasesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeCasesResponse' <$>
                   (x .?> "cases" .!@ mempty) <*> (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeCases where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSSupport_20130415.DescribeCases" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeCases where
        toJSON DescribeCases'{..}
          = object
              ["includeResolvedCases" .= _dcrqIncludeResolvedCases,
               "caseIdList" .= _dcrqCaseIdList,
               "afterTime" .= _dcrqAfterTime,
               "nextToken" .= _dcrqNextToken,
               "beforeTime" .= _dcrqBeforeTime,
               "includeCommunications" .=
                 _dcrqIncludeCommunications,
               "displayId" .= _dcrqDisplayId,
               "language" .= _dcrqLanguage,
               "maxResults" .= _dcrqMaxResults]

instance ToPath DescribeCases where
        toPath = const "/"

instance ToQuery DescribeCases where
        toQuery = const mempty

-- | Returns an array of CaseDetails objects and a @NextToken@ that defines a
-- point for pagination in the result set.
--
-- /See:/ 'describeCasesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcrsCases'
--
-- * 'dcrsNextToken'
--
-- * 'dcrsStatus'
data DescribeCasesResponse = DescribeCasesResponse'
    { _dcrsCases     :: !(Maybe [CaseDetails])
    , _dcrsNextToken :: !(Maybe Text)
    , _dcrsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeCasesResponse' smart constructor.
describeCasesResponse :: Int -> DescribeCasesResponse
describeCasesResponse pStatus =
    DescribeCasesResponse'
    { _dcrsCases = Nothing
    , _dcrsNextToken = Nothing
    , _dcrsStatus = pStatus
    }

-- | The details for the cases that match the request.
dcrsCases :: Lens' DescribeCasesResponse [CaseDetails]
dcrsCases = lens _dcrsCases (\ s a -> s{_dcrsCases = a}) . _Default;

-- | A resumption point for pagination.
dcrsNextToken :: Lens' DescribeCasesResponse (Maybe Text)
dcrsNextToken = lens _dcrsNextToken (\ s a -> s{_dcrsNextToken = a});

-- | FIXME: Undocumented member.
dcrsStatus :: Lens' DescribeCasesResponse Int
dcrsStatus = lens _dcrsStatus (\ s a -> s{_dcrsStatus = a});
