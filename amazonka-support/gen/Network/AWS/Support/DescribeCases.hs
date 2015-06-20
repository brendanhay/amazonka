{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Support.DescribeCases
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns a list of cases that you specify by passing one or more case
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
    , dcIncludeResolvedCases
    , dcCaseIdList
    , dcAfterTime
    , dcNextToken
    , dcBeforeTime
    , dcIncludeCommunications
    , dcDisplayId
    , dcLanguage
    , dcMaxResults

    -- * Response
    , DescribeCasesResponse
    -- ** Response constructor
    , describeCasesResponse
    -- ** Response lenses
    , desCases
    , desNextToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Support.Types

-- | /See:/ 'describeCases' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcIncludeResolvedCases'
--
-- * 'dcCaseIdList'
--
-- * 'dcAfterTime'
--
-- * 'dcNextToken'
--
-- * 'dcBeforeTime'
--
-- * 'dcIncludeCommunications'
--
-- * 'dcDisplayId'
--
-- * 'dcLanguage'
--
-- * 'dcMaxResults'
data DescribeCases = DescribeCases'{_dcIncludeResolvedCases :: Maybe Bool, _dcCaseIdList :: Maybe [Text], _dcAfterTime :: Maybe Text, _dcNextToken :: Maybe Text, _dcBeforeTime :: Maybe Text, _dcIncludeCommunications :: Maybe Bool, _dcDisplayId :: Maybe Text, _dcLanguage :: Maybe Text, _dcMaxResults :: Maybe Nat} deriving (Eq, Read, Show)

-- | 'DescribeCases' smart constructor.
describeCases :: DescribeCases
describeCases = DescribeCases'{_dcIncludeResolvedCases = Nothing, _dcCaseIdList = Nothing, _dcAfterTime = Nothing, _dcNextToken = Nothing, _dcBeforeTime = Nothing, _dcIncludeCommunications = Nothing, _dcDisplayId = Nothing, _dcLanguage = Nothing, _dcMaxResults = Nothing};

-- | Specifies whether resolved support cases should be included in the
-- DescribeCases results. The default is /false/.
dcIncludeResolvedCases :: Lens' DescribeCases (Maybe Bool)
dcIncludeResolvedCases = lens _dcIncludeResolvedCases (\ s a -> s{_dcIncludeResolvedCases = a});

-- | A list of ID numbers of the support cases you want returned. The maximum
-- number of cases is 100.
dcCaseIdList :: Lens' DescribeCases [Text]
dcCaseIdList = lens _dcCaseIdList (\ s a -> s{_dcCaseIdList = a}) . _Default;

-- | The start date for a filtered date search on support case
-- communications. Case communications are available for 12 months after
-- creation.
dcAfterTime :: Lens' DescribeCases (Maybe Text)
dcAfterTime = lens _dcAfterTime (\ s a -> s{_dcAfterTime = a});

-- | A resumption point for pagination.
dcNextToken :: Lens' DescribeCases (Maybe Text)
dcNextToken = lens _dcNextToken (\ s a -> s{_dcNextToken = a});

-- | The end date for a filtered date search on support case communications.
-- Case communications are available for 12 months after creation.
dcBeforeTime :: Lens' DescribeCases (Maybe Text)
dcBeforeTime = lens _dcBeforeTime (\ s a -> s{_dcBeforeTime = a});

-- | Specifies whether communications should be included in the DescribeCases
-- results. The default is /true/.
dcIncludeCommunications :: Lens' DescribeCases (Maybe Bool)
dcIncludeCommunications = lens _dcIncludeCommunications (\ s a -> s{_dcIncludeCommunications = a});

-- | The ID displayed for a case in the AWS Support Center user interface.
dcDisplayId :: Lens' DescribeCases (Maybe Text)
dcDisplayId = lens _dcDisplayId (\ s a -> s{_dcDisplayId = a});

-- | The ISO 639-1 code for the language in which AWS provides support. AWS
-- Support currently supports English (\"en\") and Japanese (\"ja\").
-- Language parameters must be passed explicitly for operations that take
-- them.
dcLanguage :: Lens' DescribeCases (Maybe Text)
dcLanguage = lens _dcLanguage (\ s a -> s{_dcLanguage = a});

-- | The maximum number of results to return before paginating.
dcMaxResults :: Lens' DescribeCases (Maybe Natural)
dcMaxResults = lens _dcMaxResults (\ s a -> s{_dcMaxResults = a}) . mapping _Nat;

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest DescribeCases where
        type Sv DescribeCases = Support
        type Rs DescribeCases = DescribeCasesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeCasesResponse' <$>
                   (x .?> "cases" .!@ mempty) <*> (x .?> "nextToken"))

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
              ["includeResolvedCases" .= _dcIncludeResolvedCases,
               "caseIdList" .= _dcCaseIdList,
               "afterTime" .= _dcAfterTime,
               "nextToken" .= _dcNextToken,
               "beforeTime" .= _dcBeforeTime,
               "includeCommunications" .= _dcIncludeCommunications,
               "displayId" .= _dcDisplayId,
               "language" .= _dcLanguage,
               "maxResults" .= _dcMaxResults]

instance ToPath DescribeCases where
        toPath = const "/"

instance ToQuery DescribeCases where
        toQuery = const mempty

-- | /See:/ 'describeCasesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desCases'
--
-- * 'desNextToken'
data DescribeCasesResponse = DescribeCasesResponse'{_desCases :: Maybe [CaseDetails], _desNextToken :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DescribeCasesResponse' smart constructor.
describeCasesResponse :: DescribeCasesResponse
describeCasesResponse = DescribeCasesResponse'{_desCases = Nothing, _desNextToken = Nothing};

-- | The details for the cases that match the request.
desCases :: Lens' DescribeCasesResponse [CaseDetails]
desCases = lens _desCases (\ s a -> s{_desCases = a}) . _Default;

-- | A resumption point for pagination.
desNextToken :: Lens' DescribeCasesResponse (Maybe Text)
desNextToken = lens _desNextToken (\ s a -> s{_desNextToken = a});
