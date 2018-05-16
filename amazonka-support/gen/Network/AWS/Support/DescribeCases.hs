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
-- Module      : Network.AWS.Support.DescribeCases
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of cases that you specify by passing one or more case IDs. In addition, you can filter the cases by date by setting values for the @afterTime@ and @beforeTime@ request parameters. You can set values for the @includeResolvedCases@ and @includeCommunications@ request parameters to control how much information is returned.
--
--
-- Case data is available for 12 months after creation. If a case was created more than 12 months ago, a request for data might cause an error.
--
-- The response returns the following in JSON format:
--
--     * One or more 'CaseDetails' data types.
--
--     * One or more @nextToken@ values, which specify where to paginate the returned records represented by the @CaseDetails@ objects.
--
--
--
--
-- This operation returns paginated results.
module Network.AWS.Support.DescribeCases
    (
    -- * Creating a Request
      describeCases
    , DescribeCases
    -- * Request Lenses
    , dcIncludeResolvedCases
    , dcCaseIdList
    , dcAfterTime
    , dcBeforeTime
    , dcNextToken
    , dcIncludeCommunications
    , dcDisplayId
    , dcLanguage
    , dcMaxResults

    -- * Destructuring the Response
    , describeCasesResponse
    , DescribeCasesResponse
    -- * Response Lenses
    , drsCases
    , drsNextToken
    , drsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Support.Types
import Network.AWS.Support.Types.Product

-- |
--
--
--
-- /See:/ 'describeCases' smart constructor.
data DescribeCases = DescribeCases'
  { _dcIncludeResolvedCases  :: !(Maybe Bool)
  , _dcCaseIdList            :: !(Maybe [Text])
  , _dcAfterTime             :: !(Maybe Text)
  , _dcBeforeTime            :: !(Maybe Text)
  , _dcNextToken             :: !(Maybe Text)
  , _dcIncludeCommunications :: !(Maybe Bool)
  , _dcDisplayId             :: !(Maybe Text)
  , _dcLanguage              :: !(Maybe Text)
  , _dcMaxResults            :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeCases' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcIncludeResolvedCases' - Specifies whether resolved support cases should be included in the 'DescribeCases' results. The default is /false/ .
--
-- * 'dcCaseIdList' - A list of ID numbers of the support cases you want returned. The maximum number of cases is 100.
--
-- * 'dcAfterTime' - The start date for a filtered date search on support case communications. Case communications are available for 12 months after creation.
--
-- * 'dcBeforeTime' - The end date for a filtered date search on support case communications. Case communications are available for 12 months after creation.
--
-- * 'dcNextToken' - A resumption point for pagination.
--
-- * 'dcIncludeCommunications' - Specifies whether communications should be included in the 'DescribeCases' results. The default is /true/ .
--
-- * 'dcDisplayId' - The ID displayed for a case in the AWS Support Center user interface.
--
-- * 'dcLanguage' - The ISO 639-1 code for the language in which AWS provides support. AWS Support currently supports English ("en") and Japanese ("ja"). Language parameters must be passed explicitly for operations that take them.
--
-- * 'dcMaxResults' - The maximum number of results to return before paginating.
describeCases
    :: DescribeCases
describeCases =
  DescribeCases'
    { _dcIncludeResolvedCases = Nothing
    , _dcCaseIdList = Nothing
    , _dcAfterTime = Nothing
    , _dcBeforeTime = Nothing
    , _dcNextToken = Nothing
    , _dcIncludeCommunications = Nothing
    , _dcDisplayId = Nothing
    , _dcLanguage = Nothing
    , _dcMaxResults = Nothing
    }


-- | Specifies whether resolved support cases should be included in the 'DescribeCases' results. The default is /false/ .
dcIncludeResolvedCases :: Lens' DescribeCases (Maybe Bool)
dcIncludeResolvedCases = lens _dcIncludeResolvedCases (\ s a -> s{_dcIncludeResolvedCases = a})

-- | A list of ID numbers of the support cases you want returned. The maximum number of cases is 100.
dcCaseIdList :: Lens' DescribeCases [Text]
dcCaseIdList = lens _dcCaseIdList (\ s a -> s{_dcCaseIdList = a}) . _Default . _Coerce

-- | The start date for a filtered date search on support case communications. Case communications are available for 12 months after creation.
dcAfterTime :: Lens' DescribeCases (Maybe Text)
dcAfterTime = lens _dcAfterTime (\ s a -> s{_dcAfterTime = a})

-- | The end date for a filtered date search on support case communications. Case communications are available for 12 months after creation.
dcBeforeTime :: Lens' DescribeCases (Maybe Text)
dcBeforeTime = lens _dcBeforeTime (\ s a -> s{_dcBeforeTime = a})

-- | A resumption point for pagination.
dcNextToken :: Lens' DescribeCases (Maybe Text)
dcNextToken = lens _dcNextToken (\ s a -> s{_dcNextToken = a})

-- | Specifies whether communications should be included in the 'DescribeCases' results. The default is /true/ .
dcIncludeCommunications :: Lens' DescribeCases (Maybe Bool)
dcIncludeCommunications = lens _dcIncludeCommunications (\ s a -> s{_dcIncludeCommunications = a})

-- | The ID displayed for a case in the AWS Support Center user interface.
dcDisplayId :: Lens' DescribeCases (Maybe Text)
dcDisplayId = lens _dcDisplayId (\ s a -> s{_dcDisplayId = a})

-- | The ISO 639-1 code for the language in which AWS provides support. AWS Support currently supports English ("en") and Japanese ("ja"). Language parameters must be passed explicitly for operations that take them.
dcLanguage :: Lens' DescribeCases (Maybe Text)
dcLanguage = lens _dcLanguage (\ s a -> s{_dcLanguage = a})

-- | The maximum number of results to return before paginating.
dcMaxResults :: Lens' DescribeCases (Maybe Natural)
dcMaxResults = lens _dcMaxResults (\ s a -> s{_dcMaxResults = a}) . mapping _Nat

instance AWSPager DescribeCases where
        page rq rs
          | stop (rs ^. drsNextToken) = Nothing
          | stop (rs ^. drsCases) = Nothing
          | otherwise =
            Just $ rq & dcNextToken .~ rs ^. drsNextToken

instance AWSRequest DescribeCases where
        type Rs DescribeCases = DescribeCasesResponse
        request = postJSON support
        response
          = receiveJSON
              (\ s h x ->
                 DescribeCasesResponse' <$>
                   (x .?> "cases" .!@ mempty) <*> (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeCases where

instance NFData DescribeCases where

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
              (catMaybes
                 [("includeResolvedCases" .=) <$>
                    _dcIncludeResolvedCases,
                  ("caseIdList" .=) <$> _dcCaseIdList,
                  ("afterTime" .=) <$> _dcAfterTime,
                  ("beforeTime" .=) <$> _dcBeforeTime,
                  ("nextToken" .=) <$> _dcNextToken,
                  ("includeCommunications" .=) <$>
                    _dcIncludeCommunications,
                  ("displayId" .=) <$> _dcDisplayId,
                  ("language" .=) <$> _dcLanguage,
                  ("maxResults" .=) <$> _dcMaxResults])

instance ToPath DescribeCases where
        toPath = const "/"

instance ToQuery DescribeCases where
        toQuery = const mempty

-- | Returns an array of 'CaseDetails' objects and a @nextToken@ that defines a point for pagination in the result set.
--
--
--
-- /See:/ 'describeCasesResponse' smart constructor.
data DescribeCasesResponse = DescribeCasesResponse'
  { _drsCases          :: !(Maybe [CaseDetails])
  , _drsNextToken      :: !(Maybe Text)
  , _drsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeCasesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsCases' - The details for the cases that match the request.
--
-- * 'drsNextToken' - A resumption point for pagination.
--
-- * 'drsResponseStatus' - -- | The response status code.
describeCasesResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DescribeCasesResponse
describeCasesResponse pResponseStatus_ =
  DescribeCasesResponse'
    { _drsCases = Nothing
    , _drsNextToken = Nothing
    , _drsResponseStatus = pResponseStatus_
    }


-- | The details for the cases that match the request.
drsCases :: Lens' DescribeCasesResponse [CaseDetails]
drsCases = lens _drsCases (\ s a -> s{_drsCases = a}) . _Default . _Coerce

-- | A resumption point for pagination.
drsNextToken :: Lens' DescribeCasesResponse (Maybe Text)
drsNextToken = lens _drsNextToken (\ s a -> s{_drsNextToken = a})

-- | -- | The response status code.
drsResponseStatus :: Lens' DescribeCasesResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DescribeCasesResponse where
