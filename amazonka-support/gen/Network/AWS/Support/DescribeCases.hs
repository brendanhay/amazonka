{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Support.DescribeCases
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of cases that you specify by passing one or more case IDs.
-- In addition, you can filter the cases by date by setting values for the
-- @AfterTime@ and @BeforeTime@ request parameters. Case data is available for
-- 12 months after creation. If a case was created more than 12 months ago, a
-- request for data might cause an error. The response returns the following
-- in JSON format: One or more 'CaseDetails' data types. One or more
-- @NextToken@ values, which specify where to paginate the returned records
-- represented by the @CaseDetails@ objects.
--
-- <http://docs.aws.amazon.com/awssupport/latest/APIReference/API_DescribeCases.html>
module Network.AWS.Support.DescribeCases
    (
    -- * Request
      DescribeCases
    -- ** Request constructor
    , describeCases
    -- ** Request lenses
    , dcAfterTime
    , dcBeforeTime
    , dcCaseIdList
    , dcDisplayId
    , dcIncludeCommunications
    , dcIncludeResolvedCases
    , dcLanguage
    , dcMaxResults
    , dcNextToken

    -- * Response
    , DescribeCasesResponse
    -- ** Response constructor
    , describeCasesResponse
    -- ** Response lenses
    , dcr1Cases
    , dcr1NextToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.Support.Types
import qualified GHC.Exts

data DescribeCases = DescribeCases
    { _dcAfterTime             :: Maybe Text
    , _dcBeforeTime            :: Maybe Text
    , _dcCaseIdList            :: List "caseIdList" Text
    , _dcDisplayId             :: Maybe Text
    , _dcIncludeCommunications :: Maybe Bool
    , _dcIncludeResolvedCases  :: Maybe Bool
    , _dcLanguage              :: Maybe Text
    , _dcMaxResults            :: Maybe Nat
    , _dcNextToken             :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'DescribeCases' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcAfterTime' @::@ 'Maybe' 'Text'
--
-- * 'dcBeforeTime' @::@ 'Maybe' 'Text'
--
-- * 'dcCaseIdList' @::@ ['Text']
--
-- * 'dcDisplayId' @::@ 'Maybe' 'Text'
--
-- * 'dcIncludeCommunications' @::@ 'Maybe' 'Bool'
--
-- * 'dcIncludeResolvedCases' @::@ 'Maybe' 'Bool'
--
-- * 'dcLanguage' @::@ 'Maybe' 'Text'
--
-- * 'dcMaxResults' @::@ 'Maybe' 'Natural'
--
-- * 'dcNextToken' @::@ 'Maybe' 'Text'
--
describeCases :: DescribeCases
describeCases = DescribeCases
    { _dcCaseIdList            = mempty
    , _dcDisplayId             = Nothing
    , _dcAfterTime             = Nothing
    , _dcBeforeTime            = Nothing
    , _dcIncludeResolvedCases  = Nothing
    , _dcNextToken             = Nothing
    , _dcMaxResults            = Nothing
    , _dcLanguage              = Nothing
    , _dcIncludeCommunications = Nothing
    }

-- | The start date for a filtered date search on support case communications.
-- Case communications are available for 12 months after creation.
dcAfterTime :: Lens' DescribeCases (Maybe Text)
dcAfterTime = lens _dcAfterTime (\s a -> s { _dcAfterTime = a })

-- | The end date for a filtered date search on support case communications.
-- Case communications are available for 12 months after creation.
dcBeforeTime :: Lens' DescribeCases (Maybe Text)
dcBeforeTime = lens _dcBeforeTime (\s a -> s { _dcBeforeTime = a })

-- | A list of ID numbers of the support cases you want returned. The maximum
-- number of cases is 100.
dcCaseIdList :: Lens' DescribeCases [Text]
dcCaseIdList = lens _dcCaseIdList (\s a -> s { _dcCaseIdList = a }) . _List

-- | The ID displayed for a case in the AWS Support Center user interface.
dcDisplayId :: Lens' DescribeCases (Maybe Text)
dcDisplayId = lens _dcDisplayId (\s a -> s { _dcDisplayId = a })

-- | Specifies whether communications should be included in the
-- 'DescribeCases' results. The default is /true/.
dcIncludeCommunications :: Lens' DescribeCases (Maybe Bool)
dcIncludeCommunications =
    lens _dcIncludeCommunications (\s a -> s { _dcIncludeCommunications = a })

-- | Specifies whether resolved support cases should be included in the
-- 'DescribeCases' results. The default is /false/.
dcIncludeResolvedCases :: Lens' DescribeCases (Maybe Bool)
dcIncludeResolvedCases =
    lens _dcIncludeResolvedCases (\s a -> s { _dcIncludeResolvedCases = a })

-- | The ISO 639-1 code for the language in which AWS provides support. AWS
-- Support currently supports English ("en") and Japanese ("ja"). Language
-- parameters must be passed explicitly for operations that take them.
dcLanguage :: Lens' DescribeCases (Maybe Text)
dcLanguage = lens _dcLanguage (\s a -> s { _dcLanguage = a })

-- | The maximum number of results to return before paginating.
dcMaxResults :: Lens' DescribeCases (Maybe Natural)
dcMaxResults = lens _dcMaxResults (\s a -> s { _dcMaxResults = a }) . mapping _Nat

-- | A resumption point for pagination.
dcNextToken :: Lens' DescribeCases (Maybe Text)
dcNextToken = lens _dcNextToken (\s a -> s { _dcNextToken = a })

data DescribeCasesResponse = DescribeCasesResponse
    { _dcr1Cases     :: List "cases" CaseDetails
    , _dcr1NextToken :: Maybe Text
    } deriving (Eq, Show)

-- | 'DescribeCasesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcr1Cases' @::@ ['CaseDetails']
--
-- * 'dcr1NextToken' @::@ 'Maybe' 'Text'
--
describeCasesResponse :: DescribeCasesResponse
describeCasesResponse = DescribeCasesResponse
    { _dcr1Cases     = mempty
    , _dcr1NextToken = Nothing
    }

-- | The details for the cases that match the request.
dcr1Cases :: Lens' DescribeCasesResponse [CaseDetails]
dcr1Cases = lens _dcr1Cases (\s a -> s { _dcr1Cases = a }) . _List

-- | A resumption point for pagination.
dcr1NextToken :: Lens' DescribeCasesResponse (Maybe Text)
dcr1NextToken = lens _dcr1NextToken (\s a -> s { _dcr1NextToken = a })

instance ToPath DescribeCases where
    toPath = const "/"

instance ToQuery DescribeCases where
    toQuery = const mempty

instance ToHeaders DescribeCases

instance ToJSON DescribeCases where
    toJSON DescribeCases{..} = object
        [ "caseIdList"            .= _dcCaseIdList
        , "displayId"             .= _dcDisplayId
        , "afterTime"             .= _dcAfterTime
        , "beforeTime"            .= _dcBeforeTime
        , "includeResolvedCases"  .= _dcIncludeResolvedCases
        , "nextToken"             .= _dcNextToken
        , "maxResults"            .= _dcMaxResults
        , "language"              .= _dcLanguage
        , "includeCommunications" .= _dcIncludeCommunications
        ]

instance AWSRequest DescribeCases where
    type Sv DescribeCases = Support
    type Rs DescribeCases = DescribeCasesResponse

    request  = post "DescribeCases"
    response = jsonResponse

instance FromJSON DescribeCasesResponse where
    parseJSON = withObject "DescribeCasesResponse" $ \o -> DescribeCasesResponse
        <$> o .:  "cases"
        <*> o .:? "nextToken"

instance AWSPager DescribeCases where
    page rq rs
        | stop (rq ^. dcNextToken) = Nothing
        | otherwise = (\x -> rq & dcNextToken ?~ x)
            <$> (rs ^. dcr1NextToken)
