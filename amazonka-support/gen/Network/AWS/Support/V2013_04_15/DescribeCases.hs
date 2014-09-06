{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Support.V2013_04_15.DescribeCases
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
-- AfterTime and BeforeTime request parameters. Case data is available for 12
-- months after creation. If a case was created more than 12 months ago, a
-- request for data might cause an error. The response returns the following
-- in JSON format: One or more CaseDetails data types. One or more NextToken
-- values, which specify where to paginate the returned records represented by
-- the CaseDetails objects.
module Network.AWS.Support.V2013_04_15.DescribeCases
    (
    -- * Request
      DescribeCases
    -- ** Request constructor
    , mkDescribeCases
    -- ** Request lenses
    , dcCaseIdList
    , dcDisplayId
    , dcAfterTime
    , dcBeforeTime
    , dcIncludeResolvedCases
    , dcNextToken
    , dcMaxResults
    , dcLanguage
    , dcIncludeCommunications

    -- * Response
    , DescribeCasesResponse
    -- ** Response lenses
    , dcrsCases
    , dcrsNextToken
    ) where

import           Network.AWS.Support.V2013_04_15.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | 
data DescribeCases = DescribeCases
    { _dcCaseIdList :: [Text]
    , _dcDisplayId :: Maybe Text
    , _dcAfterTime :: Maybe Text
    , _dcBeforeTime :: Maybe Text
    , _dcIncludeResolvedCases :: Maybe Bool
    , _dcNextToken :: Maybe Text
    , _dcMaxResults :: Maybe Integer
    , _dcLanguage :: Maybe Text
    , _dcIncludeCommunications :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeCases' request.
mkDescribeCases :: DescribeCases
mkDescribeCases = DescribeCases
    { _dcCaseIdList = mempty
    , _dcDisplayId = Nothing
    , _dcAfterTime = Nothing
    , _dcBeforeTime = Nothing
    , _dcIncludeResolvedCases = Nothing
    , _dcNextToken = Nothing
    , _dcMaxResults = Nothing
    , _dcLanguage = Nothing
    , _dcIncludeCommunications = Nothing
    }
{-# INLINE mkDescribeCases #-}

-- | A list of ID numbers of the support cases you want returned. The maximum
-- number of cases is 100.
dcCaseIdList :: Lens' DescribeCases [Text]
dcCaseIdList = lens _dcCaseIdList (\s a -> s { _dcCaseIdList = a })
{-# INLINE dcCaseIdList #-}

-- | The ID displayed for a case in the AWS Support Center user interface.
dcDisplayId :: Lens' DescribeCases (Maybe Text)
dcDisplayId = lens _dcDisplayId (\s a -> s { _dcDisplayId = a })
{-# INLINE dcDisplayId #-}

-- | The start date for a filtered date search on support case communications.
-- Case communications are available for 12 months after creation.
dcAfterTime :: Lens' DescribeCases (Maybe Text)
dcAfterTime = lens _dcAfterTime (\s a -> s { _dcAfterTime = a })
{-# INLINE dcAfterTime #-}

-- | The end date for a filtered date search on support case communications.
-- Case communications are available for 12 months after creation.
dcBeforeTime :: Lens' DescribeCases (Maybe Text)
dcBeforeTime = lens _dcBeforeTime (\s a -> s { _dcBeforeTime = a })
{-# INLINE dcBeforeTime #-}

-- | Specifies whether resolved support cases should be included in the
-- DescribeCases results. The default is false.
dcIncludeResolvedCases :: Lens' DescribeCases (Maybe Bool)
dcIncludeResolvedCases =
    lens _dcIncludeResolvedCases (\s a -> s { _dcIncludeResolvedCases = a })
{-# INLINE dcIncludeResolvedCases #-}

-- | A resumption point for pagination.
dcNextToken :: Lens' DescribeCases (Maybe Text)
dcNextToken = lens _dcNextToken (\s a -> s { _dcNextToken = a })
{-# INLINE dcNextToken #-}

-- | The maximum number of results to return before paginating.
dcMaxResults :: Lens' DescribeCases (Maybe Integer)
dcMaxResults = lens _dcMaxResults (\s a -> s { _dcMaxResults = a })
{-# INLINE dcMaxResults #-}

-- | The ISO 639-1 code for the language in which AWS provides support. AWS
-- Support currently supports English ("en") and Japanese ("ja"). Language
-- parameters must be passed explicitly for operations that take them.
dcLanguage :: Lens' DescribeCases (Maybe Text)
dcLanguage = lens _dcLanguage (\s a -> s { _dcLanguage = a })
{-# INLINE dcLanguage #-}

-- | Specifies whether communications should be included in the DescribeCases
-- results. The default is true.
dcIncludeCommunications :: Lens' DescribeCases (Maybe Bool)
dcIncludeCommunications =
    lens _dcIncludeCommunications
         (\s a -> s { _dcIncludeCommunications = a })
{-# INLINE dcIncludeCommunications #-}

instance ToPath DescribeCases

instance ToQuery DescribeCases

instance ToHeaders DescribeCases

instance ToJSON DescribeCases

-- | Returns an array of CaseDetails objects and a NextToken that defines a
-- point for pagination in the result set.
data DescribeCasesResponse = DescribeCasesResponse
    { _dcrsCases :: [CaseDetails]
    , _dcrsNextToken :: Maybe Text
    } deriving (Show, Generic)

-- | The details for the cases that match the request.
dcrsCases :: Lens' DescribeCasesResponse [CaseDetails]
dcrsCases = lens _dcrsCases (\s a -> s { _dcrsCases = a })
{-# INLINE dcrsCases #-}

-- | A resumption point for pagination.
dcrsNextToken :: Lens' DescribeCasesResponse (Maybe Text)
dcrsNextToken = lens _dcrsNextToken (\s a -> s { _dcrsNextToken = a })
{-# INLINE dcrsNextToken #-}

instance FromJSON DescribeCasesResponse

instance AWSRequest DescribeCases where
    type Sv DescribeCases = Support
    type Rs DescribeCases = DescribeCasesResponse

    request = get
    response _ = jsonResponse

instance AWSPager DescribeCases where
    next rq rs = (\x -> rq { _dcNextToken = Just x })
        <$> (_dcrsNextToken rs)
