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
    , mkDescribeCasesRequest
    -- ** Request lenses
    , dcrCaseIdList
    , dcrDisplayId
    , dcrAfterTime
    , dcrBeforeTime
    , dcrIncludeResolvedCases
    , dcrNextToken
    , dcrMaxResults
    , dcrLanguage
    , dcrIncludeCommunications

    -- * Response
    , DescribeCasesResponse
    -- ** Response lenses
    , dcsCases
    , dcsNextToken
    ) where

import           Network.AWS.Support.V2013_04_15.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeCases' request.
mkDescribeCasesRequest :: DescribeCases
mkDescribeCasesRequest = DescribeCases
    { _dcrCaseIdList = mempty
    , _dcrDisplayId = Nothing
    , _dcrAfterTime = Nothing
    , _dcrBeforeTime = Nothing
    , _dcrIncludeResolvedCases = Nothing
    , _dcrNextToken = Nothing
    , _dcrMaxResults = Nothing
    , _dcrLanguage = Nothing
    , _dcrIncludeCommunications = Nothing
    }
{-# INLINE mkDescribeCasesRequest #-}

data DescribeCases = DescribeCases
    { _dcrCaseIdList :: [Text]
      -- ^ A list of ID numbers of the support cases you want returned. The
      -- maximum number of cases is 100.
    , _dcrDisplayId :: Maybe Text
      -- ^ The ID displayed for a case in the AWS Support Center user
      -- interface.
    , _dcrAfterTime :: Maybe Text
      -- ^ The start date for a filtered date search on support case
      -- communications. Case communications are available for 12 months
      -- after creation.
    , _dcrBeforeTime :: Maybe Text
      -- ^ The end date for a filtered date search on support case
      -- communications. Case communications are available for 12 months
      -- after creation.
    , _dcrIncludeResolvedCases :: Maybe Bool
      -- ^ Specifies whether resolved support cases should be included in
      -- the DescribeCases results. The default is false.
    , _dcrNextToken :: Maybe Text
      -- ^ A resumption point for pagination.
    , _dcrMaxResults :: Maybe Integer
      -- ^ The maximum number of results to return before paginating.
    , _dcrLanguage :: Maybe Text
      -- ^ The ISO 639-1 code for the language in which AWS provides
      -- support. AWS Support currently supports English ("en") and
      -- Japanese ("ja"). Language parameters must be passed explicitly
      -- for operations that take them.
    , _dcrIncludeCommunications :: Maybe Bool
      -- ^ Specifies whether communications should be included in the
      -- DescribeCases results. The default is true.
    } deriving (Show, Generic)

-- | A list of ID numbers of the support cases you want returned. The maximum
-- number of cases is 100.
dcrCaseIdList :: Lens' DescribeCases ([Text])
dcrCaseIdList = lens _dcrCaseIdList (\s a -> s { _dcrCaseIdList = a })
{-# INLINE dcrCaseIdList #-}

-- | The ID displayed for a case in the AWS Support Center user interface.
dcrDisplayId :: Lens' DescribeCases (Maybe Text)
dcrDisplayId = lens _dcrDisplayId (\s a -> s { _dcrDisplayId = a })
{-# INLINE dcrDisplayId #-}

-- | The start date for a filtered date search on support case communications.
-- Case communications are available for 12 months after creation.
dcrAfterTime :: Lens' DescribeCases (Maybe Text)
dcrAfterTime = lens _dcrAfterTime (\s a -> s { _dcrAfterTime = a })
{-# INLINE dcrAfterTime #-}

-- | The end date for a filtered date search on support case communications.
-- Case communications are available for 12 months after creation.
dcrBeforeTime :: Lens' DescribeCases (Maybe Text)
dcrBeforeTime = lens _dcrBeforeTime (\s a -> s { _dcrBeforeTime = a })
{-# INLINE dcrBeforeTime #-}

-- | Specifies whether resolved support cases should be included in the
-- DescribeCases results. The default is false.
dcrIncludeResolvedCases :: Lens' DescribeCases (Maybe Bool)
dcrIncludeResolvedCases = lens _dcrIncludeResolvedCases (\s a -> s { _dcrIncludeResolvedCases = a })
{-# INLINE dcrIncludeResolvedCases #-}

-- | A resumption point for pagination.
dcrNextToken :: Lens' DescribeCases (Maybe Text)
dcrNextToken = lens _dcrNextToken (\s a -> s { _dcrNextToken = a })
{-# INLINE dcrNextToken #-}

-- | The maximum number of results to return before paginating.
dcrMaxResults :: Lens' DescribeCases (Maybe Integer)
dcrMaxResults = lens _dcrMaxResults (\s a -> s { _dcrMaxResults = a })
{-# INLINE dcrMaxResults #-}

-- | The ISO 639-1 code for the language in which AWS provides support. AWS
-- Support currently supports English ("en") and Japanese ("ja"). Language
-- parameters must be passed explicitly for operations that take them.
dcrLanguage :: Lens' DescribeCases (Maybe Text)
dcrLanguage = lens _dcrLanguage (\s a -> s { _dcrLanguage = a })
{-# INLINE dcrLanguage #-}

-- | Specifies whether communications should be included in the DescribeCases
-- results. The default is true.
dcrIncludeCommunications :: Lens' DescribeCases (Maybe Bool)
dcrIncludeCommunications = lens _dcrIncludeCommunications (\s a -> s { _dcrIncludeCommunications = a })
{-# INLINE dcrIncludeCommunications #-}

instance ToPath DescribeCases

instance ToQuery DescribeCases

instance ToHeaders DescribeCases

instance ToJSON DescribeCases

data DescribeCasesResponse = DescribeCasesResponse
    { _dcsCases :: [CaseDetails]
      -- ^ The details for the cases that match the request.
    , _dcsNextToken :: Maybe Text
      -- ^ A resumption point for pagination.
    } deriving (Show, Generic)

-- | The details for the cases that match the request.
dcsCases :: Lens' DescribeCasesResponse ([CaseDetails])
dcsCases = lens _dcsCases (\s a -> s { _dcsCases = a })
{-# INLINE dcsCases #-}

-- | A resumption point for pagination.
dcsNextToken :: Lens' DescribeCasesResponse (Maybe Text)
dcsNextToken = lens _dcsNextToken (\s a -> s { _dcsNextToken = a })
{-# INLINE dcsNextToken #-}

instance FromJSON DescribeCasesResponse

instance AWSRequest DescribeCases where
    type Sv DescribeCases = Support
    type Rs DescribeCases = DescribeCasesResponse

    request = get
    response _ = jsonResponse

instance AWSPager DescribeCases where
    next rq rs = (\x -> rq { _dcrNextToken = Just x })
        <$> (_dcsNextToken rs)
