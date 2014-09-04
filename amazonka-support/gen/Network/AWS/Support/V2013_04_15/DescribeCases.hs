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
    , describeCases
    -- ** Request lenses
    , dcrAfterTime
    , dcrBeforeTime
    , dcrCaseIdList
    , dcrDisplayId
    , dcrIncludeCommunications
    , dcrIncludeResolvedCases
    , dcrLanguage
    , dcrMaxResults
    , dcrNextToken

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

-- | Minimum specification for a 'DescribeCases' request.
describeCases :: DescribeCases
describeCases = DescribeCases
    { _dcrAfterTime = Nothing
    , _dcrBeforeTime = Nothing
    , _dcrCaseIdList = mempty
    , _dcrDisplayId = Nothing
    , _dcrIncludeCommunications = Nothing
    , _dcrIncludeResolvedCases = Nothing
    , _dcrLanguage = Nothing
    , _dcrMaxResults = Nothing
    , _dcrNextToken = Nothing
    }
{-# INLINE describeCases #-}

data DescribeCases = DescribeCases
    { _dcrAfterTime :: Maybe Text
      -- ^ The start date for a filtered date search on support case
      -- communications. Case communications are available for 12 months
      -- after creation.
    , _dcrBeforeTime :: Maybe Text
      -- ^ The end date for a filtered date search on support case
      -- communications. Case communications are available for 12 months
      -- after creation.
    , _dcrCaseIdList :: [Text]
      -- ^ A list of ID numbers of the support cases you want returned. The
      -- maximum number of cases is 100.
    , _dcrDisplayId :: Maybe Text
      -- ^ The ID displayed for a case in the AWS Support Center user
      -- interface.
    , _dcrIncludeCommunications :: Maybe Bool
      -- ^ Specifies whether communications should be included in the
      -- DescribeCases results. The default is true.
    , _dcrIncludeResolvedCases :: Maybe Bool
      -- ^ Specifies whether resolved support cases should be included in
      -- the DescribeCases results. The default is false.
    , _dcrLanguage :: Maybe Text
      -- ^ The ISO 639-1 code for the language in which AWS provides
      -- support. AWS Support currently supports English ("en") and
      -- Japanese ("ja"). Language parameters must be passed explicitly
      -- for operations that take them.
    , _dcrMaxResults :: Maybe Integer
      -- ^ The maximum number of results to return before paginating.
    , _dcrNextToken :: Maybe Text
      -- ^ A resumption point for pagination.
    } deriving (Show, Generic)

-- | The start date for a filtered date search on support case communications.
-- Case communications are available for 12 months after creation.
dcrAfterTime :: Lens' DescribeCases (Maybe Text)
dcrAfterTime f x =
    f (_dcrAfterTime x)
        <&> \y -> x { _dcrAfterTime = y }
{-# INLINE dcrAfterTime #-}

-- | The end date for a filtered date search on support case communications.
-- Case communications are available for 12 months after creation.
dcrBeforeTime :: Lens' DescribeCases (Maybe Text)
dcrBeforeTime f x =
    f (_dcrBeforeTime x)
        <&> \y -> x { _dcrBeforeTime = y }
{-# INLINE dcrBeforeTime #-}

-- | A list of ID numbers of the support cases you want returned. The maximum
-- number of cases is 100.
dcrCaseIdList :: Lens' DescribeCases ([Text])
dcrCaseIdList f x =
    f (_dcrCaseIdList x)
        <&> \y -> x { _dcrCaseIdList = y }
{-# INLINE dcrCaseIdList #-}

-- | The ID displayed for a case in the AWS Support Center user interface.
dcrDisplayId :: Lens' DescribeCases (Maybe Text)
dcrDisplayId f x =
    f (_dcrDisplayId x)
        <&> \y -> x { _dcrDisplayId = y }
{-# INLINE dcrDisplayId #-}

-- | Specifies whether communications should be included in the DescribeCases
-- results. The default is true.
dcrIncludeCommunications :: Lens' DescribeCases (Maybe Bool)
dcrIncludeCommunications f x =
    f (_dcrIncludeCommunications x)
        <&> \y -> x { _dcrIncludeCommunications = y }
{-# INLINE dcrIncludeCommunications #-}

-- | Specifies whether resolved support cases should be included in the
-- DescribeCases results. The default is false.
dcrIncludeResolvedCases :: Lens' DescribeCases (Maybe Bool)
dcrIncludeResolvedCases f x =
    f (_dcrIncludeResolvedCases x)
        <&> \y -> x { _dcrIncludeResolvedCases = y }
{-# INLINE dcrIncludeResolvedCases #-}

-- | The ISO 639-1 code for the language in which AWS provides support. AWS
-- Support currently supports English ("en") and Japanese ("ja"). Language
-- parameters must be passed explicitly for operations that take them.
dcrLanguage :: Lens' DescribeCases (Maybe Text)
dcrLanguage f x =
    f (_dcrLanguage x)
        <&> \y -> x { _dcrLanguage = y }
{-# INLINE dcrLanguage #-}

-- | The maximum number of results to return before paginating.
dcrMaxResults :: Lens' DescribeCases (Maybe Integer)
dcrMaxResults f x =
    f (_dcrMaxResults x)
        <&> \y -> x { _dcrMaxResults = y }
{-# INLINE dcrMaxResults #-}

-- | A resumption point for pagination.
dcrNextToken :: Lens' DescribeCases (Maybe Text)
dcrNextToken f x =
    f (_dcrNextToken x)
        <&> \y -> x { _dcrNextToken = y }
{-# INLINE dcrNextToken #-}

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
dcsCases f x =
    f (_dcsCases x)
        <&> \y -> x { _dcsCases = y }
{-# INLINE dcsCases #-}

-- | A resumption point for pagination.
dcsNextToken :: Lens' DescribeCasesResponse (Maybe Text)
dcsNextToken f x =
    f (_dcsNextToken x)
        <&> \y -> x { _dcsNextToken = y }
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
