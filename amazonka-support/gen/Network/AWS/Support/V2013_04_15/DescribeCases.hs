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
dcrAfterTime
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeCases
    -> f DescribeCases
dcrAfterTime f x =
    (\y -> x { _dcrAfterTime = y })
       <$> f (_dcrAfterTime x)
{-# INLINE dcrAfterTime #-}

-- | The end date for a filtered date search on support case communications.
-- Case communications are available for 12 months after creation.
dcrBeforeTime
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeCases
    -> f DescribeCases
dcrBeforeTime f x =
    (\y -> x { _dcrBeforeTime = y })
       <$> f (_dcrBeforeTime x)
{-# INLINE dcrBeforeTime #-}

-- | A list of ID numbers of the support cases you want returned. The maximum
-- number of cases is 100.
dcrCaseIdList
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> DescribeCases
    -> f DescribeCases
dcrCaseIdList f x =
    (\y -> x { _dcrCaseIdList = y })
       <$> f (_dcrCaseIdList x)
{-# INLINE dcrCaseIdList #-}

-- | The ID displayed for a case in the AWS Support Center user interface.
dcrDisplayId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeCases
    -> f DescribeCases
dcrDisplayId f x =
    (\y -> x { _dcrDisplayId = y })
       <$> f (_dcrDisplayId x)
{-# INLINE dcrDisplayId #-}

-- | Specifies whether communications should be included in the DescribeCases
-- results. The default is true.
dcrIncludeCommunications
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> DescribeCases
    -> f DescribeCases
dcrIncludeCommunications f x =
    (\y -> x { _dcrIncludeCommunications = y })
       <$> f (_dcrIncludeCommunications x)
{-# INLINE dcrIncludeCommunications #-}

-- | Specifies whether resolved support cases should be included in the
-- DescribeCases results. The default is false.
dcrIncludeResolvedCases
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> DescribeCases
    -> f DescribeCases
dcrIncludeResolvedCases f x =
    (\y -> x { _dcrIncludeResolvedCases = y })
       <$> f (_dcrIncludeResolvedCases x)
{-# INLINE dcrIncludeResolvedCases #-}

-- | The ISO 639-1 code for the language in which AWS provides support. AWS
-- Support currently supports English ("en") and Japanese ("ja"). Language
-- parameters must be passed explicitly for operations that take them.
dcrLanguage
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeCases
    -> f DescribeCases
dcrLanguage f x =
    (\y -> x { _dcrLanguage = y })
       <$> f (_dcrLanguage x)
{-# INLINE dcrLanguage #-}

-- | The maximum number of results to return before paginating.
dcrMaxResults
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DescribeCases
    -> f DescribeCases
dcrMaxResults f x =
    (\y -> x { _dcrMaxResults = y })
       <$> f (_dcrMaxResults x)
{-# INLINE dcrMaxResults #-}

-- | A resumption point for pagination.
dcrNextToken
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeCases
    -> f DescribeCases
dcrNextToken f x =
    (\y -> x { _dcrNextToken = y })
       <$> f (_dcrNextToken x)
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
dcsCases
    :: Functor f
    => ([CaseDetails]
    -> f ([CaseDetails]))
    -> DescribeCasesResponse
    -> f DescribeCasesResponse
dcsCases f x =
    (\y -> x { _dcsCases = y })
       <$> f (_dcsCases x)
{-# INLINE dcsCases #-}

-- | A resumption point for pagination.
dcsNextToken
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeCasesResponse
    -> f DescribeCasesResponse
dcsNextToken f x =
    (\y -> x { _dcsNextToken = y })
       <$> f (_dcsNextToken x)
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
