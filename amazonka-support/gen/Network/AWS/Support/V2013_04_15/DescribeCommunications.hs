{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Support.V2013_04_15.DescribeCommunications
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns communications (and attachments) for one or more support cases. You
-- can use the AfterTime and BeforeTime parameters to filter by date. You can
-- use the CaseId parameter to restrict the results to a particular case. Case
-- data is available for 12 months after creation. If a case was created more
-- than 12 months ago, a request for data might cause an error. You can use
-- the MaxResults and NextToken parameters to control the pagination of the
-- result set. Set MaxResults to the number of cases you want displayed on
-- each page, and use NextToken to specify the resumption of pagination.
module Network.AWS.Support.V2013_04_15.DescribeCommunications
    (
    -- * Request
      DescribeCommunications
    -- ** Request constructor
    , describeCommunications
    -- ** Request lenses
    , dctCaseId
    , dctAfterTime
    , dctBeforeTime
    , dctMaxResults
    , dctNextToken

    -- * Response
    , DescribeCommunicationsResponse
    -- ** Response lenses
    , dcuCommunications
    , dcuNextToken
    ) where

import           Network.AWS.Support.V2013_04_15.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'DescribeCommunications' request.
describeCommunications :: Text -- ^ 'dctCaseId'
                       -> DescribeCommunications
describeCommunications p1 = DescribeCommunications
    { _dctCaseId = p1
    , _dctAfterTime = Nothing
    , _dctBeforeTime = Nothing
    , _dctMaxResults = Nothing
    , _dctNextToken = Nothing
    }
{-# INLINE describeCommunications #-}

data DescribeCommunications = DescribeCommunications
    { _dctCaseId :: Text
      -- ^ The AWS Support case ID requested or returned in the call. The
      -- case ID is an alphanumeric string formatted as shown in this
      -- example: case-12345678910-2013-c4c1d2bf33c5cf47.
    , _dctAfterTime :: Maybe Text
      -- ^ The start date for a filtered date search on support case
      -- communications. Case communications are available for 12 months
      -- after creation.
    , _dctBeforeTime :: Maybe Text
      -- ^ The end date for a filtered date search on support case
      -- communications. Case communications are available for 12 months
      -- after creation.
    , _dctMaxResults :: Maybe Integer
      -- ^ The maximum number of results to return before paginating.
    , _dctNextToken :: Maybe Text
      -- ^ A resumption point for pagination.
    } deriving (Show, Generic)

-- | The AWS Support case ID requested or returned in the call. The case ID is
-- an alphanumeric string formatted as shown in this example:
-- case-12345678910-2013-c4c1d2bf33c5cf47.
dctCaseId :: Lens' DescribeCommunications (Text)
dctCaseId f x =
    f (_dctCaseId x)
        <&> \y -> x { _dctCaseId = y }
{-# INLINE dctCaseId #-}

-- | The start date for a filtered date search on support case communications.
-- Case communications are available for 12 months after creation.
dctAfterTime :: Lens' DescribeCommunications (Maybe Text)
dctAfterTime f x =
    f (_dctAfterTime x)
        <&> \y -> x { _dctAfterTime = y }
{-# INLINE dctAfterTime #-}

-- | The end date for a filtered date search on support case communications.
-- Case communications are available for 12 months after creation.
dctBeforeTime :: Lens' DescribeCommunications (Maybe Text)
dctBeforeTime f x =
    f (_dctBeforeTime x)
        <&> \y -> x { _dctBeforeTime = y }
{-# INLINE dctBeforeTime #-}

-- | The maximum number of results to return before paginating.
dctMaxResults :: Lens' DescribeCommunications (Maybe Integer)
dctMaxResults f x =
    f (_dctMaxResults x)
        <&> \y -> x { _dctMaxResults = y }
{-# INLINE dctMaxResults #-}

-- | A resumption point for pagination.
dctNextToken :: Lens' DescribeCommunications (Maybe Text)
dctNextToken f x =
    f (_dctNextToken x)
        <&> \y -> x { _dctNextToken = y }
{-# INLINE dctNextToken #-}

instance ToPath DescribeCommunications

instance ToQuery DescribeCommunications

instance ToHeaders DescribeCommunications

instance ToJSON DescribeCommunications

data DescribeCommunicationsResponse = DescribeCommunicationsResponse
    { _dcuCommunications :: [Communication]
      -- ^ The communications for the case.
    , _dcuNextToken :: Maybe Text
      -- ^ A resumption point for pagination.
    } deriving (Show, Generic)

-- | The communications for the case.
dcuCommunications :: Lens' DescribeCommunicationsResponse ([Communication])
dcuCommunications f x =
    f (_dcuCommunications x)
        <&> \y -> x { _dcuCommunications = y }
{-# INLINE dcuCommunications #-}

-- | A resumption point for pagination.
dcuNextToken :: Lens' DescribeCommunicationsResponse (Maybe Text)
dcuNextToken f x =
    f (_dcuNextToken x)
        <&> \y -> x { _dcuNextToken = y }
{-# INLINE dcuNextToken #-}

instance FromJSON DescribeCommunicationsResponse

instance AWSRequest DescribeCommunications where
    type Sv DescribeCommunications = Support
    type Rs DescribeCommunications = DescribeCommunicationsResponse

    request = get
    response _ = jsonResponse

instance AWSPager DescribeCommunications where
    next rq rs = (\x -> rq { _dctNextToken = Just x })
        <$> (_dcuNextToken rs)
