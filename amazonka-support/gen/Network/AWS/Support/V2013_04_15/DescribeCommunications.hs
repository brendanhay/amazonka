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
    , mkDescribeCommunications
    -- ** Request lenses
    , dc1CaseId
    , dc1BeforeTime
    , dc1AfterTime
    , dc1NextToken
    , dc1MaxResults

    -- * Response
    , DescribeCommunicationsResponse
    -- ** Response lenses
    , dcrsrsCommunications
    , dcrsrsNextToken
    ) where

import           Network.AWS.Support.V2013_04_15.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | 
data DescribeCommunications = DescribeCommunications
    { _dc1CaseId :: Text
    , _dc1BeforeTime :: Maybe Text
    , _dc1AfterTime :: Maybe Text
    , _dc1NextToken :: Maybe Text
    , _dc1MaxResults :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeCommunications' request.
mkDescribeCommunications :: Text -- ^ 'dc1CaseId'
                         -> DescribeCommunications
mkDescribeCommunications p1 = DescribeCommunications
    { _dc1CaseId = p1
    , _dc1BeforeTime = Nothing
    , _dc1AfterTime = Nothing
    , _dc1NextToken = Nothing
    , _dc1MaxResults = Nothing
    }

-- | The AWS Support case ID requested or returned in the call. The case ID is
-- an alphanumeric string formatted as shown in this example:
-- case-12345678910-2013-c4c1d2bf33c5cf47.
dc1CaseId :: Lens' DescribeCommunications Text
dc1CaseId = lens _dc1CaseId (\s a -> s { _dc1CaseId = a })

-- | The end date for a filtered date search on support case communications.
-- Case communications are available for 12 months after creation.
dc1BeforeTime :: Lens' DescribeCommunications (Maybe Text)
dc1BeforeTime = lens _dc1BeforeTime (\s a -> s { _dc1BeforeTime = a })

-- | The start date for a filtered date search on support case communications.
-- Case communications are available for 12 months after creation.
dc1AfterTime :: Lens' DescribeCommunications (Maybe Text)
dc1AfterTime = lens _dc1AfterTime (\s a -> s { _dc1AfterTime = a })

-- | A resumption point for pagination.
dc1NextToken :: Lens' DescribeCommunications (Maybe Text)
dc1NextToken = lens _dc1NextToken (\s a -> s { _dc1NextToken = a })

-- | The maximum number of results to return before paginating.
dc1MaxResults :: Lens' DescribeCommunications (Maybe Integer)
dc1MaxResults = lens _dc1MaxResults (\s a -> s { _dc1MaxResults = a })

instance ToPath DescribeCommunications

instance ToQuery DescribeCommunications

instance ToHeaders DescribeCommunications

instance ToJSON DescribeCommunications

-- | The communications returned by the DescribeCommunications operation.
data DescribeCommunicationsResponse = DescribeCommunicationsResponse
    { _dcrsrsCommunications :: [Communication]
    , _dcrsrsNextToken :: Maybe Text
    } deriving (Show, Generic)

-- | The communications for the case.
dcrsrsCommunications :: Lens' DescribeCommunicationsResponse [Communication]
dcrsrsCommunications =
    lens _dcrsrsCommunications (\s a -> s { _dcrsrsCommunications = a })

-- | A resumption point for pagination.
dcrsrsNextToken :: Lens' DescribeCommunicationsResponse (Maybe Text)
dcrsrsNextToken = lens _dcrsrsNextToken (\s a -> s { _dcrsrsNextToken = a })

instance FromJSON DescribeCommunicationsResponse

instance AWSRequest DescribeCommunications where
    type Sv DescribeCommunications = Support
    type Rs DescribeCommunications = DescribeCommunicationsResponse

    request = get
    response _ = jsonResponse

instance AWSPager DescribeCommunications where
    next rq rs = (\x -> rq & dc1NextToken ?~ x) <$> (rs ^. dcrsrsNextToken)

