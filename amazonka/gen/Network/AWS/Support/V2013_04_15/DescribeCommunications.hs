{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
module Network.AWS.Support.V2013_04_15.DescribeCommunications where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.Support.V2013_04_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeCommunications' request.
describeCommunications :: Text -- ^ '_dctCaseId'
                       -> DescribeCommunications
describeCommunications p1 = DescribeCommunications
    { _dctCaseId = p1
    , _dctAfterTime = Nothing
    , _dctBeforeTime = Nothing
    , _dctMaxResults = Nothing
    , _dctNextToken = Nothing
    }

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

makeLenses ''DescribeCommunications

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

makeLenses ''DescribeCommunicationsResponse

instance FromJSON DescribeCommunicationsResponse

instance AWSRequest DescribeCommunications where
    type Sv DescribeCommunications = Support
    type Rs DescribeCommunications = DescribeCommunicationsResponse

    request = get
    response _ = jsonResponse

instance AWSPager DescribeCommunications where
    next rq rs = (\x -> rq { _dctNextToken = Just x })
        <$> (_dcuNextToken rs)
