{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.EMR.DescribeJobFlows
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This API is deprecated and will eventually be removed. We recommend you use
-- ListClusters, DescribeCluster, ListSteps, ListInstanceGroups and
-- ListBootstrapActions instead. DescribeJobFlows returns a list of job flows
-- that match all of the supplied parameters. The parameters can include a
-- list of job flow IDs, job flow states, and restrictions on job flow
-- creation date and time. Regardless of supplied parameters, only job flows
-- created within the last two months are returned. If no parameters are
-- supplied, then job flows matching either of the following criteria are
-- returned: Job flows created and completed in the last two weeks Job flows
-- created within the last two months that are in one of the following states:
-- RUNNING, WAITING, SHUTTING_DOWN, STARTING Amazon Elastic MapReduce can
-- return a maximum of 512 job flow descriptions.
module Network.AWS.EMR.DescribeJobFlows
    (
    -- * Request
      DescribeJobFlows
    -- ** Request constructor
    , describeJobFlows
    -- ** Request lenses
    , djfCreatedAfter
    , djfCreatedBefore
    , djfJobFlowIds
    , djfJobFlowStates

    -- * Response
    , DescribeJobFlowsResponse
    -- ** Response constructor
    , describeJobFlowsResponse
    -- ** Response lenses
    , djfrJobFlows
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.EMR.Types

data DescribeJobFlows = DescribeJobFlows
    { _djfCreatedAfter  :: Maybe RFC822
    , _djfCreatedBefore :: Maybe RFC822
    , _djfJobFlowIds    :: [Text]
    , _djfJobFlowStates :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeJobFlows' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'djfCreatedAfter' @::@ 'Maybe' 'UTCTime'
--
-- * 'djfCreatedBefore' @::@ 'Maybe' 'UTCTime'
--
-- * 'djfJobFlowIds' @::@ ['Text']
--
-- * 'djfJobFlowStates' @::@ ['Text']
--
describeJobFlows :: DescribeJobFlows
describeJobFlows = DescribeJobFlows
    { _djfCreatedAfter  = Nothing
    , _djfCreatedBefore = Nothing
    , _djfJobFlowIds    = mempty
    , _djfJobFlowStates = mempty
    }

-- | Return only job flows created after this date and time.
djfCreatedAfter :: Lens' DescribeJobFlows (Maybe UTCTime)
djfCreatedAfter = lens _djfCreatedAfter (\s a -> s { _djfCreatedAfter = a })
    . mapping _Time

-- | Return only job flows created before this date and time.
djfCreatedBefore :: Lens' DescribeJobFlows (Maybe UTCTime)
djfCreatedBefore = lens _djfCreatedBefore (\s a -> s { _djfCreatedBefore = a })
    . mapping _Time

-- | Return only job flows whose job flow ID is contained in this list.
djfJobFlowIds :: Lens' DescribeJobFlows [Text]
djfJobFlowIds = lens _djfJobFlowIds (\s a -> s { _djfJobFlowIds = a })

-- | Return only job flows whose state is contained in this list.
djfJobFlowStates :: Lens' DescribeJobFlows [Text]
djfJobFlowStates = lens _djfJobFlowStates (\s a -> s { _djfJobFlowStates = a })

instance ToPath DescribeJobFlows where
    toPath = const "/"

instance ToQuery DescribeJobFlows where
    toQuery = const mempty

instance ToHeaders DescribeJobFlows

instance ToBody DescribeJobFlows where
    toBody = toBody . encode . _djfCreatedAfter

newtype DescribeJobFlowsResponse = DescribeJobFlowsResponse
    { _djfrJobFlows :: [JobFlowDetail]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeJobFlowsResponse where
    type Item DescribeJobFlowsResponse = JobFlowDetail

    fromList = DescribeJobFlowsResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _djfrJobFlows

-- | 'DescribeJobFlowsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'djfrJobFlows' @::@ ['JobFlowDetail']
--
describeJobFlowsResponse :: DescribeJobFlowsResponse
describeJobFlowsResponse = DescribeJobFlowsResponse
    { _djfrJobFlows = mempty
    }

-- | A list of job flows matching the parameters supplied.
djfrJobFlows :: Lens' DescribeJobFlowsResponse [JobFlowDetail]
djfrJobFlows = lens _djfrJobFlows (\s a -> s { _djfrJobFlows = a })

instance AWSRequest DescribeJobFlows where
    type Sv DescribeJobFlows = EMR
    type Rs DescribeJobFlows = DescribeJobFlowsResponse

    request  = post
    response = jsonResponse $ \h o -> DescribeJobFlowsResponse
        <$> o .: "JobFlows"
