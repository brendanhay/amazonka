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

-- Module      : Network.AWS.EMR.DescribeJobFlows
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This API is deprecated and will eventually be removed. We recommend you use 'ListClusters', 'DescribeCluster', 'ListSteps', 'ListInstanceGroups' and 'ListBootstrapActions'
-- instead.
--
-- DescribeJobFlows returns a list of job flows that match all of the supplied
-- parameters. The parameters can include a list of job flow IDs, job flow
-- states, and restrictions on job flow creation date and time.
--
-- Regardless of supplied parameters, only job flows created within the last
-- two months are returned.
--
-- If no parameters are supplied, then job flows matching either of the
-- following criteria are returned:
--
-- Job flows created and completed in the last two weeks  Job flows created
-- within the last two months that are in one of the following states: 'RUNNING', 'WAITING', 'SHUTTING_DOWN', 'STARTING'    Amazon Elastic MapReduce can return a maximum of
-- 512 job flow descriptions.
--
-- <http://docs.aws.amazon.com/ElasticMapReduce/latest/API/API_DescribeJobFlows.html>
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
import Network.AWS.Request.JSON
import Network.AWS.EMR.Types
import qualified GHC.Exts

data DescribeJobFlows = DescribeJobFlows
    { _djfCreatedAfter  :: Maybe POSIX
    , _djfCreatedBefore :: Maybe POSIX
    , _djfJobFlowIds    :: List "JobFlowIds" Text
    , _djfJobFlowStates :: List "JobFlowStates" JobFlowExecutionState
    } deriving (Eq, Show)

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
-- * 'djfJobFlowStates' @::@ ['JobFlowExecutionState']
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
djfCreatedAfter = lens _djfCreatedAfter (\s a -> s { _djfCreatedAfter = a }) . mapping _Time

-- | Return only job flows created before this date and time.
djfCreatedBefore :: Lens' DescribeJobFlows (Maybe UTCTime)
djfCreatedBefore = lens _djfCreatedBefore (\s a -> s { _djfCreatedBefore = a }) . mapping _Time

-- | Return only job flows whose job flow ID is contained in this list.
djfJobFlowIds :: Lens' DescribeJobFlows [Text]
djfJobFlowIds = lens _djfJobFlowIds (\s a -> s { _djfJobFlowIds = a }) . _List

-- | Return only job flows whose state is contained in this list.
djfJobFlowStates :: Lens' DescribeJobFlows [JobFlowExecutionState]
djfJobFlowStates = lens _djfJobFlowStates (\s a -> s { _djfJobFlowStates = a }) . _List

newtype DescribeJobFlowsResponse = DescribeJobFlowsResponse
    { _djfrJobFlows :: List "JobFlows" JobFlowDetail
    } deriving (Eq, Show, Monoid, Semigroup)

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
djfrJobFlows = lens _djfrJobFlows (\s a -> s { _djfrJobFlows = a }) . _List

instance ToPath DescribeJobFlows where
    toPath = const "/"

instance ToQuery DescribeJobFlows where
    toQuery = const mempty

instance ToHeaders DescribeJobFlows

instance ToJSON DescribeJobFlows where
    toJSON DescribeJobFlows{..} = object
        [ "CreatedAfter"  .= _djfCreatedAfter
        , "CreatedBefore" .= _djfCreatedBefore
        , "JobFlowIds"    .= _djfJobFlowIds
        , "JobFlowStates" .= _djfJobFlowStates
        ]

instance AWSRequest DescribeJobFlows where
    type Sv DescribeJobFlows = EMR
    type Rs DescribeJobFlows = DescribeJobFlowsResponse

    request  = post "DescribeJobFlows"
    response = jsonResponse

instance FromJSON DescribeJobFlowsResponse where
    parseJSON = withObject "DescribeJobFlowsResponse" $ \o -> DescribeJobFlowsResponse
        <$> o .:? "JobFlows" .!= mempty
