{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.DescribePolicies
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns descriptions of what each policy does. This action supports
-- pagination. If the response includes a token, there are more records
-- available. To get the additional records, repeat the request with the
-- response token as the NextToken parameter.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribePolicies.html>
module Network.AWS.AutoScaling.DescribePolicies
    (
    -- * Request
      DescribePolicies
    -- ** Request constructor
    , describePolicies
    -- ** Request lenses
    , dp1AutoScalingGroupName
    , dp1MaxRecords
    , dp1NextToken
    , dp1PolicyNames

    -- * Response
    , DescribePoliciesResponse
    -- ** Response constructor
    , describePoliciesResponse
    -- ** Response lenses
    , dprNextToken
    , dprScalingPolicies
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import qualified GHC.Exts

data DescribePolicies = DescribePolicies
    { _dp1AutoScalingGroupName :: Maybe Text
    , _dp1MaxRecords           :: Maybe Int
    , _dp1NextToken            :: Maybe Text
    , _dp1PolicyNames          :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribePolicies' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dp1AutoScalingGroupName' @::@ 'Maybe' 'Text'
--
-- * 'dp1MaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'dp1NextToken' @::@ 'Maybe' 'Text'
--
-- * 'dp1PolicyNames' @::@ ['Text']
--
describePolicies :: DescribePolicies
describePolicies = DescribePolicies
    { _dp1AutoScalingGroupName = Nothing
    , _dp1PolicyNames          = mempty
    , _dp1NextToken            = Nothing
    , _dp1MaxRecords           = Nothing
    }

-- | The name of the Auto Scaling group.
dp1AutoScalingGroupName :: Lens' DescribePolicies (Maybe Text)
dp1AutoScalingGroupName =
    lens _dp1AutoScalingGroupName (\s a -> s { _dp1AutoScalingGroupName = a })

-- | The maximum number of policies that will be described with each call.
dp1MaxRecords :: Lens' DescribePolicies (Maybe Int)
dp1MaxRecords = lens _dp1MaxRecords (\s a -> s { _dp1MaxRecords = a })

-- | A string that is used to mark the start of the next batch of returned
-- results for pagination.
dp1NextToken :: Lens' DescribePolicies (Maybe Text)
dp1NextToken = lens _dp1NextToken (\s a -> s { _dp1NextToken = a })

-- | A list of policy names or policy ARNs to be described. If this list is
-- omitted, all policy names are described. If an auto scaling group name is
-- provided, the results are limited to that group. The list of requested
-- policy names cannot contain more than 50 items. If unknown policy names
-- are requested, they are ignored with no error.
dp1PolicyNames :: Lens' DescribePolicies [Text]
dp1PolicyNames = lens _dp1PolicyNames (\s a -> s { _dp1PolicyNames = a })

data DescribePoliciesResponse = DescribePoliciesResponse
    { _dprNextToken       :: Maybe Text
    , _dprScalingPolicies :: [ScalingPolicy]
    } deriving (Eq, Show, Generic)

-- | 'DescribePoliciesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dprNextToken' @::@ 'Maybe' 'Text'
--
-- * 'dprScalingPolicies' @::@ ['ScalingPolicy']
--
describePoliciesResponse :: DescribePoliciesResponse
describePoliciesResponse = DescribePoliciesResponse
    { _dprScalingPolicies = mempty
    , _dprNextToken       = Nothing
    }

-- | A string that marks the start of the next batch of returned results.
dprNextToken :: Lens' DescribePoliciesResponse (Maybe Text)
dprNextToken = lens _dprNextToken (\s a -> s { _dprNextToken = a })

-- | A list of scaling policies.
dprScalingPolicies :: Lens' DescribePoliciesResponse [ScalingPolicy]
dprScalingPolicies =
    lens _dprScalingPolicies (\s a -> s { _dprScalingPolicies = a })

instance ToPath DescribePolicies where
    toPath = const "/"

instance ToQuery DescribePolicies

instance ToHeaders DescribePolicies

instance AWSRequest DescribePolicies where
    type Sv DescribePolicies = AutoScaling
    type Rs DescribePolicies = DescribePoliciesResponse

    request  = post "DescribePolicies"
    response = xmlResponse

instance FromXML DescribePoliciesResponse where
    parseXML = withElement "DescribePoliciesResult" $ \x ->
        DescribePoliciesResponse
            <$> x .@? "NextToken"
            <*> x .@ "ScalingPolicies"
