{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.DescribePolicies
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
-- https://autoscaling.amazonaws.com/?AutoScalingGroupName=my-test-asg
-- &MaxRecords=20 &Version=2011-01-01 &Action=DescribePolicies &AUTHPARAMS
-- arn:aws:autoscaling:us-east-1:803981987763:scalingPolicy:c322
-- 761b-3172-4d56-9a21-0ed9d6161d67:autoScalingGroupName/my-test-asg:policyName/MyScaleDownPolicy
-- ChangeInCapacity -1 MyScaleDownPolicy my-test-asg 60 TestQueue
-- arn:aws:cloudwatch:us-east-1:803981987763:alarm:TestQueue
-- arn:aws:autoscaling:us-east-1:803981987763:scalingPolicy:c55a5cdd-9be0-435b-b60b-a8dd313159f5:autoScalingGroupName/my-test-asg:policyName/MyScaleUpPolicy
-- ChangeInCapacity 1 MyScaleUpPolicy my-test-asg 60 TestQueue
-- arn:aws:cloudwatch:us-east-1:803981987763:alarm:TestQueue
-- ec3bffad-b739-11e2-b38d-15fbEXAMPLE.
module Network.AWS.AutoScaling.V2011_01_01.DescribePolicies
    (
    -- * Request
      DescribePolicies
    -- ** Request constructor
    , mkDescribePolicies
    -- ** Request lenses
    , dp1AutoScalingGroupName
    , dp1PolicyNames
    , dp1NextToken
    , dp1MaxRecords

    -- * Response
    , DescribePoliciesResponse
    -- ** Response lenses
    , dprsScalingPolicies
    , dprsNextToken
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | 
data DescribePolicies = DescribePolicies
    { _dp1AutoScalingGroupName :: Maybe Text
    , _dp1PolicyNames :: [Text]
    , _dp1NextToken :: Maybe Text
    , _dp1MaxRecords :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribePolicies' request.
mkDescribePolicies :: DescribePolicies
mkDescribePolicies = DescribePolicies
    { _dp1AutoScalingGroupName = Nothing
    , _dp1PolicyNames = mempty
    , _dp1NextToken = Nothing
    , _dp1MaxRecords = Nothing
    }

-- | The name of the Auto Scaling group.
dp1AutoScalingGroupName :: Lens' DescribePolicies (Maybe Text)
dp1AutoScalingGroupName =
    lens _dp1AutoScalingGroupName
         (\s a -> s { _dp1AutoScalingGroupName = a })

-- | A list of policy names or policy ARNs to be described. If this list is
-- omitted, all policy names are described. If an auto scaling group name is
-- provided, the results are limited to that group. The list of requested
-- policy names cannot contain more than 50 items. If unknown policy names are
-- requested, they are ignored with no error.
dp1PolicyNames :: Lens' DescribePolicies [Text]
dp1PolicyNames = lens _dp1PolicyNames (\s a -> s { _dp1PolicyNames = a })

-- | A string that is used to mark the start of the next batch of returned
-- results for pagination.
dp1NextToken :: Lens' DescribePolicies (Maybe Text)
dp1NextToken = lens _dp1NextToken (\s a -> s { _dp1NextToken = a })

-- | The maximum number of policies that will be described with each call.
dp1MaxRecords :: Lens' DescribePolicies (Maybe Integer)
dp1MaxRecords = lens _dp1MaxRecords (\s a -> s { _dp1MaxRecords = a })

instance ToQuery DescribePolicies where
    toQuery = genericQuery def

-- | The PoliciesType data type.
data DescribePoliciesResponse = DescribePoliciesResponse
    { _dprsScalingPolicies :: [ScalingPolicy]
    , _dprsNextToken :: Maybe Text
    } deriving (Show, Generic)

-- | A list of scaling policies.
dprsScalingPolicies :: Lens' DescribePoliciesResponse [ScalingPolicy]
dprsScalingPolicies =
    lens _dprsScalingPolicies (\s a -> s { _dprsScalingPolicies = a })

-- | A string that marks the start of the next batch of returned results.
dprsNextToken :: Lens' DescribePoliciesResponse (Maybe Text)
dprsNextToken = lens _dprsNextToken (\s a -> s { _dprsNextToken = a })

instance FromXML DescribePoliciesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribePolicies where
    type Sv DescribePolicies = AutoScaling
    type Rs DescribePolicies = DescribePoliciesResponse

    request = post "DescribePolicies"
    response _ = xmlResponse

instance AWSPager DescribePolicies where
    next rq rs = (\x -> rq & dp1NextToken ?~ x)
        <$> (rs ^. dprsNextToken)
