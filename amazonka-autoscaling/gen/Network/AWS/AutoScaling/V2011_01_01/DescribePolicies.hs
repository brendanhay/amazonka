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
    , mkDescribePoliciesType
    -- ** Request lenses
    , dpuAutoScalingGroupName
    , dpuPolicyNames
    , dpuNextToken
    , dpuMaxRecords

    -- * Response
    , DescribePoliciesResponse
    -- ** Response lenses
    , ptScalingPolicies
    , ptNextToken
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribePolicies' request.
mkDescribePoliciesType :: DescribePolicies
mkDescribePoliciesType = DescribePolicies
    { _dpuAutoScalingGroupName = Nothing
    , _dpuPolicyNames = mempty
    , _dpuNextToken = Nothing
    , _dpuMaxRecords = Nothing
    }
{-# INLINE mkDescribePoliciesType #-}

data DescribePolicies = DescribePolicies
    { _dpuAutoScalingGroupName :: Maybe Text
      -- ^ The name of the Auto Scaling group.
    , _dpuPolicyNames :: [Text]
      -- ^ A list of policy names or policy ARNs to be described. If this
      -- list is omitted, all policy names are described. If an auto
      -- scaling group name is provided, the results are limited to that
      -- group. The list of requested policy names cannot contain more
      -- than 50 items. If unknown policy names are requested, they are
      -- ignored with no error.
    , _dpuNextToken :: Maybe Text
      -- ^ A string that is used to mark the start of the next batch of
      -- returned results for pagination.
    , _dpuMaxRecords :: Maybe Integer
      -- ^ The maximum number of policies that will be described with each
      -- call.
    } deriving (Show, Generic)

-- | The name of the Auto Scaling group.
dpuAutoScalingGroupName :: Lens' DescribePolicies (Maybe Text)
dpuAutoScalingGroupName = lens _dpuAutoScalingGroupName (\s a -> s { _dpuAutoScalingGroupName = a })
{-# INLINE dpuAutoScalingGroupName #-}

-- | A list of policy names or policy ARNs to be described. If this list is
-- omitted, all policy names are described. If an auto scaling group name is
-- provided, the results are limited to that group. The list of requested
-- policy names cannot contain more than 50 items. If unknown policy names are
-- requested, they are ignored with no error.
dpuPolicyNames :: Lens' DescribePolicies ([Text])
dpuPolicyNames = lens _dpuPolicyNames (\s a -> s { _dpuPolicyNames = a })
{-# INLINE dpuPolicyNames #-}

-- | A string that is used to mark the start of the next batch of returned
-- results for pagination.
dpuNextToken :: Lens' DescribePolicies (Maybe Text)
dpuNextToken = lens _dpuNextToken (\s a -> s { _dpuNextToken = a })
{-# INLINE dpuNextToken #-}

-- | The maximum number of policies that will be described with each call.
dpuMaxRecords :: Lens' DescribePolicies (Maybe Integer)
dpuMaxRecords = lens _dpuMaxRecords (\s a -> s { _dpuMaxRecords = a })
{-# INLINE dpuMaxRecords #-}

instance ToQuery DescribePolicies where
    toQuery = genericQuery def

data DescribePoliciesResponse = DescribePoliciesResponse
    { _ptScalingPolicies :: [ScalingPolicy]
      -- ^ A list of scaling policies.
    , _ptNextToken :: Maybe Text
      -- ^ A string that marks the start of the next batch of returned
      -- results.
    } deriving (Show, Generic)

-- | A list of scaling policies.
ptScalingPolicies :: Lens' DescribePoliciesResponse ([ScalingPolicy])
ptScalingPolicies = lens _ptScalingPolicies (\s a -> s { _ptScalingPolicies = a })
{-# INLINE ptScalingPolicies #-}

-- | A string that marks the start of the next batch of returned results.
ptNextToken :: Lens' DescribePoliciesResponse (Maybe Text)
ptNextToken = lens _ptNextToken (\s a -> s { _ptNextToken = a })
{-# INLINE ptNextToken #-}

instance FromXML DescribePoliciesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribePolicies where
    type Sv DescribePolicies = AutoScaling
    type Rs DescribePolicies = DescribePoliciesResponse

    request = post "DescribePolicies"
    response _ = xmlResponse

instance AWSPager DescribePolicies where
    next rq rs = (\x -> rq { _dpuNextToken = Just x })
        <$> (_ptNextToken rs)
