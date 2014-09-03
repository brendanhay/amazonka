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
    , describePolicies
    -- ** Request lenses
    , dpuMaxRecords
    , dpuPolicyNames
    , dpuAutoScalingGroupName
    , dpuNextToken

    -- * Response
    , DescribePoliciesResponse
    -- ** Response lenses
    , ptScalingPolicies
    , ptNextToken
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribePolicies' request.
describePolicies :: DescribePolicies
describePolicies = DescribePolicies
    { _dpuMaxRecords = Nothing
    , _dpuPolicyNames = mempty
    , _dpuAutoScalingGroupName = Nothing
    , _dpuNextToken = Nothing
    }

data DescribePolicies = DescribePolicies
    { _dpuMaxRecords :: Maybe Integer
      -- ^ The maximum number of policies that will be described with each
      -- call.
    , _dpuPolicyNames :: [Text]
      -- ^ A list of policy names or policy ARNs to be described. If this
      -- list is omitted, all policy names are described. If an auto
      -- scaling group name is provided, the results are limited to that
      -- group. The list of requested policy names cannot contain more
      -- than 50 items. If unknown policy names are requested, they are
      -- ignored with no error.
    , _dpuAutoScalingGroupName :: Maybe Text
      -- ^ The name of the Auto Scaling group.
    , _dpuNextToken :: Maybe Text
      -- ^ A string that is used to mark the start of the next batch of
      -- returned results for pagination.
    } deriving (Show, Generic)

-- | The maximum number of policies that will be described with each call.
dpuMaxRecords
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DescribePolicies
    -> f DescribePolicies
dpuMaxRecords f x =
    (\y -> x { _dpuMaxRecords = y })
       <$> f (_dpuMaxRecords x)
{-# INLINE dpuMaxRecords #-}

-- | A list of policy names or policy ARNs to be described. If this list is
-- omitted, all policy names are described. If an auto scaling group name is
-- provided, the results are limited to that group. The list of requested
-- policy names cannot contain more than 50 items. If unknown policy names are
-- requested, they are ignored with no error.
dpuPolicyNames
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> DescribePolicies
    -> f DescribePolicies
dpuPolicyNames f x =
    (\y -> x { _dpuPolicyNames = y })
       <$> f (_dpuPolicyNames x)
{-# INLINE dpuPolicyNames #-}

-- | The name of the Auto Scaling group.
dpuAutoScalingGroupName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribePolicies
    -> f DescribePolicies
dpuAutoScalingGroupName f x =
    (\y -> x { _dpuAutoScalingGroupName = y })
       <$> f (_dpuAutoScalingGroupName x)
{-# INLINE dpuAutoScalingGroupName #-}

-- | A string that is used to mark the start of the next batch of returned
-- results for pagination.
dpuNextToken
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribePolicies
    -> f DescribePolicies
dpuNextToken f x =
    (\y -> x { _dpuNextToken = y })
       <$> f (_dpuNextToken x)
{-# INLINE dpuNextToken #-}

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
ptScalingPolicies
    :: Functor f
    => ([ScalingPolicy]
    -> f ([ScalingPolicy]))
    -> DescribePoliciesResponse
    -> f DescribePoliciesResponse
ptScalingPolicies f x =
    (\y -> x { _ptScalingPolicies = y })
       <$> f (_ptScalingPolicies x)
{-# INLINE ptScalingPolicies #-}

-- | A string that marks the start of the next batch of returned results.
ptNextToken
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribePoliciesResponse
    -> f DescribePoliciesResponse
ptNextToken f x =
    (\y -> x { _ptNextToken = y })
       <$> f (_ptNextToken x)
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
