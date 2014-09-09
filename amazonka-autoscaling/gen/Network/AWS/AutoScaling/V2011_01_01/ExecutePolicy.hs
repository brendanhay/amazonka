{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.ExecutePolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Executes the specified policy.
module Network.AWS.AutoScaling.V2011_01_01.ExecutePolicy
    (
    -- * Request
      ExecutePolicy
    -- ** Request constructor
    , mkExecutePolicy
    -- ** Request lenses
    , epAutoScalingGroupName
    , epPolicyName
    , epHonorCooldown

    -- * Response
    , ExecutePolicyResponse
    -- ** Response constructor
    , mkExecutePolicyResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

data ExecutePolicy = ExecutePolicy
    { _epAutoScalingGroupName :: Maybe Text
    , _epPolicyName :: Text
    , _epHonorCooldown :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ExecutePolicy' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AutoScalingGroupName ::@ @Maybe Text@
--
-- * @PolicyName ::@ @Text@
--
-- * @HonorCooldown ::@ @Maybe Bool@
--
mkExecutePolicy :: Text -- ^ 'epPolicyName'
                -> ExecutePolicy
mkExecutePolicy p2 = ExecutePolicy
    { _epAutoScalingGroupName = Nothing
    , _epPolicyName = p2
    , _epHonorCooldown = Nothing
    }

-- | The name or the Amazon Resource Name (ARN) of the Auto Scaling group.
epAutoScalingGroupName :: Lens' ExecutePolicy (Maybe Text)
epAutoScalingGroupName =
    lens _epAutoScalingGroupName (\s a -> s { _epAutoScalingGroupName = a })

-- | The name or ARN of the policy you want to run.
epPolicyName :: Lens' ExecutePolicy Text
epPolicyName = lens _epPolicyName (\s a -> s { _epPolicyName = a })

-- | Set to True if you want Auto Scaling to wait for the cooldown period
-- associated with the Auto Scaling group to complete before executing the
-- policy. Set to False if you want Auto Scaling to circumvent the cooldown
-- period associated with the Auto Scaling group and execute the policy before
-- the cooldown period ends. For information about cooldown period, see
-- Cooldown Period in the Auto Scaling Developer Guide.
epHonorCooldown :: Lens' ExecutePolicy (Maybe Bool)
epHonorCooldown = lens _epHonorCooldown (\s a -> s { _epHonorCooldown = a })

instance ToQuery ExecutePolicy where
    toQuery = genericQuery def

data ExecutePolicyResponse = ExecutePolicyResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ExecutePolicyResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkExecutePolicyResponse :: ExecutePolicyResponse
mkExecutePolicyResponse = ExecutePolicyResponse

instance AWSRequest ExecutePolicy where
    type Sv ExecutePolicy = AutoScaling
    type Rs ExecutePolicy = ExecutePolicyResponse

    request = post "ExecutePolicy"
    response _ = nullaryResponse ExecutePolicyResponse
