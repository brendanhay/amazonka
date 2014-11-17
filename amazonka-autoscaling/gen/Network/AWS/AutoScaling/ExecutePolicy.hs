{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.ExecutePolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Executes the specified policy.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_ExecutePolicy.html>
module Network.AWS.AutoScaling.ExecutePolicy
    (
    -- * Request
      ExecutePolicy
    -- ** Request constructor
    , executePolicy
    -- ** Request lenses
    , epAutoScalingGroupName
    , epHonorCooldown
    , epPolicyName

    -- * Response
    , ExecutePolicyResponse
    -- ** Response constructor
    , executePolicyResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import qualified GHC.Exts

data ExecutePolicy = ExecutePolicy
    { _epAutoScalingGroupName :: Maybe Text
    , _epHonorCooldown        :: Maybe Bool
    , _epPolicyName           :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ExecutePolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'epAutoScalingGroupName' @::@ 'Maybe' 'Text'
--
-- * 'epHonorCooldown' @::@ 'Maybe' 'Bool'
--
-- * 'epPolicyName' @::@ 'Text'
--
executePolicy :: Text -- ^ 'epPolicyName'
              -> ExecutePolicy
executePolicy p1 = ExecutePolicy
    { _epPolicyName           = p1
    , _epAutoScalingGroupName = Nothing
    , _epHonorCooldown        = Nothing
    }

-- | The name or the Amazon Resource Name (ARN) of the Auto Scaling group.
epAutoScalingGroupName :: Lens' ExecutePolicy (Maybe Text)
epAutoScalingGroupName =
    lens _epAutoScalingGroupName (\s a -> s { _epAutoScalingGroupName = a })

-- | Set to True if you want Auto Scaling to wait for the cooldown period
-- associated with the Auto Scaling group to complete before executing the
-- policy. Set to False if you want Auto Scaling to circumvent the cooldown
-- period associated with the Auto Scaling group and execute the policy
-- before the cooldown period ends. For information about cooldown period,
-- see Cooldown Period in the Auto Scaling Developer Guide.
epHonorCooldown :: Lens' ExecutePolicy (Maybe Bool)
epHonorCooldown = lens _epHonorCooldown (\s a -> s { _epHonorCooldown = a })

-- | The name or ARN of the policy you want to run.
epPolicyName :: Lens' ExecutePolicy Text
epPolicyName = lens _epPolicyName (\s a -> s { _epPolicyName = a })

data ExecutePolicyResponse = ExecutePolicyResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'ExecutePolicyResponse' constructor.
executePolicyResponse :: ExecutePolicyResponse
executePolicyResponse = ExecutePolicyResponse

instance AWSRequest ExecutePolicy where
    type Sv ExecutePolicy = AutoScaling
    type Rs ExecutePolicy = ExecutePolicyResponse

    request  = post "ExecutePolicy"
    response = nullResponse ExecutePolicyResponse

instance ToPath ExecutePolicy where
    toPath = const "/"

instance ToHeaders ExecutePolicy

instance ToQuery ExecutePolicy
