{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
module Network.AWS.AutoScaling.ExecutePolicy
    (
    -- * Request
      ExecutePolicyType
    -- ** Request constructor
    , executePolicy
    -- ** Request lenses
    , eptAutoScalingGroupName
    , eptHonorCooldown
    , eptPolicyName

    -- * Response
    , ExecutePolicyResponse
    -- ** Response constructor
    , executePolicyResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types

data ExecutePolicyType = ExecutePolicyType
    { _eptAutoScalingGroupName :: Maybe Text
    , _eptHonorCooldown        :: Maybe Bool
    , _eptPolicyName           :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ExecutePolicyType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eptAutoScalingGroupName' @::@ 'Maybe' 'Text'
--
-- * 'eptHonorCooldown' @::@ 'Maybe' 'Bool'
--
-- * 'eptPolicyName' @::@ 'Text'
--
executePolicy :: Text -- ^ 'eptPolicyName'
              -> ExecutePolicyType
executePolicy p1 = ExecutePolicyType
    { _eptPolicyName           = p1
    , _eptAutoScalingGroupName = Nothing
    , _eptHonorCooldown        = Nothing
    }

-- | The name or the Amazon Resource Name (ARN) of the Auto Scaling group.
eptAutoScalingGroupName :: Lens' ExecutePolicyType (Maybe Text)
eptAutoScalingGroupName =
    lens _eptAutoScalingGroupName (\s a -> s { _eptAutoScalingGroupName = a })

-- | Set to True if you want Auto Scaling to wait for the cooldown period
-- associated with the Auto Scaling group to complete before executing the
-- policy. Set to False if you want Auto Scaling to circumvent the cooldown
-- period associated with the Auto Scaling group and execute the policy
-- before the cooldown period ends. For information about cooldown period,
-- see Cooldown Period in the Auto Scaling Developer Guide.
eptHonorCooldown :: Lens' ExecutePolicyType (Maybe Bool)
eptHonorCooldown = lens _eptHonorCooldown (\s a -> s { _eptHonorCooldown = a })

-- | The name or ARN of the policy you want to run.
eptPolicyName :: Lens' ExecutePolicyType Text
eptPolicyName = lens _eptPolicyName (\s a -> s { _eptPolicyName = a })

instance ToPath ExecutePolicyType where
    toPath = const "/"

instance ToQuery ExecutePolicyType

data ExecutePolicyResponse = ExecutePolicyResponse

-- | 'ExecutePolicyResponse' constructor.
executePolicyResponse :: ExecutePolicyResponse
executePolicyResponse = ExecutePolicyResponse

instance AWSRequest ExecutePolicyType where
    type Sv ExecutePolicyType = AutoScaling
    type Rs ExecutePolicyType = ExecutePolicyResponse

    request  = post "ExecutePolicy"
    response = const (nullaryResponse ExecutePolicyResponse)
