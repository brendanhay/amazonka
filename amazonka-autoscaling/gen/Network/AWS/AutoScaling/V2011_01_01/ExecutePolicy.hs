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
    , executePolicy
    -- ** Request lenses
    , eptPolicyName
    , eptHonorCooldown
    , eptAutoScalingGroupName

    -- * Response
    , ExecutePolicyResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ExecutePolicy' request.
executePolicy :: Text -- ^ 'eptPolicyName'
              -> ExecutePolicy
executePolicy p1 = ExecutePolicy
    { _eptPolicyName = p1
    , _eptHonorCooldown = Nothing
    , _eptAutoScalingGroupName = Nothing
    }

data ExecutePolicy = ExecutePolicy
    { _eptPolicyName :: Text
      -- ^ The name or ARN of the policy you want to run.
    , _eptHonorCooldown :: Maybe Bool
      -- ^ Set to True if you want Auto Scaling to wait for the cooldown
      -- period associated with the Auto Scaling group to complete before
      -- executing the policy. Set to False if you want Auto Scaling to
      -- circumvent the cooldown period associated with the Auto Scaling
      -- group and execute the policy before the cooldown period ends. For
      -- information about cooldown period, see Cooldown Period in the
      -- Auto Scaling Developer Guide.
    , _eptAutoScalingGroupName :: Maybe Text
      -- ^ The name or the Amazon Resource Name (ARN) of the Auto Scaling
      -- group.
    } deriving (Show, Generic)

-- | The name or ARN of the policy you want to run.
eptPolicyName
    :: Functor f
    => (Text
    -> f (Text))
    -> ExecutePolicy
    -> f ExecutePolicy
eptPolicyName f x =
    (\y -> x { _eptPolicyName = y })
       <$> f (_eptPolicyName x)
{-# INLINE eptPolicyName #-}

-- | Set to True if you want Auto Scaling to wait for the cooldown period
-- associated with the Auto Scaling group to complete before executing the
-- policy. Set to False if you want Auto Scaling to circumvent the cooldown
-- period associated with the Auto Scaling group and execute the policy before
-- the cooldown period ends. For information about cooldown period, see
-- Cooldown Period in the Auto Scaling Developer Guide.
eptHonorCooldown
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> ExecutePolicy
    -> f ExecutePolicy
eptHonorCooldown f x =
    (\y -> x { _eptHonorCooldown = y })
       <$> f (_eptHonorCooldown x)
{-# INLINE eptHonorCooldown #-}

-- | The name or the Amazon Resource Name (ARN) of the Auto Scaling group.
eptAutoScalingGroupName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ExecutePolicy
    -> f ExecutePolicy
eptAutoScalingGroupName f x =
    (\y -> x { _eptAutoScalingGroupName = y })
       <$> f (_eptAutoScalingGroupName x)
{-# INLINE eptAutoScalingGroupName #-}

instance ToQuery ExecutePolicy where
    toQuery = genericQuery def

data ExecutePolicyResponse = ExecutePolicyResponse
    deriving (Eq, Show, Generic)

instance AWSRequest ExecutePolicy where
    type Sv ExecutePolicy = AutoScaling
    type Rs ExecutePolicy = ExecutePolicyResponse

    request = post "ExecutePolicy"
    response _ = nullaryResponse ExecutePolicyResponse
