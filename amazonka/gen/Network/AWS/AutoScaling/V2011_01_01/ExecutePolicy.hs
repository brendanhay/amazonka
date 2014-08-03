{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
module Network.AWS.AutoScaling.V2011_01_01.ExecutePolicy where

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ExecutePolicy' request.
executePolicy :: Text -- ^ '_eptPolicyName'
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
    } deriving (Generic)

makeLenses ''ExecutePolicy

instance ToQuery ExecutePolicy where
    toQuery = genericToQuery def

data ExecutePolicyResponse = ExecutePolicyResponse
    deriving (Eq, Show, Generic)

makeLenses ''ExecutePolicyResponse

instance AWSRequest ExecutePolicy where
    type Sv ExecutePolicy = AutoScaling
    type Rs ExecutePolicy = ExecutePolicyResponse

    request = post "ExecutePolicy"
    response _ _ = return (Right ExecutePolicyResponse)
