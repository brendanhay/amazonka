{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.AutoScaling.ExecutePolicy
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

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
    , epHonorCooldown
    , epAutoScalingGroupName
    , epPolicyName

    -- * Response
    , ExecutePolicyResponse
    -- ** Response constructor
    , executePolicyResponse
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'executePolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'epHonorCooldown'
--
-- * 'epAutoScalingGroupName'
--
-- * 'epPolicyName'
data ExecutePolicy = ExecutePolicy'
    { _epHonorCooldown        :: !(Maybe Bool)
    , _epAutoScalingGroupName :: !(Maybe Text)
    , _epPolicyName           :: !Text
    } deriving (Eq,Read,Show)

-- | 'ExecutePolicy' smart constructor.
executePolicy :: Text -> ExecutePolicy
executePolicy pPolicyName =
    ExecutePolicy'
    { _epHonorCooldown = Nothing
    , _epAutoScalingGroupName = Nothing
    , _epPolicyName = pPolicyName
    }

-- | Set to @True@ if you want Auto Scaling to wait for the cooldown period
-- associated with the Auto Scaling group to complete before executing the
-- policy.
--
-- Set to @False@ if you want Auto Scaling to circumvent the cooldown
-- period associated with the Auto Scaling group and execute the policy
-- before the cooldown period ends.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/Cooldown.html Understanding Auto Scaling Cooldowns>
-- in the /Auto Scaling Developer Guide/.
epHonorCooldown :: Lens' ExecutePolicy (Maybe Bool)
epHonorCooldown = lens _epHonorCooldown (\ s a -> s{_epHonorCooldown = a});

-- | The name or Amazon Resource Name (ARN) of the Auto Scaling group.
epAutoScalingGroupName :: Lens' ExecutePolicy (Maybe Text)
epAutoScalingGroupName = lens _epAutoScalingGroupName (\ s a -> s{_epAutoScalingGroupName = a});

-- | The name or ARN of the policy.
epPolicyName :: Lens' ExecutePolicy Text
epPolicyName = lens _epPolicyName (\ s a -> s{_epPolicyName = a});

instance AWSRequest ExecutePolicy where
        type Sv ExecutePolicy = AutoScaling
        type Rs ExecutePolicy = ExecutePolicyResponse
        request = post
        response = receiveNull ExecutePolicyResponse'

instance ToHeaders ExecutePolicy where
        toHeaders = const mempty

instance ToPath ExecutePolicy where
        toPath = const "/"

instance ToQuery ExecutePolicy where
        toQuery ExecutePolicy'{..}
          = mconcat
              ["Action" =: ("ExecutePolicy" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "HonorCooldown" =: _epHonorCooldown,
               "AutoScalingGroupName" =: _epAutoScalingGroupName,
               "PolicyName" =: _epPolicyName]

-- | /See:/ 'executePolicyResponse' smart constructor.
data ExecutePolicyResponse =
    ExecutePolicyResponse'
    deriving (Eq,Read,Show)

-- | 'ExecutePolicyResponse' smart constructor.
executePolicyResponse :: ExecutePolicyResponse
executePolicyResponse = ExecutePolicyResponse'
