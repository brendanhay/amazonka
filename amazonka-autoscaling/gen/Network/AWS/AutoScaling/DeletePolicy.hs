{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.AutoScaling.DeletePolicy
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

-- | Deletes the specified Auto Scaling policy.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DeletePolicy.html>
module Network.AWS.AutoScaling.DeletePolicy
    (
    -- * Request
      DeletePolicy
    -- ** Request constructor
    , deletePolicy
    -- ** Request lenses
    , dpAutoScalingGroupName
    , dpPolicyName

    -- * Response
    , DeletePolicyResponse
    -- ** Response constructor
    , deletePolicyResponse
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'deletePolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dpAutoScalingGroupName'
--
-- * 'dpPolicyName'
data DeletePolicy = DeletePolicy'
    { _dpAutoScalingGroupName :: !(Maybe Text)
    , _dpPolicyName           :: !Text
    } deriving (Eq,Read,Show)

-- | 'DeletePolicy' smart constructor.
deletePolicy :: Text -> DeletePolicy
deletePolicy pPolicyName =
    DeletePolicy'
    { _dpAutoScalingGroupName = Nothing
    , _dpPolicyName = pPolicyName
    }

-- | The name of the Auto Scaling group.
dpAutoScalingGroupName :: Lens' DeletePolicy (Maybe Text)
dpAutoScalingGroupName = lens _dpAutoScalingGroupName (\ s a -> s{_dpAutoScalingGroupName = a});

-- | The name or Amazon Resource Name (ARN) of the policy.
dpPolicyName :: Lens' DeletePolicy Text
dpPolicyName = lens _dpPolicyName (\ s a -> s{_dpPolicyName = a});

instance AWSRequest DeletePolicy where
        type Sv DeletePolicy = AutoScaling
        type Rs DeletePolicy = DeletePolicyResponse
        request = post
        response = receiveNull DeletePolicyResponse'

instance ToHeaders DeletePolicy where
        toHeaders = const mempty

instance ToPath DeletePolicy where
        toPath = const "/"

instance ToQuery DeletePolicy where
        toQuery DeletePolicy'{..}
          = mconcat
              ["Action" =: ("DeletePolicy" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "AutoScalingGroupName" =: _dpAutoScalingGroupName,
               "PolicyName" =: _dpPolicyName]

-- | /See:/ 'deletePolicyResponse' smart constructor.
data DeletePolicyResponse =
    DeletePolicyResponse'
    deriving (Eq,Read,Show)

-- | 'DeletePolicyResponse' smart constructor.
deletePolicyResponse :: DeletePolicyResponse
deletePolicyResponse = DeletePolicyResponse'
