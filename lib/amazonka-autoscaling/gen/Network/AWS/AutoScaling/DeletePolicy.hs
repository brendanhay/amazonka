{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DeletePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified scaling policy.
--
--
-- Deleting either a step scaling policy or a simple scaling policy deletes the underlying alarm action, but does not delete the alarm, even if it no longer has an associated action.
--
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/deleting-scaling-policy.html Deleting a scaling policy> in the /Amazon EC2 Auto Scaling User Guide/ .
module Network.AWS.AutoScaling.DeletePolicy
  ( -- * Creating a Request
    deletePolicy,
    DeletePolicy,

    -- * Request Lenses
    dpAutoScalingGroupName,
    dpPolicyName,

    -- * Destructuring the Response
    deletePolicyResponse,
    DeletePolicyResponse,
  )
where

import Network.AWS.AutoScaling.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deletePolicy' smart constructor.
data DeletePolicy = DeletePolicy'
  { _dpAutoScalingGroupName ::
      !(Maybe Text),
    _dpPolicyName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeletePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpAutoScalingGroupName' - The name of the Auto Scaling group.
--
-- * 'dpPolicyName' - The name or Amazon Resource Name (ARN) of the policy.
deletePolicy ::
  -- | 'dpPolicyName'
  Text ->
  DeletePolicy
deletePolicy pPolicyName_ =
  DeletePolicy'
    { _dpAutoScalingGroupName = Nothing,
      _dpPolicyName = pPolicyName_
    }

-- | The name of the Auto Scaling group.
dpAutoScalingGroupName :: Lens' DeletePolicy (Maybe Text)
dpAutoScalingGroupName = lens _dpAutoScalingGroupName (\s a -> s {_dpAutoScalingGroupName = a})

-- | The name or Amazon Resource Name (ARN) of the policy.
dpPolicyName :: Lens' DeletePolicy Text
dpPolicyName = lens _dpPolicyName (\s a -> s {_dpPolicyName = a})

instance AWSRequest DeletePolicy where
  type Rs DeletePolicy = DeletePolicyResponse
  request = postQuery autoScaling
  response = receiveNull DeletePolicyResponse'

instance Hashable DeletePolicy

instance NFData DeletePolicy

instance ToHeaders DeletePolicy where
  toHeaders = const mempty

instance ToPath DeletePolicy where
  toPath = const "/"

instance ToQuery DeletePolicy where
  toQuery DeletePolicy' {..} =
    mconcat
      [ "Action" =: ("DeletePolicy" :: ByteString),
        "Version" =: ("2011-01-01" :: ByteString),
        "AutoScalingGroupName" =: _dpAutoScalingGroupName,
        "PolicyName" =: _dpPolicyName
      ]

-- | /See:/ 'deletePolicyResponse' smart constructor.
data DeletePolicyResponse = DeletePolicyResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeletePolicyResponse' with the minimum fields required to make a request.
deletePolicyResponse ::
  DeletePolicyResponse
deletePolicyResponse = DeletePolicyResponse'

instance NFData DeletePolicyResponse
