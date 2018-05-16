{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DeleteScalingPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a fleet scaling policy. This action means that the policy is no longer in force and removes all record of it. To delete a scaling policy, specify both the scaling policy name and the fleet ID it is associated with.
--
--
-- To temporarily suspend scaling policies, call 'StopFleetActions' . This operation suspends all policies for the fleet.
--
-- Operations related to fleet capacity scaling include:
--
--     * 'DescribeFleetCapacity'
--
--     * 'UpdateFleetCapacity'
--
--     * 'DescribeEC2InstanceLimits'
--
--     * Manage scaling policies:
--
--     * 'PutScalingPolicy' (auto-scaling)
--
--     * 'DescribeScalingPolicies' (auto-scaling)
--
--     * 'DeleteScalingPolicy' (auto-scaling)
--
--
--
--     * Manage fleet actions:
--
--     * 'StartFleetActions'
--
--     * 'StopFleetActions'
--
--
--
--
--
module Network.AWS.GameLift.DeleteScalingPolicy
    (
    -- * Creating a Request
      deleteScalingPolicy
    , DeleteScalingPolicy
    -- * Request Lenses
    , dspName
    , dspFleetId

    -- * Destructuring the Response
    , deleteScalingPolicyResponse
    , DeleteScalingPolicyResponse
    ) where

import Network.AWS.GameLift.Types
import Network.AWS.GameLift.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input for a request action.
--
--
--
-- /See:/ 'deleteScalingPolicy' smart constructor.
data DeleteScalingPolicy = DeleteScalingPolicy'
  { _dspName    :: !Text
  , _dspFleetId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteScalingPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dspName' - Descriptive label that is associated with a scaling policy. Policy names do not need to be unique.
--
-- * 'dspFleetId' - Unique identifier for a fleet to be deleted.
deleteScalingPolicy
    :: Text -- ^ 'dspName'
    -> Text -- ^ 'dspFleetId'
    -> DeleteScalingPolicy
deleteScalingPolicy pName_ pFleetId_ =
  DeleteScalingPolicy' {_dspName = pName_, _dspFleetId = pFleetId_}


-- | Descriptive label that is associated with a scaling policy. Policy names do not need to be unique.
dspName :: Lens' DeleteScalingPolicy Text
dspName = lens _dspName (\ s a -> s{_dspName = a})

-- | Unique identifier for a fleet to be deleted.
dspFleetId :: Lens' DeleteScalingPolicy Text
dspFleetId = lens _dspFleetId (\ s a -> s{_dspFleetId = a})

instance AWSRequest DeleteScalingPolicy where
        type Rs DeleteScalingPolicy =
             DeleteScalingPolicyResponse
        request = postJSON gameLift
        response = receiveNull DeleteScalingPolicyResponse'

instance Hashable DeleteScalingPolicy where

instance NFData DeleteScalingPolicy where

instance ToHeaders DeleteScalingPolicy where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.DeleteScalingPolicy" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteScalingPolicy where
        toJSON DeleteScalingPolicy'{..}
          = object
              (catMaybes
                 [Just ("Name" .= _dspName),
                  Just ("FleetId" .= _dspFleetId)])

instance ToPath DeleteScalingPolicy where
        toPath = const "/"

instance ToQuery DeleteScalingPolicy where
        toQuery = const mempty

-- | /See:/ 'deleteScalingPolicyResponse' smart constructor.
data DeleteScalingPolicyResponse =
  DeleteScalingPolicyResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteScalingPolicyResponse' with the minimum fields required to make a request.
--
deleteScalingPolicyResponse
    :: DeleteScalingPolicyResponse
deleteScalingPolicyResponse = DeleteScalingPolicyResponse'


instance NFData DeleteScalingPolicyResponse where
