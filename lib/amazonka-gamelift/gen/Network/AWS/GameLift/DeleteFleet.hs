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
-- Module      : Network.AWS.GameLift.DeleteFleet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes everything related to a fleet. Before deleting a fleet, you must set the fleet's desired capacity to zero. See 'UpdateFleetCapacity' .
--
--
-- This action removes the fleet's resources and the fleet record. Once a fleet is deleted, you can no longer use that fleet.
--
-- Fleet-related operations include:
--
--     * 'CreateFleet'
--
--     * 'ListFleets'
--
--     * 'DeleteFleet'
--
--     * Describe fleets:
--
--     * 'DescribeFleetAttributes'
--
--     * 'DescribeFleetCapacity'
--
--     * 'DescribeFleetPortSettings'
--
--     * 'DescribeFleetUtilization'
--
--     * 'DescribeRuntimeConfiguration'
--
--     * 'DescribeEC2InstanceLimits'
--
--     * 'DescribeFleetEvents'
--
--
--
--     * Update fleets:
--
--     * 'UpdateFleetAttributes'
--
--     * 'UpdateFleetCapacity'
--
--     * 'UpdateFleetPortSettings'
--
--     * 'UpdateRuntimeConfiguration'
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
module Network.AWS.GameLift.DeleteFleet
    (
    -- * Creating a Request
      deleteFleet
    , DeleteFleet
    -- * Request Lenses
    , dfFleetId

    -- * Destructuring the Response
    , deleteFleetResponse
    , DeleteFleetResponse
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
-- /See:/ 'deleteFleet' smart constructor.
newtype DeleteFleet = DeleteFleet'
  { _dfFleetId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteFleet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfFleetId' - Unique identifier for a fleet to be deleted.
deleteFleet
    :: Text -- ^ 'dfFleetId'
    -> DeleteFleet
deleteFleet pFleetId_ = DeleteFleet' {_dfFleetId = pFleetId_}


-- | Unique identifier for a fleet to be deleted.
dfFleetId :: Lens' DeleteFleet Text
dfFleetId = lens _dfFleetId (\ s a -> s{_dfFleetId = a})

instance AWSRequest DeleteFleet where
        type Rs DeleteFleet = DeleteFleetResponse
        request = postJSON gameLift
        response = receiveNull DeleteFleetResponse'

instance Hashable DeleteFleet where

instance NFData DeleteFleet where

instance ToHeaders DeleteFleet where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.DeleteFleet" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteFleet where
        toJSON DeleteFleet'{..}
          = object (catMaybes [Just ("FleetId" .= _dfFleetId)])

instance ToPath DeleteFleet where
        toPath = const "/"

instance ToQuery DeleteFleet where
        toQuery = const mempty

-- | /See:/ 'deleteFleetResponse' smart constructor.
data DeleteFleetResponse =
  DeleteFleetResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteFleetResponse' with the minimum fields required to make a request.
--
deleteFleetResponse
    :: DeleteFleetResponse
deleteFleetResponse = DeleteFleetResponse'


instance NFData DeleteFleetResponse where
