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
-- Module      : Network.AWS.EC2.ModifyFleet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified EC2 Fleet.
--
--
-- You can only modify an EC2 Fleet request of type @maintain@ .
--
-- While the EC2 Fleet is being modified, it is in the @modifying@ state.
--
-- To scale up your EC2 Fleet, increase its target capacity. The EC2 Fleet launches the additional Spot Instances according to the allocation strategy for the EC2 Fleet request. If the allocation strategy is @lowest-price@ , the EC2 Fleet launches instances using the Spot Instance pool with the lowest price. If the allocation strategy is @diversified@ , the EC2 Fleet distributes the instances across the Spot Instance pools. If the allocation strategy is @capacity-optimized@ , EC2 Fleet launches instances from Spot Instance pools with optimal capacity for the number of instances that are launching.
--
-- To scale down your EC2 Fleet, decrease its target capacity. First, the EC2 Fleet cancels any open requests that exceed the new target capacity. You can request that the EC2 Fleet terminate Spot Instances until the size of the fleet no longer exceeds the new target capacity. If the allocation strategy is @lowest-price@ , the EC2 Fleet terminates the instances with the highest price per unit. If the allocation strategy is @capacity-optimized@ , the EC2 Fleet terminates the instances in the Spot Instance pools that have the least available Spot Instance capacity. If the allocation strategy is @diversified@ , the EC2 Fleet terminates instances across the Spot Instance pools. Alternatively, you can request that the EC2 Fleet keep the fleet at its current size, but not replace any Spot Instances that are interrupted or that you terminate manually.
--
-- If you are finished with your EC2 Fleet for now, but will use it again later, you can set the target capacity to 0.
module Network.AWS.EC2.ModifyFleet
  ( -- * Creating a Request
    modifyFleet,
    ModifyFleet,

    -- * Request Lenses
    mfTargetCapacitySpecification,
    mfExcessCapacityTerminationPolicy,
    mfLaunchTemplateConfigs,
    mfDryRun,
    mfFleetId,

    -- * Destructuring the Response
    modifyFleetResponse,
    ModifyFleetResponse,

    -- * Response Lenses
    mfrsReturn,
    mfrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'modifyFleet' smart constructor.
data ModifyFleet = ModifyFleet'
  { _mfTargetCapacitySpecification ::
      !(Maybe TargetCapacitySpecificationRequest),
    _mfExcessCapacityTerminationPolicy ::
      !(Maybe FleetExcessCapacityTerminationPolicy),
    _mfLaunchTemplateConfigs ::
      !(Maybe [FleetLaunchTemplateConfigRequest]),
    _mfDryRun :: !(Maybe Bool),
    _mfFleetId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifyFleet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mfTargetCapacitySpecification' - The size of the EC2 Fleet.
--
-- * 'mfExcessCapacityTerminationPolicy' - Indicates whether running instances should be terminated if the total target capacity of the EC2 Fleet is decreased below the current size of the EC2 Fleet.
--
-- * 'mfLaunchTemplateConfigs' - The launch template and overrides.
--
-- * 'mfDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'mfFleetId' - The ID of the EC2 Fleet.
modifyFleet ::
  -- | 'mfFleetId'
  Text ->
  ModifyFleet
modifyFleet pFleetId_ =
  ModifyFleet'
    { _mfTargetCapacitySpecification = Nothing,
      _mfExcessCapacityTerminationPolicy = Nothing,
      _mfLaunchTemplateConfigs = Nothing,
      _mfDryRun = Nothing,
      _mfFleetId = pFleetId_
    }

-- | The size of the EC2 Fleet.
mfTargetCapacitySpecification :: Lens' ModifyFleet (Maybe TargetCapacitySpecificationRequest)
mfTargetCapacitySpecification = lens _mfTargetCapacitySpecification (\s a -> s {_mfTargetCapacitySpecification = a})

-- | Indicates whether running instances should be terminated if the total target capacity of the EC2 Fleet is decreased below the current size of the EC2 Fleet.
mfExcessCapacityTerminationPolicy :: Lens' ModifyFleet (Maybe FleetExcessCapacityTerminationPolicy)
mfExcessCapacityTerminationPolicy = lens _mfExcessCapacityTerminationPolicy (\s a -> s {_mfExcessCapacityTerminationPolicy = a})

-- | The launch template and overrides.
mfLaunchTemplateConfigs :: Lens' ModifyFleet [FleetLaunchTemplateConfigRequest]
mfLaunchTemplateConfigs = lens _mfLaunchTemplateConfigs (\s a -> s {_mfLaunchTemplateConfigs = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mfDryRun :: Lens' ModifyFleet (Maybe Bool)
mfDryRun = lens _mfDryRun (\s a -> s {_mfDryRun = a})

-- | The ID of the EC2 Fleet.
mfFleetId :: Lens' ModifyFleet Text
mfFleetId = lens _mfFleetId (\s a -> s {_mfFleetId = a})

instance AWSRequest ModifyFleet where
  type Rs ModifyFleet = ModifyFleetResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          ModifyFleetResponse' <$> (x .@? "return") <*> (pure (fromEnum s))
      )

instance Hashable ModifyFleet

instance NFData ModifyFleet

instance ToHeaders ModifyFleet where
  toHeaders = const mempty

instance ToPath ModifyFleet where
  toPath = const "/"

instance ToQuery ModifyFleet where
  toQuery ModifyFleet' {..} =
    mconcat
      [ "Action" =: ("ModifyFleet" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "TargetCapacitySpecification" =: _mfTargetCapacitySpecification,
        "ExcessCapacityTerminationPolicy"
          =: _mfExcessCapacityTerminationPolicy,
        toQuery
          (toQueryList "LaunchTemplateConfig" <$> _mfLaunchTemplateConfigs),
        "DryRun" =: _mfDryRun,
        "FleetId" =: _mfFleetId
      ]

-- | /See:/ 'modifyFleetResponse' smart constructor.
data ModifyFleetResponse = ModifyFleetResponse'
  { _mfrsReturn ::
      !(Maybe Bool),
    _mfrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifyFleetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mfrsReturn' - Is @true@ if the request succeeds, and an error otherwise.
--
-- * 'mfrsResponseStatus' - -- | The response status code.
modifyFleetResponse ::
  -- | 'mfrsResponseStatus'
  Int ->
  ModifyFleetResponse
modifyFleetResponse pResponseStatus_ =
  ModifyFleetResponse'
    { _mfrsReturn = Nothing,
      _mfrsResponseStatus = pResponseStatus_
    }

-- | Is @true@ if the request succeeds, and an error otherwise.
mfrsReturn :: Lens' ModifyFleetResponse (Maybe Bool)
mfrsReturn = lens _mfrsReturn (\s a -> s {_mfrsReturn = a})

-- | -- | The response status code.
mfrsResponseStatus :: Lens' ModifyFleetResponse Int
mfrsResponseStatus = lens _mfrsResponseStatus (\s a -> s {_mfrsResponseStatus = a})

instance NFData ModifyFleetResponse
