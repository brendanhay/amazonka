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
-- Module      : Network.AWS.EC2.ModifyFleet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified EC2 Fleet.
--
--
-- While the EC2 Fleet is being modified, it is in the @modifying@ state.
--
module Network.AWS.EC2.ModifyFleet
    (
    -- * Creating a Request
      modifyFleet
    , ModifyFleet
    -- * Request Lenses
    , mfExcessCapacityTerminationPolicy
    , mfDryRun
    , mfFleetId
    , mfTargetCapacitySpecification

    -- * Destructuring the Response
    , modifyFleetResponse
    , ModifyFleetResponse
    -- * Response Lenses
    , mfrsReturn
    , mfrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'modifyFleet' smart constructor.
data ModifyFleet = ModifyFleet'
  { _mfExcessCapacityTerminationPolicy :: !(Maybe FleetExcessCapacityTerminationPolicy)
  , _mfDryRun :: !(Maybe Bool)
  , _mfFleetId :: !Text
  , _mfTargetCapacitySpecification :: !TargetCapacitySpecificationRequest
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyFleet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mfExcessCapacityTerminationPolicy' - Indicates whether running instances should be terminated if the total target capacity of the EC2 Fleet is decreased below the current size of the EC2 Fleet.
--
-- * 'mfDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'mfFleetId' - The ID of the EC2 Fleet.
--
-- * 'mfTargetCapacitySpecification' - The size of the EC2 Fleet.
modifyFleet
    :: Text -- ^ 'mfFleetId'
    -> TargetCapacitySpecificationRequest -- ^ 'mfTargetCapacitySpecification'
    -> ModifyFleet
modifyFleet pFleetId_ pTargetCapacitySpecification_ =
  ModifyFleet'
    { _mfExcessCapacityTerminationPolicy = Nothing
    , _mfDryRun = Nothing
    , _mfFleetId = pFleetId_
    , _mfTargetCapacitySpecification = pTargetCapacitySpecification_
    }


-- | Indicates whether running instances should be terminated if the total target capacity of the EC2 Fleet is decreased below the current size of the EC2 Fleet.
mfExcessCapacityTerminationPolicy :: Lens' ModifyFleet (Maybe FleetExcessCapacityTerminationPolicy)
mfExcessCapacityTerminationPolicy = lens _mfExcessCapacityTerminationPolicy (\ s a -> s{_mfExcessCapacityTerminationPolicy = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mfDryRun :: Lens' ModifyFleet (Maybe Bool)
mfDryRun = lens _mfDryRun (\ s a -> s{_mfDryRun = a})

-- | The ID of the EC2 Fleet.
mfFleetId :: Lens' ModifyFleet Text
mfFleetId = lens _mfFleetId (\ s a -> s{_mfFleetId = a})

-- | The size of the EC2 Fleet.
mfTargetCapacitySpecification :: Lens' ModifyFleet TargetCapacitySpecificationRequest
mfTargetCapacitySpecification = lens _mfTargetCapacitySpecification (\ s a -> s{_mfTargetCapacitySpecification = a})

instance AWSRequest ModifyFleet where
        type Rs ModifyFleet = ModifyFleetResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 ModifyFleetResponse' <$>
                   (x .@? "return") <*> (pure (fromEnum s)))

instance Hashable ModifyFleet where

instance NFData ModifyFleet where

instance ToHeaders ModifyFleet where
        toHeaders = const mempty

instance ToPath ModifyFleet where
        toPath = const "/"

instance ToQuery ModifyFleet where
        toQuery ModifyFleet'{..}
          = mconcat
              ["Action" =: ("ModifyFleet" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "ExcessCapacityTerminationPolicy" =:
                 _mfExcessCapacityTerminationPolicy,
               "DryRun" =: _mfDryRun, "FleetId" =: _mfFleetId,
               "TargetCapacitySpecification" =:
                 _mfTargetCapacitySpecification]

-- | /See:/ 'modifyFleetResponse' smart constructor.
data ModifyFleetResponse = ModifyFleetResponse'
  { _mfrsReturn         :: !(Maybe Bool)
  , _mfrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyFleetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mfrsReturn' - Is @true@ if the request succeeds, and an error otherwise.
--
-- * 'mfrsResponseStatus' - -- | The response status code.
modifyFleetResponse
    :: Int -- ^ 'mfrsResponseStatus'
    -> ModifyFleetResponse
modifyFleetResponse pResponseStatus_ =
  ModifyFleetResponse'
    {_mfrsReturn = Nothing, _mfrsResponseStatus = pResponseStatus_}


-- | Is @true@ if the request succeeds, and an error otherwise.
mfrsReturn :: Lens' ModifyFleetResponse (Maybe Bool)
mfrsReturn = lens _mfrsReturn (\ s a -> s{_mfrsReturn = a})

-- | -- | The response status code.
mfrsResponseStatus :: Lens' ModifyFleetResponse Int
mfrsResponseStatus = lens _mfrsResponseStatus (\ s a -> s{_mfrsResponseStatus = a})

instance NFData ModifyFleetResponse where
