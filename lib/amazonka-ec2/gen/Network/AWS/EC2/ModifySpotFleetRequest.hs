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
-- Module      : Network.AWS.EC2.ModifySpotFleetRequest
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified Spot Fleet request.
--
--
-- While the Spot Fleet request is being modified, it is in the @modifying@ state.
--
-- To scale up your Spot Fleet, increase its target capacity. The Spot Fleet launches the additional Spot Instances according to the allocation strategy for the Spot Fleet request. If the allocation strategy is @lowestPrice@ , the Spot Fleet launches instances using the Spot pool with the lowest price. If the allocation strategy is @diversified@ , the Spot Fleet distributes the instances across the Spot pools.
--
-- To scale down your Spot Fleet, decrease its target capacity. First, the Spot Fleet cancels any open requests that exceed the new target capacity. You can request that the Spot Fleet terminate Spot Instances until the size of the fleet no longer exceeds the new target capacity. If the allocation strategy is @lowestPrice@ , the Spot Fleet terminates the instances with the highest price per unit. If the allocation strategy is @diversified@ , the Spot Fleet terminates instances across the Spot pools. Alternatively, you can request that the Spot Fleet keep the fleet at its current size, but not replace any Spot Instances that are interrupted or that you terminate manually.
--
-- If you are finished with your Spot Fleet for now, but will use it again later, you can set the target capacity to 0.
--
module Network.AWS.EC2.ModifySpotFleetRequest
    (
    -- * Creating a Request
      modifySpotFleetRequest
    , ModifySpotFleetRequest
    -- * Request Lenses
    , msfrTargetCapacity
    , msfrExcessCapacityTerminationPolicy
    , msfrSpotFleetRequestId

    -- * Destructuring the Response
    , modifySpotFleetRequestResponse
    , ModifySpotFleetRequestResponse
    -- * Response Lenses
    , msfrrsReturn
    , msfrrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for ModifySpotFleetRequest.
--
--
--
-- /See:/ 'modifySpotFleetRequest' smart constructor.
data ModifySpotFleetRequest = ModifySpotFleetRequest'
  { _msfrTargetCapacity :: !(Maybe Int)
  , _msfrExcessCapacityTerminationPolicy :: !(Maybe ExcessCapacityTerminationPolicy)
  , _msfrSpotFleetRequestId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifySpotFleetRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'msfrTargetCapacity' - The size of the fleet.
--
-- * 'msfrExcessCapacityTerminationPolicy' - Indicates whether running Spot Instances should be terminated if the target capacity of the Spot Fleet request is decreased below the current size of the Spot Fleet.
--
-- * 'msfrSpotFleetRequestId' - The ID of the Spot Fleet request.
modifySpotFleetRequest
    :: Text -- ^ 'msfrSpotFleetRequestId'
    -> ModifySpotFleetRequest
modifySpotFleetRequest pSpotFleetRequestId_ =
  ModifySpotFleetRequest'
    { _msfrTargetCapacity = Nothing
    , _msfrExcessCapacityTerminationPolicy = Nothing
    , _msfrSpotFleetRequestId = pSpotFleetRequestId_
    }


-- | The size of the fleet.
msfrTargetCapacity :: Lens' ModifySpotFleetRequest (Maybe Int)
msfrTargetCapacity = lens _msfrTargetCapacity (\ s a -> s{_msfrTargetCapacity = a})

-- | Indicates whether running Spot Instances should be terminated if the target capacity of the Spot Fleet request is decreased below the current size of the Spot Fleet.
msfrExcessCapacityTerminationPolicy :: Lens' ModifySpotFleetRequest (Maybe ExcessCapacityTerminationPolicy)
msfrExcessCapacityTerminationPolicy = lens _msfrExcessCapacityTerminationPolicy (\ s a -> s{_msfrExcessCapacityTerminationPolicy = a})

-- | The ID of the Spot Fleet request.
msfrSpotFleetRequestId :: Lens' ModifySpotFleetRequest Text
msfrSpotFleetRequestId = lens _msfrSpotFleetRequestId (\ s a -> s{_msfrSpotFleetRequestId = a})

instance AWSRequest ModifySpotFleetRequest where
        type Rs ModifySpotFleetRequest =
             ModifySpotFleetRequestResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 ModifySpotFleetRequestResponse' <$>
                   (x .@? "return") <*> (pure (fromEnum s)))

instance Hashable ModifySpotFleetRequest where

instance NFData ModifySpotFleetRequest where

instance ToHeaders ModifySpotFleetRequest where
        toHeaders = const mempty

instance ToPath ModifySpotFleetRequest where
        toPath = const "/"

instance ToQuery ModifySpotFleetRequest where
        toQuery ModifySpotFleetRequest'{..}
          = mconcat
              ["Action" =:
                 ("ModifySpotFleetRequest" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "TargetCapacity" =: _msfrTargetCapacity,
               "ExcessCapacityTerminationPolicy" =:
                 _msfrExcessCapacityTerminationPolicy,
               "SpotFleetRequestId" =: _msfrSpotFleetRequestId]

-- | Contains the output of ModifySpotFleetRequest.
--
--
--
-- /See:/ 'modifySpotFleetRequestResponse' smart constructor.
data ModifySpotFleetRequestResponse = ModifySpotFleetRequestResponse'
  { _msfrrsReturn         :: !(Maybe Bool)
  , _msfrrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifySpotFleetRequestResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'msfrrsReturn' - Is @true@ if the request succeeds, and an error otherwise.
--
-- * 'msfrrsResponseStatus' - -- | The response status code.
modifySpotFleetRequestResponse
    :: Int -- ^ 'msfrrsResponseStatus'
    -> ModifySpotFleetRequestResponse
modifySpotFleetRequestResponse pResponseStatus_ =
  ModifySpotFleetRequestResponse'
    {_msfrrsReturn = Nothing, _msfrrsResponseStatus = pResponseStatus_}


-- | Is @true@ if the request succeeds, and an error otherwise.
msfrrsReturn :: Lens' ModifySpotFleetRequestResponse (Maybe Bool)
msfrrsReturn = lens _msfrrsReturn (\ s a -> s{_msfrrsReturn = a})

-- | -- | The response status code.
msfrrsResponseStatus :: Lens' ModifySpotFleetRequestResponse Int
msfrrsResponseStatus = lens _msfrrsResponseStatus (\ s a -> s{_msfrrsResponseStatus = a})

instance NFData ModifySpotFleetRequestResponse where
