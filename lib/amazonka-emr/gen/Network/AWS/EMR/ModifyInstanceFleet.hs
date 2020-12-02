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
-- Module      : Network.AWS.EMR.ModifyInstanceFleet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the target On-Demand and target Spot capacities for the instance fleet with the specified InstanceFleetID within the cluster specified using ClusterID. The call either succeeds or fails atomically.
--
--
module Network.AWS.EMR.ModifyInstanceFleet
    (
    -- * Creating a Request
      modifyInstanceFleet
    , ModifyInstanceFleet
    -- * Request Lenses
    , mifClusterId
    , mifInstanceFleet

    -- * Destructuring the Response
    , modifyInstanceFleetResponse
    , ModifyInstanceFleetResponse
    ) where

import Network.AWS.EMR.Types
import Network.AWS.EMR.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'modifyInstanceFleet' smart constructor.
data ModifyInstanceFleet = ModifyInstanceFleet'
  { _mifClusterId     :: !Text
  , _mifInstanceFleet :: !InstanceFleetModifyConfig
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyInstanceFleet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mifClusterId' - The unique identifier of the cluster.
--
-- * 'mifInstanceFleet' - The unique identifier of the instance fleet.
modifyInstanceFleet
    :: Text -- ^ 'mifClusterId'
    -> InstanceFleetModifyConfig -- ^ 'mifInstanceFleet'
    -> ModifyInstanceFleet
modifyInstanceFleet pClusterId_ pInstanceFleet_ =
  ModifyInstanceFleet'
    {_mifClusterId = pClusterId_, _mifInstanceFleet = pInstanceFleet_}


-- | The unique identifier of the cluster.
mifClusterId :: Lens' ModifyInstanceFleet Text
mifClusterId = lens _mifClusterId (\ s a -> s{_mifClusterId = a})

-- | The unique identifier of the instance fleet.
mifInstanceFleet :: Lens' ModifyInstanceFleet InstanceFleetModifyConfig
mifInstanceFleet = lens _mifInstanceFleet (\ s a -> s{_mifInstanceFleet = a})

instance AWSRequest ModifyInstanceFleet where
        type Rs ModifyInstanceFleet =
             ModifyInstanceFleetResponse
        request = postJSON emr
        response = receiveNull ModifyInstanceFleetResponse'

instance Hashable ModifyInstanceFleet where

instance NFData ModifyInstanceFleet where

instance ToHeaders ModifyInstanceFleet where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("ElasticMapReduce.ModifyInstanceFleet" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ModifyInstanceFleet where
        toJSON ModifyInstanceFleet'{..}
          = object
              (catMaybes
                 [Just ("ClusterId" .= _mifClusterId),
                  Just ("InstanceFleet" .= _mifInstanceFleet)])

instance ToPath ModifyInstanceFleet where
        toPath = const "/"

instance ToQuery ModifyInstanceFleet where
        toQuery = const mempty

-- | /See:/ 'modifyInstanceFleetResponse' smart constructor.
data ModifyInstanceFleetResponse =
  ModifyInstanceFleetResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyInstanceFleetResponse' with the minimum fields required to make a request.
--
modifyInstanceFleetResponse
    :: ModifyInstanceFleetResponse
modifyInstanceFleetResponse = ModifyInstanceFleetResponse'


instance NFData ModifyInstanceFleetResponse where
