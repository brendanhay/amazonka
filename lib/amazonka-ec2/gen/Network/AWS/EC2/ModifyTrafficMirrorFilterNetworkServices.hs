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
-- Module      : Network.AWS.EC2.ModifyTrafficMirrorFilterNetworkServices
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows or restricts mirroring network services.
--
--
-- By default, Amazon DNS network services are not eligible for Traffic Mirror. Use @AddNetworkServices@ to add network services to a Traffic Mirror filter. When a network service is added to the Traffic Mirror filter, all traffic related to that network service will be mirrored. When you no longer want to mirror network services, use @RemoveNetworkServices@ to remove the network services from the Traffic Mirror filter.
--
-- For information about filter rule properties, see <https://docs.aws.amazon.com/vpc/latest/mirroring/traffic-mirroring-considerations.html Network Services> in the /Traffic Mirroring User Guide / .
module Network.AWS.EC2.ModifyTrafficMirrorFilterNetworkServices
  ( -- * Creating a Request
    modifyTrafficMirrorFilterNetworkServices,
    ModifyTrafficMirrorFilterNetworkServices,

    -- * Request Lenses
    mtmfnsAddNetworkServices,
    mtmfnsRemoveNetworkServices,
    mtmfnsDryRun,
    mtmfnsTrafficMirrorFilterId,

    -- * Destructuring the Response
    modifyTrafficMirrorFilterNetworkServicesResponse,
    ModifyTrafficMirrorFilterNetworkServicesResponse,

    -- * Response Lenses
    mtmfnsrsTrafficMirrorFilter,
    mtmfnsrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'modifyTrafficMirrorFilterNetworkServices' smart constructor.
data ModifyTrafficMirrorFilterNetworkServices = ModifyTrafficMirrorFilterNetworkServices'
  { _mtmfnsAddNetworkServices ::
      !( Maybe
           [TrafficMirrorNetworkService]
       ),
    _mtmfnsRemoveNetworkServices ::
      !( Maybe
           [TrafficMirrorNetworkService]
       ),
    _mtmfnsDryRun ::
      !( Maybe
           Bool
       ),
    _mtmfnsTrafficMirrorFilterId ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifyTrafficMirrorFilterNetworkServices' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mtmfnsAddNetworkServices' - The network service, for example Amazon DNS, that you want to mirror.
--
-- * 'mtmfnsRemoveNetworkServices' - The network service, for example Amazon DNS, that you no longer want to mirror.
--
-- * 'mtmfnsDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'mtmfnsTrafficMirrorFilterId' - The ID of the Traffic Mirror filter.
modifyTrafficMirrorFilterNetworkServices ::
  -- | 'mtmfnsTrafficMirrorFilterId'
  Text ->
  ModifyTrafficMirrorFilterNetworkServices
modifyTrafficMirrorFilterNetworkServices pTrafficMirrorFilterId_ =
  ModifyTrafficMirrorFilterNetworkServices'
    { _mtmfnsAddNetworkServices =
        Nothing,
      _mtmfnsRemoveNetworkServices = Nothing,
      _mtmfnsDryRun = Nothing,
      _mtmfnsTrafficMirrorFilterId =
        pTrafficMirrorFilterId_
    }

-- | The network service, for example Amazon DNS, that you want to mirror.
mtmfnsAddNetworkServices :: Lens' ModifyTrafficMirrorFilterNetworkServices [TrafficMirrorNetworkService]
mtmfnsAddNetworkServices = lens _mtmfnsAddNetworkServices (\s a -> s {_mtmfnsAddNetworkServices = a}) . _Default . _Coerce

-- | The network service, for example Amazon DNS, that you no longer want to mirror.
mtmfnsRemoveNetworkServices :: Lens' ModifyTrafficMirrorFilterNetworkServices [TrafficMirrorNetworkService]
mtmfnsRemoveNetworkServices = lens _mtmfnsRemoveNetworkServices (\s a -> s {_mtmfnsRemoveNetworkServices = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mtmfnsDryRun :: Lens' ModifyTrafficMirrorFilterNetworkServices (Maybe Bool)
mtmfnsDryRun = lens _mtmfnsDryRun (\s a -> s {_mtmfnsDryRun = a})

-- | The ID of the Traffic Mirror filter.
mtmfnsTrafficMirrorFilterId :: Lens' ModifyTrafficMirrorFilterNetworkServices Text
mtmfnsTrafficMirrorFilterId = lens _mtmfnsTrafficMirrorFilterId (\s a -> s {_mtmfnsTrafficMirrorFilterId = a})

instance AWSRequest ModifyTrafficMirrorFilterNetworkServices where
  type
    Rs ModifyTrafficMirrorFilterNetworkServices =
      ModifyTrafficMirrorFilterNetworkServicesResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          ModifyTrafficMirrorFilterNetworkServicesResponse'
            <$> (x .@? "trafficMirrorFilter") <*> (pure (fromEnum s))
      )

instance Hashable ModifyTrafficMirrorFilterNetworkServices

instance NFData ModifyTrafficMirrorFilterNetworkServices

instance ToHeaders ModifyTrafficMirrorFilterNetworkServices where
  toHeaders = const mempty

instance ToPath ModifyTrafficMirrorFilterNetworkServices where
  toPath = const "/"

instance ToQuery ModifyTrafficMirrorFilterNetworkServices where
  toQuery ModifyTrafficMirrorFilterNetworkServices' {..} =
    mconcat
      [ "Action"
          =: ("ModifyTrafficMirrorFilterNetworkServices" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        toQuery
          (toQueryList "AddNetworkService" <$> _mtmfnsAddNetworkServices),
        toQuery
          ( toQueryList "RemoveNetworkService"
              <$> _mtmfnsRemoveNetworkServices
          ),
        "DryRun" =: _mtmfnsDryRun,
        "TrafficMirrorFilterId" =: _mtmfnsTrafficMirrorFilterId
      ]

-- | /See:/ 'modifyTrafficMirrorFilterNetworkServicesResponse' smart constructor.
data ModifyTrafficMirrorFilterNetworkServicesResponse = ModifyTrafficMirrorFilterNetworkServicesResponse'
  { _mtmfnsrsTrafficMirrorFilter ::
      !( Maybe
           TrafficMirrorFilter
       ),
    _mtmfnsrsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'ModifyTrafficMirrorFilterNetworkServicesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mtmfnsrsTrafficMirrorFilter' - The Traffic Mirror filter that the network service is associated with.
--
-- * 'mtmfnsrsResponseStatus' - -- | The response status code.
modifyTrafficMirrorFilterNetworkServicesResponse ::
  -- | 'mtmfnsrsResponseStatus'
  Int ->
  ModifyTrafficMirrorFilterNetworkServicesResponse
modifyTrafficMirrorFilterNetworkServicesResponse pResponseStatus_ =
  ModifyTrafficMirrorFilterNetworkServicesResponse'
    { _mtmfnsrsTrafficMirrorFilter =
        Nothing,
      _mtmfnsrsResponseStatus = pResponseStatus_
    }

-- | The Traffic Mirror filter that the network service is associated with.
mtmfnsrsTrafficMirrorFilter :: Lens' ModifyTrafficMirrorFilterNetworkServicesResponse (Maybe TrafficMirrorFilter)
mtmfnsrsTrafficMirrorFilter = lens _mtmfnsrsTrafficMirrorFilter (\s a -> s {_mtmfnsrsTrafficMirrorFilter = a})

-- | -- | The response status code.
mtmfnsrsResponseStatus :: Lens' ModifyTrafficMirrorFilterNetworkServicesResponse Int
mtmfnsrsResponseStatus = lens _mtmfnsrsResponseStatus (\s a -> s {_mtmfnsrsResponseStatus = a})

instance NFData ModifyTrafficMirrorFilterNetworkServicesResponse
