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
-- Module      : Network.AWS.EC2.ModifyTrafficMirrorSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a Traffic Mirror session.
module Network.AWS.EC2.ModifyTrafficMirrorSession
  ( -- * Creating a Request
    modifyTrafficMirrorSession,
    ModifyTrafficMirrorSession,

    -- * Request Lenses
    mtmsRemoveFields,
    mtmsTrafficMirrorTargetId,
    mtmsTrafficMirrorFilterId,
    mtmsPacketLength,
    mtmsVirtualNetworkId,
    mtmsSessionNumber,
    mtmsDescription,
    mtmsDryRun,
    mtmsTrafficMirrorSessionId,

    -- * Destructuring the Response
    modifyTrafficMirrorSessionResponse,
    ModifyTrafficMirrorSessionResponse,

    -- * Response Lenses
    mtmsrsTrafficMirrorSession,
    mtmsrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'modifyTrafficMirrorSession' smart constructor.
data ModifyTrafficMirrorSession = ModifyTrafficMirrorSession'
  { _mtmsRemoveFields ::
      !(Maybe [TrafficMirrorSessionField]),
    _mtmsTrafficMirrorTargetId ::
      !(Maybe Text),
    _mtmsTrafficMirrorFilterId ::
      !(Maybe Text),
    _mtmsPacketLength :: !(Maybe Int),
    _mtmsVirtualNetworkId :: !(Maybe Int),
    _mtmsSessionNumber :: !(Maybe Int),
    _mtmsDescription :: !(Maybe Text),
    _mtmsDryRun :: !(Maybe Bool),
    _mtmsTrafficMirrorSessionId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifyTrafficMirrorSession' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mtmsRemoveFields' - The properties that you want to remove from the Traffic Mirror session. When you remove a property from a Traffic Mirror session, the property is set to the default.
--
-- * 'mtmsTrafficMirrorTargetId' - The Traffic Mirror target. The target must be in the same VPC as the source, or have a VPC peering connection with the source.
--
-- * 'mtmsTrafficMirrorFilterId' - The ID of the Traffic Mirror filter.
--
-- * 'mtmsPacketLength' - The number of bytes in each packet to mirror. These are bytes after the VXLAN header. To mirror a subset, set this to the length (in bytes) to mirror. For example, if you set this value to 100, then the first 100 bytes that meet the filter criteria are copied to the target. Do not specify this parameter when you want to mirror the entire packet.
--
-- * 'mtmsVirtualNetworkId' - The virtual network ID of the Traffic Mirror session.
--
-- * 'mtmsSessionNumber' - The session number determines the order in which sessions are evaluated when an interface is used by multiple sessions. The first session with a matching filter is the one that mirrors the packets. Valid values are 1-32766.
--
-- * 'mtmsDescription' - The description to assign to the Traffic Mirror session.
--
-- * 'mtmsDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'mtmsTrafficMirrorSessionId' - The ID of the Traffic Mirror session.
modifyTrafficMirrorSession ::
  -- | 'mtmsTrafficMirrorSessionId'
  Text ->
  ModifyTrafficMirrorSession
modifyTrafficMirrorSession pTrafficMirrorSessionId_ =
  ModifyTrafficMirrorSession'
    { _mtmsRemoveFields = Nothing,
      _mtmsTrafficMirrorTargetId = Nothing,
      _mtmsTrafficMirrorFilterId = Nothing,
      _mtmsPacketLength = Nothing,
      _mtmsVirtualNetworkId = Nothing,
      _mtmsSessionNumber = Nothing,
      _mtmsDescription = Nothing,
      _mtmsDryRun = Nothing,
      _mtmsTrafficMirrorSessionId = pTrafficMirrorSessionId_
    }

-- | The properties that you want to remove from the Traffic Mirror session. When you remove a property from a Traffic Mirror session, the property is set to the default.
mtmsRemoveFields :: Lens' ModifyTrafficMirrorSession [TrafficMirrorSessionField]
mtmsRemoveFields = lens _mtmsRemoveFields (\s a -> s {_mtmsRemoveFields = a}) . _Default . _Coerce

-- | The Traffic Mirror target. The target must be in the same VPC as the source, or have a VPC peering connection with the source.
mtmsTrafficMirrorTargetId :: Lens' ModifyTrafficMirrorSession (Maybe Text)
mtmsTrafficMirrorTargetId = lens _mtmsTrafficMirrorTargetId (\s a -> s {_mtmsTrafficMirrorTargetId = a})

-- | The ID of the Traffic Mirror filter.
mtmsTrafficMirrorFilterId :: Lens' ModifyTrafficMirrorSession (Maybe Text)
mtmsTrafficMirrorFilterId = lens _mtmsTrafficMirrorFilterId (\s a -> s {_mtmsTrafficMirrorFilterId = a})

-- | The number of bytes in each packet to mirror. These are bytes after the VXLAN header. To mirror a subset, set this to the length (in bytes) to mirror. For example, if you set this value to 100, then the first 100 bytes that meet the filter criteria are copied to the target. Do not specify this parameter when you want to mirror the entire packet.
mtmsPacketLength :: Lens' ModifyTrafficMirrorSession (Maybe Int)
mtmsPacketLength = lens _mtmsPacketLength (\s a -> s {_mtmsPacketLength = a})

-- | The virtual network ID of the Traffic Mirror session.
mtmsVirtualNetworkId :: Lens' ModifyTrafficMirrorSession (Maybe Int)
mtmsVirtualNetworkId = lens _mtmsVirtualNetworkId (\s a -> s {_mtmsVirtualNetworkId = a})

-- | The session number determines the order in which sessions are evaluated when an interface is used by multiple sessions. The first session with a matching filter is the one that mirrors the packets. Valid values are 1-32766.
mtmsSessionNumber :: Lens' ModifyTrafficMirrorSession (Maybe Int)
mtmsSessionNumber = lens _mtmsSessionNumber (\s a -> s {_mtmsSessionNumber = a})

-- | The description to assign to the Traffic Mirror session.
mtmsDescription :: Lens' ModifyTrafficMirrorSession (Maybe Text)
mtmsDescription = lens _mtmsDescription (\s a -> s {_mtmsDescription = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mtmsDryRun :: Lens' ModifyTrafficMirrorSession (Maybe Bool)
mtmsDryRun = lens _mtmsDryRun (\s a -> s {_mtmsDryRun = a})

-- | The ID of the Traffic Mirror session.
mtmsTrafficMirrorSessionId :: Lens' ModifyTrafficMirrorSession Text
mtmsTrafficMirrorSessionId = lens _mtmsTrafficMirrorSessionId (\s a -> s {_mtmsTrafficMirrorSessionId = a})

instance AWSRequest ModifyTrafficMirrorSession where
  type
    Rs ModifyTrafficMirrorSession =
      ModifyTrafficMirrorSessionResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          ModifyTrafficMirrorSessionResponse'
            <$> (x .@? "trafficMirrorSession") <*> (pure (fromEnum s))
      )

instance Hashable ModifyTrafficMirrorSession

instance NFData ModifyTrafficMirrorSession

instance ToHeaders ModifyTrafficMirrorSession where
  toHeaders = const mempty

instance ToPath ModifyTrafficMirrorSession where
  toPath = const "/"

instance ToQuery ModifyTrafficMirrorSession where
  toQuery ModifyTrafficMirrorSession' {..} =
    mconcat
      [ "Action" =: ("ModifyTrafficMirrorSession" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        toQuery (toQueryList "RemoveField" <$> _mtmsRemoveFields),
        "TrafficMirrorTargetId" =: _mtmsTrafficMirrorTargetId,
        "TrafficMirrorFilterId" =: _mtmsTrafficMirrorFilterId,
        "PacketLength" =: _mtmsPacketLength,
        "VirtualNetworkId" =: _mtmsVirtualNetworkId,
        "SessionNumber" =: _mtmsSessionNumber,
        "Description" =: _mtmsDescription,
        "DryRun" =: _mtmsDryRun,
        "TrafficMirrorSessionId" =: _mtmsTrafficMirrorSessionId
      ]

-- | /See:/ 'modifyTrafficMirrorSessionResponse' smart constructor.
data ModifyTrafficMirrorSessionResponse = ModifyTrafficMirrorSessionResponse'
  { _mtmsrsTrafficMirrorSession ::
      !( Maybe
           TrafficMirrorSession
       ),
    _mtmsrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifyTrafficMirrorSessionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mtmsrsTrafficMirrorSession' - Information about the Traffic Mirror session.
--
-- * 'mtmsrsResponseStatus' - -- | The response status code.
modifyTrafficMirrorSessionResponse ::
  -- | 'mtmsrsResponseStatus'
  Int ->
  ModifyTrafficMirrorSessionResponse
modifyTrafficMirrorSessionResponse pResponseStatus_ =
  ModifyTrafficMirrorSessionResponse'
    { _mtmsrsTrafficMirrorSession =
        Nothing,
      _mtmsrsResponseStatus = pResponseStatus_
    }

-- | Information about the Traffic Mirror session.
mtmsrsTrafficMirrorSession :: Lens' ModifyTrafficMirrorSessionResponse (Maybe TrafficMirrorSession)
mtmsrsTrafficMirrorSession = lens _mtmsrsTrafficMirrorSession (\s a -> s {_mtmsrsTrafficMirrorSession = a})

-- | -- | The response status code.
mtmsrsResponseStatus :: Lens' ModifyTrafficMirrorSessionResponse Int
mtmsrsResponseStatus = lens _mtmsrsResponseStatus (\s a -> s {_mtmsrsResponseStatus = a})

instance NFData ModifyTrafficMirrorSessionResponse
