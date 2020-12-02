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
-- Module      : Network.AWS.EC2.CreateTrafficMirrorSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Traffic Mirror session.
--
--
-- A Traffic Mirror session actively copies packets from a Traffic Mirror source to a Traffic Mirror target. Create a filter, and then assign it to the session to define a subset of the traffic to mirror, for example all TCP traffic.
--
-- The Traffic Mirror source and the Traffic Mirror target (monitoring appliances) can be in the same VPC, or in a different VPC connected via VPC peering or a transit gateway.
--
-- By default, no traffic is mirrored. Use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTrafficMirrorFilter.htm CreateTrafficMirrorFilter> to create filter rules that specify the traffic to mirror.
module Network.AWS.EC2.CreateTrafficMirrorSession
  ( -- * Creating a Request
    createTrafficMirrorSession,
    CreateTrafficMirrorSession,

    -- * Request Lenses
    ctmsClientToken,
    ctmsPacketLength,
    ctmsTagSpecifications,
    ctmsVirtualNetworkId,
    ctmsDescription,
    ctmsDryRun,
    ctmsNetworkInterfaceId,
    ctmsTrafficMirrorTargetId,
    ctmsTrafficMirrorFilterId,
    ctmsSessionNumber,

    -- * Destructuring the Response
    createTrafficMirrorSessionResponse,
    CreateTrafficMirrorSessionResponse,

    -- * Response Lenses
    ctmsrsTrafficMirrorSession,
    ctmsrsClientToken,
    ctmsrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createTrafficMirrorSession' smart constructor.
data CreateTrafficMirrorSession = CreateTrafficMirrorSession'
  { _ctmsClientToken ::
      !(Maybe Text),
    _ctmsPacketLength :: !(Maybe Int),
    _ctmsTagSpecifications ::
      !(Maybe [TagSpecification]),
    _ctmsVirtualNetworkId :: !(Maybe Int),
    _ctmsDescription :: !(Maybe Text),
    _ctmsDryRun :: !(Maybe Bool),
    _ctmsNetworkInterfaceId :: !Text,
    _ctmsTrafficMirrorTargetId :: !Text,
    _ctmsTrafficMirrorFilterId :: !Text,
    _ctmsSessionNumber :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateTrafficMirrorSession' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctmsClientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- * 'ctmsPacketLength' - The number of bytes in each packet to mirror. These are bytes after the VXLAN header. Do not specify this parameter when you want to mirror the entire packet. To mirror a subset of the packet, set this to the length (in bytes) that you want to mirror. For example, if you set this value to 100, then the first 100 bytes that meet the filter criteria are copied to the target. If you do not want to mirror the entire packet, use the @PacketLength@ parameter to specify the number of bytes in each packet to mirror.
--
-- * 'ctmsTagSpecifications' - The tags to assign to a Traffic Mirror session.
--
-- * 'ctmsVirtualNetworkId' - The VXLAN ID for the Traffic Mirror session. For more information about the VXLAN protocol, see <https://tools.ietf.org/html/rfc7348 RFC 7348> . If you do not specify a @VirtualNetworkId@ , an account-wide unique id is chosen at random.
--
-- * 'ctmsDescription' - The description of the Traffic Mirror session.
--
-- * 'ctmsDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'ctmsNetworkInterfaceId' - The ID of the source network interface.
--
-- * 'ctmsTrafficMirrorTargetId' - The ID of the Traffic Mirror target.
--
-- * 'ctmsTrafficMirrorFilterId' - The ID of the Traffic Mirror filter.
--
-- * 'ctmsSessionNumber' - The session number determines the order in which sessions are evaluated when an interface is used by multiple sessions. The first session with a matching filter is the one that mirrors the packets. Valid values are 1-32766.
createTrafficMirrorSession ::
  -- | 'ctmsNetworkInterfaceId'
  Text ->
  -- | 'ctmsTrafficMirrorTargetId'
  Text ->
  -- | 'ctmsTrafficMirrorFilterId'
  Text ->
  -- | 'ctmsSessionNumber'
  Int ->
  CreateTrafficMirrorSession
createTrafficMirrorSession
  pNetworkInterfaceId_
  pTrafficMirrorTargetId_
  pTrafficMirrorFilterId_
  pSessionNumber_ =
    CreateTrafficMirrorSession'
      { _ctmsClientToken = Nothing,
        _ctmsPacketLength = Nothing,
        _ctmsTagSpecifications = Nothing,
        _ctmsVirtualNetworkId = Nothing,
        _ctmsDescription = Nothing,
        _ctmsDryRun = Nothing,
        _ctmsNetworkInterfaceId = pNetworkInterfaceId_,
        _ctmsTrafficMirrorTargetId = pTrafficMirrorTargetId_,
        _ctmsTrafficMirrorFilterId = pTrafficMirrorFilterId_,
        _ctmsSessionNumber = pSessionNumber_
      }

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
ctmsClientToken :: Lens' CreateTrafficMirrorSession (Maybe Text)
ctmsClientToken = lens _ctmsClientToken (\s a -> s {_ctmsClientToken = a})

-- | The number of bytes in each packet to mirror. These are bytes after the VXLAN header. Do not specify this parameter when you want to mirror the entire packet. To mirror a subset of the packet, set this to the length (in bytes) that you want to mirror. For example, if you set this value to 100, then the first 100 bytes that meet the filter criteria are copied to the target. If you do not want to mirror the entire packet, use the @PacketLength@ parameter to specify the number of bytes in each packet to mirror.
ctmsPacketLength :: Lens' CreateTrafficMirrorSession (Maybe Int)
ctmsPacketLength = lens _ctmsPacketLength (\s a -> s {_ctmsPacketLength = a})

-- | The tags to assign to a Traffic Mirror session.
ctmsTagSpecifications :: Lens' CreateTrafficMirrorSession [TagSpecification]
ctmsTagSpecifications = lens _ctmsTagSpecifications (\s a -> s {_ctmsTagSpecifications = a}) . _Default . _Coerce

-- | The VXLAN ID for the Traffic Mirror session. For more information about the VXLAN protocol, see <https://tools.ietf.org/html/rfc7348 RFC 7348> . If you do not specify a @VirtualNetworkId@ , an account-wide unique id is chosen at random.
ctmsVirtualNetworkId :: Lens' CreateTrafficMirrorSession (Maybe Int)
ctmsVirtualNetworkId = lens _ctmsVirtualNetworkId (\s a -> s {_ctmsVirtualNetworkId = a})

-- | The description of the Traffic Mirror session.
ctmsDescription :: Lens' CreateTrafficMirrorSession (Maybe Text)
ctmsDescription = lens _ctmsDescription (\s a -> s {_ctmsDescription = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
ctmsDryRun :: Lens' CreateTrafficMirrorSession (Maybe Bool)
ctmsDryRun = lens _ctmsDryRun (\s a -> s {_ctmsDryRun = a})

-- | The ID of the source network interface.
ctmsNetworkInterfaceId :: Lens' CreateTrafficMirrorSession Text
ctmsNetworkInterfaceId = lens _ctmsNetworkInterfaceId (\s a -> s {_ctmsNetworkInterfaceId = a})

-- | The ID of the Traffic Mirror target.
ctmsTrafficMirrorTargetId :: Lens' CreateTrafficMirrorSession Text
ctmsTrafficMirrorTargetId = lens _ctmsTrafficMirrorTargetId (\s a -> s {_ctmsTrafficMirrorTargetId = a})

-- | The ID of the Traffic Mirror filter.
ctmsTrafficMirrorFilterId :: Lens' CreateTrafficMirrorSession Text
ctmsTrafficMirrorFilterId = lens _ctmsTrafficMirrorFilterId (\s a -> s {_ctmsTrafficMirrorFilterId = a})

-- | The session number determines the order in which sessions are evaluated when an interface is used by multiple sessions. The first session with a matching filter is the one that mirrors the packets. Valid values are 1-32766.
ctmsSessionNumber :: Lens' CreateTrafficMirrorSession Int
ctmsSessionNumber = lens _ctmsSessionNumber (\s a -> s {_ctmsSessionNumber = a})

instance AWSRequest CreateTrafficMirrorSession where
  type
    Rs CreateTrafficMirrorSession =
      CreateTrafficMirrorSessionResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          CreateTrafficMirrorSessionResponse'
            <$> (x .@? "trafficMirrorSession")
            <*> (x .@? "clientToken")
            <*> (pure (fromEnum s))
      )

instance Hashable CreateTrafficMirrorSession

instance NFData CreateTrafficMirrorSession

instance ToHeaders CreateTrafficMirrorSession where
  toHeaders = const mempty

instance ToPath CreateTrafficMirrorSession where
  toPath = const "/"

instance ToQuery CreateTrafficMirrorSession where
  toQuery CreateTrafficMirrorSession' {..} =
    mconcat
      [ "Action" =: ("CreateTrafficMirrorSession" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "ClientToken" =: _ctmsClientToken,
        "PacketLength" =: _ctmsPacketLength,
        toQuery
          (toQueryList "TagSpecification" <$> _ctmsTagSpecifications),
        "VirtualNetworkId" =: _ctmsVirtualNetworkId,
        "Description" =: _ctmsDescription,
        "DryRun" =: _ctmsDryRun,
        "NetworkInterfaceId" =: _ctmsNetworkInterfaceId,
        "TrafficMirrorTargetId" =: _ctmsTrafficMirrorTargetId,
        "TrafficMirrorFilterId" =: _ctmsTrafficMirrorFilterId,
        "SessionNumber" =: _ctmsSessionNumber
      ]

-- | /See:/ 'createTrafficMirrorSessionResponse' smart constructor.
data CreateTrafficMirrorSessionResponse = CreateTrafficMirrorSessionResponse'
  { _ctmsrsTrafficMirrorSession ::
      !( Maybe
           TrafficMirrorSession
       ),
    _ctmsrsClientToken ::
      !(Maybe Text),
    _ctmsrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateTrafficMirrorSessionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctmsrsTrafficMirrorSession' - Information about the Traffic Mirror session.
--
-- * 'ctmsrsClientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- * 'ctmsrsResponseStatus' - -- | The response status code.
createTrafficMirrorSessionResponse ::
  -- | 'ctmsrsResponseStatus'
  Int ->
  CreateTrafficMirrorSessionResponse
createTrafficMirrorSessionResponse pResponseStatus_ =
  CreateTrafficMirrorSessionResponse'
    { _ctmsrsTrafficMirrorSession =
        Nothing,
      _ctmsrsClientToken = Nothing,
      _ctmsrsResponseStatus = pResponseStatus_
    }

-- | Information about the Traffic Mirror session.
ctmsrsTrafficMirrorSession :: Lens' CreateTrafficMirrorSessionResponse (Maybe TrafficMirrorSession)
ctmsrsTrafficMirrorSession = lens _ctmsrsTrafficMirrorSession (\s a -> s {_ctmsrsTrafficMirrorSession = a})

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
ctmsrsClientToken :: Lens' CreateTrafficMirrorSessionResponse (Maybe Text)
ctmsrsClientToken = lens _ctmsrsClientToken (\s a -> s {_ctmsrsClientToken = a})

-- | -- | The response status code.
ctmsrsResponseStatus :: Lens' CreateTrafficMirrorSessionResponse Int
ctmsrsResponseStatus = lens _ctmsrsResponseStatus (\s a -> s {_ctmsrsResponseStatus = a})

instance NFData CreateTrafficMirrorSessionResponse
