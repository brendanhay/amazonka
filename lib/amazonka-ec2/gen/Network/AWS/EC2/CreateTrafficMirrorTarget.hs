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
-- Module      : Network.AWS.EC2.CreateTrafficMirrorTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a target for your Traffic Mirror session.
--
--
-- A Traffic Mirror target is the destination for mirrored traffic. The Traffic Mirror source and the Traffic Mirror target (monitoring appliances) can be in the same VPC, or in different VPCs connected via VPC peering or a transit gateway.
--
-- A Traffic Mirror target can be a network interface, or a Network Load Balancer.
--
-- To use the target in a Traffic Mirror session, use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTrafficMirrorSession.htm CreateTrafficMirrorSession> .
module Network.AWS.EC2.CreateTrafficMirrorTarget
  ( -- * Creating a Request
    createTrafficMirrorTarget,
    CreateTrafficMirrorTarget,

    -- * Request Lenses
    ctmtClientToken,
    ctmtNetworkInterfaceId,
    ctmtNetworkLoadBalancerARN,
    ctmtTagSpecifications,
    ctmtDescription,
    ctmtDryRun,

    -- * Destructuring the Response
    createTrafficMirrorTargetResponse,
    CreateTrafficMirrorTargetResponse,

    -- * Response Lenses
    ctmtrsClientToken,
    ctmtrsTrafficMirrorTarget,
    ctmtrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createTrafficMirrorTarget' smart constructor.
data CreateTrafficMirrorTarget = CreateTrafficMirrorTarget'
  { _ctmtClientToken ::
      !(Maybe Text),
    _ctmtNetworkInterfaceId ::
      !(Maybe Text),
    _ctmtNetworkLoadBalancerARN ::
      !(Maybe Text),
    _ctmtTagSpecifications ::
      !(Maybe [TagSpecification]),
    _ctmtDescription :: !(Maybe Text),
    _ctmtDryRun :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateTrafficMirrorTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctmtClientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- * 'ctmtNetworkInterfaceId' - The network interface ID that is associated with the target.
--
-- * 'ctmtNetworkLoadBalancerARN' - The Amazon Resource Name (ARN) of the Network Load Balancer that is associated with the target.
--
-- * 'ctmtTagSpecifications' - The tags to assign to the Traffic Mirror target.
--
-- * 'ctmtDescription' - The description of the Traffic Mirror target.
--
-- * 'ctmtDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
createTrafficMirrorTarget ::
  CreateTrafficMirrorTarget
createTrafficMirrorTarget =
  CreateTrafficMirrorTarget'
    { _ctmtClientToken = Nothing,
      _ctmtNetworkInterfaceId = Nothing,
      _ctmtNetworkLoadBalancerARN = Nothing,
      _ctmtTagSpecifications = Nothing,
      _ctmtDescription = Nothing,
      _ctmtDryRun = Nothing
    }

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
ctmtClientToken :: Lens' CreateTrafficMirrorTarget (Maybe Text)
ctmtClientToken = lens _ctmtClientToken (\s a -> s {_ctmtClientToken = a})

-- | The network interface ID that is associated with the target.
ctmtNetworkInterfaceId :: Lens' CreateTrafficMirrorTarget (Maybe Text)
ctmtNetworkInterfaceId = lens _ctmtNetworkInterfaceId (\s a -> s {_ctmtNetworkInterfaceId = a})

-- | The Amazon Resource Name (ARN) of the Network Load Balancer that is associated with the target.
ctmtNetworkLoadBalancerARN :: Lens' CreateTrafficMirrorTarget (Maybe Text)
ctmtNetworkLoadBalancerARN = lens _ctmtNetworkLoadBalancerARN (\s a -> s {_ctmtNetworkLoadBalancerARN = a})

-- | The tags to assign to the Traffic Mirror target.
ctmtTagSpecifications :: Lens' CreateTrafficMirrorTarget [TagSpecification]
ctmtTagSpecifications = lens _ctmtTagSpecifications (\s a -> s {_ctmtTagSpecifications = a}) . _Default . _Coerce

-- | The description of the Traffic Mirror target.
ctmtDescription :: Lens' CreateTrafficMirrorTarget (Maybe Text)
ctmtDescription = lens _ctmtDescription (\s a -> s {_ctmtDescription = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
ctmtDryRun :: Lens' CreateTrafficMirrorTarget (Maybe Bool)
ctmtDryRun = lens _ctmtDryRun (\s a -> s {_ctmtDryRun = a})

instance AWSRequest CreateTrafficMirrorTarget where
  type
    Rs CreateTrafficMirrorTarget =
      CreateTrafficMirrorTargetResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          CreateTrafficMirrorTargetResponse'
            <$> (x .@? "clientToken")
            <*> (x .@? "trafficMirrorTarget")
            <*> (pure (fromEnum s))
      )

instance Hashable CreateTrafficMirrorTarget

instance NFData CreateTrafficMirrorTarget

instance ToHeaders CreateTrafficMirrorTarget where
  toHeaders = const mempty

instance ToPath CreateTrafficMirrorTarget where
  toPath = const "/"

instance ToQuery CreateTrafficMirrorTarget where
  toQuery CreateTrafficMirrorTarget' {..} =
    mconcat
      [ "Action" =: ("CreateTrafficMirrorTarget" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "ClientToken" =: _ctmtClientToken,
        "NetworkInterfaceId" =: _ctmtNetworkInterfaceId,
        "NetworkLoadBalancerArn" =: _ctmtNetworkLoadBalancerARN,
        toQuery
          (toQueryList "TagSpecification" <$> _ctmtTagSpecifications),
        "Description" =: _ctmtDescription,
        "DryRun" =: _ctmtDryRun
      ]

-- | /See:/ 'createTrafficMirrorTargetResponse' smart constructor.
data CreateTrafficMirrorTargetResponse = CreateTrafficMirrorTargetResponse'
  { _ctmtrsClientToken ::
      !(Maybe Text),
    _ctmtrsTrafficMirrorTarget ::
      !( Maybe
           TrafficMirrorTarget
       ),
    _ctmtrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateTrafficMirrorTargetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctmtrsClientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- * 'ctmtrsTrafficMirrorTarget' - Information about the Traffic Mirror target.
--
-- * 'ctmtrsResponseStatus' - -- | The response status code.
createTrafficMirrorTargetResponse ::
  -- | 'ctmtrsResponseStatus'
  Int ->
  CreateTrafficMirrorTargetResponse
createTrafficMirrorTargetResponse pResponseStatus_ =
  CreateTrafficMirrorTargetResponse'
    { _ctmtrsClientToken = Nothing,
      _ctmtrsTrafficMirrorTarget = Nothing,
      _ctmtrsResponseStatus = pResponseStatus_
    }

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
ctmtrsClientToken :: Lens' CreateTrafficMirrorTargetResponse (Maybe Text)
ctmtrsClientToken = lens _ctmtrsClientToken (\s a -> s {_ctmtrsClientToken = a})

-- | Information about the Traffic Mirror target.
ctmtrsTrafficMirrorTarget :: Lens' CreateTrafficMirrorTargetResponse (Maybe TrafficMirrorTarget)
ctmtrsTrafficMirrorTarget = lens _ctmtrsTrafficMirrorTarget (\s a -> s {_ctmtrsTrafficMirrorTarget = a})

-- | -- | The response status code.
ctmtrsResponseStatus :: Lens' CreateTrafficMirrorTargetResponse Int
ctmtrsResponseStatus = lens _ctmtrsResponseStatus (\s a -> s {_ctmtrsResponseStatus = a})

instance NFData CreateTrafficMirrorTargetResponse
