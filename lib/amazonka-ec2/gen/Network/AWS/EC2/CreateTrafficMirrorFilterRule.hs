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
-- Module      : Network.AWS.EC2.CreateTrafficMirrorFilterRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Traffic Mirror filter rule.
--
--
-- A Traffic Mirror rule defines the Traffic Mirror source traffic to mirror.
--
-- You need the Traffic Mirror filter ID when you create the rule.
module Network.AWS.EC2.CreateTrafficMirrorFilterRule
  ( -- * Creating a Request
    createTrafficMirrorFilterRule,
    CreateTrafficMirrorFilterRule,

    -- * Request Lenses
    ctmfrClientToken,
    ctmfrProtocol,
    ctmfrDestinationPortRange,
    ctmfrSourcePortRange,
    ctmfrDescription,
    ctmfrDryRun,
    ctmfrTrafficMirrorFilterId,
    ctmfrTrafficDirection,
    ctmfrRuleNumber,
    ctmfrRuleAction,
    ctmfrDestinationCidrBlock,
    ctmfrSourceCidrBlock,

    -- * Destructuring the Response
    createTrafficMirrorFilterRuleResponse,
    CreateTrafficMirrorFilterRuleResponse,

    -- * Response Lenses
    ctmfrrsTrafficMirrorFilterRule,
    ctmfrrsClientToken,
    ctmfrrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createTrafficMirrorFilterRule' smart constructor.
data CreateTrafficMirrorFilterRule = CreateTrafficMirrorFilterRule'
  { _ctmfrClientToken ::
      !(Maybe Text),
    _ctmfrProtocol :: !(Maybe Int),
    _ctmfrDestinationPortRange ::
      !( Maybe
           TrafficMirrorPortRangeRequest
       ),
    _ctmfrSourcePortRange ::
      !( Maybe
           TrafficMirrorPortRangeRequest
       ),
    _ctmfrDescription ::
      !(Maybe Text),
    _ctmfrDryRun :: !(Maybe Bool),
    _ctmfrTrafficMirrorFilterId ::
      !Text,
    _ctmfrTrafficDirection ::
      !TrafficDirection,
    _ctmfrRuleNumber :: !Int,
    _ctmfrRuleAction ::
      !TrafficMirrorRuleAction,
    _ctmfrDestinationCidrBlock ::
      !Text,
    _ctmfrSourceCidrBlock :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateTrafficMirrorFilterRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctmfrClientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- * 'ctmfrProtocol' - The protocol, for example UDP, to assign to the Traffic Mirror rule. For information about the protocol value, see <https://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers> on the Internet Assigned Numbers Authority (IANA) website.
--
-- * 'ctmfrDestinationPortRange' - The destination port range.
--
-- * 'ctmfrSourcePortRange' - The source port range.
--
-- * 'ctmfrDescription' - The description of the Traffic Mirror rule.
--
-- * 'ctmfrDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'ctmfrTrafficMirrorFilterId' - The ID of the filter that this rule is associated with.
--
-- * 'ctmfrTrafficDirection' - The type of traffic (@ingress@ | @egress@ ).
--
-- * 'ctmfrRuleNumber' - The number of the Traffic Mirror rule. This number must be unique for each Traffic Mirror rule in a given direction. The rules are processed in ascending order by rule number.
--
-- * 'ctmfrRuleAction' - The action to take (@accept@ | @reject@ ) on the filtered traffic.
--
-- * 'ctmfrDestinationCidrBlock' - The destination CIDR block to assign to the Traffic Mirror rule.
--
-- * 'ctmfrSourceCidrBlock' - The source CIDR block to assign to the Traffic Mirror rule.
createTrafficMirrorFilterRule ::
  -- | 'ctmfrTrafficMirrorFilterId'
  Text ->
  -- | 'ctmfrTrafficDirection'
  TrafficDirection ->
  -- | 'ctmfrRuleNumber'
  Int ->
  -- | 'ctmfrRuleAction'
  TrafficMirrorRuleAction ->
  -- | 'ctmfrDestinationCidrBlock'
  Text ->
  -- | 'ctmfrSourceCidrBlock'
  Text ->
  CreateTrafficMirrorFilterRule
createTrafficMirrorFilterRule
  pTrafficMirrorFilterId_
  pTrafficDirection_
  pRuleNumber_
  pRuleAction_
  pDestinationCidrBlock_
  pSourceCidrBlock_ =
    CreateTrafficMirrorFilterRule'
      { _ctmfrClientToken = Nothing,
        _ctmfrProtocol = Nothing,
        _ctmfrDestinationPortRange = Nothing,
        _ctmfrSourcePortRange = Nothing,
        _ctmfrDescription = Nothing,
        _ctmfrDryRun = Nothing,
        _ctmfrTrafficMirrorFilterId = pTrafficMirrorFilterId_,
        _ctmfrTrafficDirection = pTrafficDirection_,
        _ctmfrRuleNumber = pRuleNumber_,
        _ctmfrRuleAction = pRuleAction_,
        _ctmfrDestinationCidrBlock = pDestinationCidrBlock_,
        _ctmfrSourceCidrBlock = pSourceCidrBlock_
      }

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
ctmfrClientToken :: Lens' CreateTrafficMirrorFilterRule (Maybe Text)
ctmfrClientToken = lens _ctmfrClientToken (\s a -> s {_ctmfrClientToken = a})

-- | The protocol, for example UDP, to assign to the Traffic Mirror rule. For information about the protocol value, see <https://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers> on the Internet Assigned Numbers Authority (IANA) website.
ctmfrProtocol :: Lens' CreateTrafficMirrorFilterRule (Maybe Int)
ctmfrProtocol = lens _ctmfrProtocol (\s a -> s {_ctmfrProtocol = a})

-- | The destination port range.
ctmfrDestinationPortRange :: Lens' CreateTrafficMirrorFilterRule (Maybe TrafficMirrorPortRangeRequest)
ctmfrDestinationPortRange = lens _ctmfrDestinationPortRange (\s a -> s {_ctmfrDestinationPortRange = a})

-- | The source port range.
ctmfrSourcePortRange :: Lens' CreateTrafficMirrorFilterRule (Maybe TrafficMirrorPortRangeRequest)
ctmfrSourcePortRange = lens _ctmfrSourcePortRange (\s a -> s {_ctmfrSourcePortRange = a})

-- | The description of the Traffic Mirror rule.
ctmfrDescription :: Lens' CreateTrafficMirrorFilterRule (Maybe Text)
ctmfrDescription = lens _ctmfrDescription (\s a -> s {_ctmfrDescription = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
ctmfrDryRun :: Lens' CreateTrafficMirrorFilterRule (Maybe Bool)
ctmfrDryRun = lens _ctmfrDryRun (\s a -> s {_ctmfrDryRun = a})

-- | The ID of the filter that this rule is associated with.
ctmfrTrafficMirrorFilterId :: Lens' CreateTrafficMirrorFilterRule Text
ctmfrTrafficMirrorFilterId = lens _ctmfrTrafficMirrorFilterId (\s a -> s {_ctmfrTrafficMirrorFilterId = a})

-- | The type of traffic (@ingress@ | @egress@ ).
ctmfrTrafficDirection :: Lens' CreateTrafficMirrorFilterRule TrafficDirection
ctmfrTrafficDirection = lens _ctmfrTrafficDirection (\s a -> s {_ctmfrTrafficDirection = a})

-- | The number of the Traffic Mirror rule. This number must be unique for each Traffic Mirror rule in a given direction. The rules are processed in ascending order by rule number.
ctmfrRuleNumber :: Lens' CreateTrafficMirrorFilterRule Int
ctmfrRuleNumber = lens _ctmfrRuleNumber (\s a -> s {_ctmfrRuleNumber = a})

-- | The action to take (@accept@ | @reject@ ) on the filtered traffic.
ctmfrRuleAction :: Lens' CreateTrafficMirrorFilterRule TrafficMirrorRuleAction
ctmfrRuleAction = lens _ctmfrRuleAction (\s a -> s {_ctmfrRuleAction = a})

-- | The destination CIDR block to assign to the Traffic Mirror rule.
ctmfrDestinationCidrBlock :: Lens' CreateTrafficMirrorFilterRule Text
ctmfrDestinationCidrBlock = lens _ctmfrDestinationCidrBlock (\s a -> s {_ctmfrDestinationCidrBlock = a})

-- | The source CIDR block to assign to the Traffic Mirror rule.
ctmfrSourceCidrBlock :: Lens' CreateTrafficMirrorFilterRule Text
ctmfrSourceCidrBlock = lens _ctmfrSourceCidrBlock (\s a -> s {_ctmfrSourceCidrBlock = a})

instance AWSRequest CreateTrafficMirrorFilterRule where
  type
    Rs CreateTrafficMirrorFilterRule =
      CreateTrafficMirrorFilterRuleResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          CreateTrafficMirrorFilterRuleResponse'
            <$> (x .@? "trafficMirrorFilterRule")
            <*> (x .@? "clientToken")
            <*> (pure (fromEnum s))
      )

instance Hashable CreateTrafficMirrorFilterRule

instance NFData CreateTrafficMirrorFilterRule

instance ToHeaders CreateTrafficMirrorFilterRule where
  toHeaders = const mempty

instance ToPath CreateTrafficMirrorFilterRule where
  toPath = const "/"

instance ToQuery CreateTrafficMirrorFilterRule where
  toQuery CreateTrafficMirrorFilterRule' {..} =
    mconcat
      [ "Action" =: ("CreateTrafficMirrorFilterRule" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "ClientToken" =: _ctmfrClientToken,
        "Protocol" =: _ctmfrProtocol,
        "DestinationPortRange" =: _ctmfrDestinationPortRange,
        "SourcePortRange" =: _ctmfrSourcePortRange,
        "Description" =: _ctmfrDescription,
        "DryRun" =: _ctmfrDryRun,
        "TrafficMirrorFilterId" =: _ctmfrTrafficMirrorFilterId,
        "TrafficDirection" =: _ctmfrTrafficDirection,
        "RuleNumber" =: _ctmfrRuleNumber,
        "RuleAction" =: _ctmfrRuleAction,
        "DestinationCidrBlock" =: _ctmfrDestinationCidrBlock,
        "SourceCidrBlock" =: _ctmfrSourceCidrBlock
      ]

-- | /See:/ 'createTrafficMirrorFilterRuleResponse' smart constructor.
data CreateTrafficMirrorFilterRuleResponse = CreateTrafficMirrorFilterRuleResponse'
  { _ctmfrrsTrafficMirrorFilterRule ::
      !( Maybe
           TrafficMirrorFilterRule
       ),
    _ctmfrrsClientToken ::
      !(Maybe Text),
    _ctmfrrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateTrafficMirrorFilterRuleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctmfrrsTrafficMirrorFilterRule' - The Traffic Mirror rule.
--
-- * 'ctmfrrsClientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- * 'ctmfrrsResponseStatus' - -- | The response status code.
createTrafficMirrorFilterRuleResponse ::
  -- | 'ctmfrrsResponseStatus'
  Int ->
  CreateTrafficMirrorFilterRuleResponse
createTrafficMirrorFilterRuleResponse pResponseStatus_ =
  CreateTrafficMirrorFilterRuleResponse'
    { _ctmfrrsTrafficMirrorFilterRule =
        Nothing,
      _ctmfrrsClientToken = Nothing,
      _ctmfrrsResponseStatus = pResponseStatus_
    }

-- | The Traffic Mirror rule.
ctmfrrsTrafficMirrorFilterRule :: Lens' CreateTrafficMirrorFilterRuleResponse (Maybe TrafficMirrorFilterRule)
ctmfrrsTrafficMirrorFilterRule = lens _ctmfrrsTrafficMirrorFilterRule (\s a -> s {_ctmfrrsTrafficMirrorFilterRule = a})

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
ctmfrrsClientToken :: Lens' CreateTrafficMirrorFilterRuleResponse (Maybe Text)
ctmfrrsClientToken = lens _ctmfrrsClientToken (\s a -> s {_ctmfrrsClientToken = a})

-- | -- | The response status code.
ctmfrrsResponseStatus :: Lens' CreateTrafficMirrorFilterRuleResponse Int
ctmfrrsResponseStatus = lens _ctmfrrsResponseStatus (\s a -> s {_ctmfrrsResponseStatus = a})

instance NFData CreateTrafficMirrorFilterRuleResponse
