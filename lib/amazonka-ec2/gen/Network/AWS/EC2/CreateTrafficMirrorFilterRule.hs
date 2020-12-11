{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
-- A Traffic Mirror rule defines the Traffic Mirror source traffic to mirror.
-- You need the Traffic Mirror filter ID when you create the rule.
module Network.AWS.EC2.CreateTrafficMirrorFilterRule
  ( -- * Creating a request
    CreateTrafficMirrorFilterRule (..),
    mkCreateTrafficMirrorFilterRule,

    -- ** Request lenses
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

    -- * Destructuring the response
    CreateTrafficMirrorFilterRuleResponse (..),
    mkCreateTrafficMirrorFilterRuleResponse,

    -- ** Response lenses
    ctmfrrsTrafficMirrorFilterRule,
    ctmfrrsClientToken,
    ctmfrrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateTrafficMirrorFilterRule' smart constructor.
data CreateTrafficMirrorFilterRule = CreateTrafficMirrorFilterRule'
  { clientToken ::
      Lude.Maybe Lude.Text,
    protocol :: Lude.Maybe Lude.Int,
    destinationPortRange ::
      Lude.Maybe
        TrafficMirrorPortRangeRequest,
    sourcePortRange ::
      Lude.Maybe
        TrafficMirrorPortRangeRequest,
    description ::
      Lude.Maybe Lude.Text,
    dryRun :: Lude.Maybe Lude.Bool,
    trafficMirrorFilterId ::
      Lude.Text,
    trafficDirection ::
      TrafficDirection,
    ruleNumber :: Lude.Int,
    ruleAction ::
      TrafficMirrorRuleAction,
    destinationCidrBlock ::
      Lude.Text,
    sourceCidrBlock :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTrafficMirrorFilterRule' with the minimum fields required to make a request.
--
-- * 'clientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
-- * 'description' - The description of the Traffic Mirror rule.
-- * 'destinationCidrBlock' - The destination CIDR block to assign to the Traffic Mirror rule.
-- * 'destinationPortRange' - The destination port range.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'protocol' - The protocol, for example UDP, to assign to the Traffic Mirror rule.
--
-- For information about the protocol value, see <https://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers> on the Internet Assigned Numbers Authority (IANA) website.
-- * 'ruleAction' - The action to take (@accept@ | @reject@ ) on the filtered traffic.
-- * 'ruleNumber' - The number of the Traffic Mirror rule. This number must be unique for each Traffic Mirror rule in a given direction. The rules are processed in ascending order by rule number.
-- * 'sourceCidrBlock' - The source CIDR block to assign to the Traffic Mirror rule.
-- * 'sourcePortRange' - The source port range.
-- * 'trafficDirection' - The type of traffic (@ingress@ | @egress@ ).
-- * 'trafficMirrorFilterId' - The ID of the filter that this rule is associated with.
mkCreateTrafficMirrorFilterRule ::
  -- | 'trafficMirrorFilterId'
  Lude.Text ->
  -- | 'trafficDirection'
  TrafficDirection ->
  -- | 'ruleNumber'
  Lude.Int ->
  -- | 'ruleAction'
  TrafficMirrorRuleAction ->
  -- | 'destinationCidrBlock'
  Lude.Text ->
  -- | 'sourceCidrBlock'
  Lude.Text ->
  CreateTrafficMirrorFilterRule
mkCreateTrafficMirrorFilterRule
  pTrafficMirrorFilterId_
  pTrafficDirection_
  pRuleNumber_
  pRuleAction_
  pDestinationCidrBlock_
  pSourceCidrBlock_ =
    CreateTrafficMirrorFilterRule'
      { clientToken = Lude.Nothing,
        protocol = Lude.Nothing,
        destinationPortRange = Lude.Nothing,
        sourcePortRange = Lude.Nothing,
        description = Lude.Nothing,
        dryRun = Lude.Nothing,
        trafficMirrorFilterId = pTrafficMirrorFilterId_,
        trafficDirection = pTrafficDirection_,
        ruleNumber = pRuleNumber_,
        ruleAction = pRuleAction_,
        destinationCidrBlock = pDestinationCidrBlock_,
        sourceCidrBlock = pSourceCidrBlock_
      }

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfrClientToken :: Lens.Lens' CreateTrafficMirrorFilterRule (Lude.Maybe Lude.Text)
ctmfrClientToken = Lens.lens (clientToken :: CreateTrafficMirrorFilterRule -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: CreateTrafficMirrorFilterRule)
{-# DEPRECATED ctmfrClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The protocol, for example UDP, to assign to the Traffic Mirror rule.
--
-- For information about the protocol value, see <https://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers> on the Internet Assigned Numbers Authority (IANA) website.
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfrProtocol :: Lens.Lens' CreateTrafficMirrorFilterRule (Lude.Maybe Lude.Int)
ctmfrProtocol = Lens.lens (protocol :: CreateTrafficMirrorFilterRule -> Lude.Maybe Lude.Int) (\s a -> s {protocol = a} :: CreateTrafficMirrorFilterRule)
{-# DEPRECATED ctmfrProtocol "Use generic-lens or generic-optics with 'protocol' instead." #-}

-- | The destination port range.
--
-- /Note:/ Consider using 'destinationPortRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfrDestinationPortRange :: Lens.Lens' CreateTrafficMirrorFilterRule (Lude.Maybe TrafficMirrorPortRangeRequest)
ctmfrDestinationPortRange = Lens.lens (destinationPortRange :: CreateTrafficMirrorFilterRule -> Lude.Maybe TrafficMirrorPortRangeRequest) (\s a -> s {destinationPortRange = a} :: CreateTrafficMirrorFilterRule)
{-# DEPRECATED ctmfrDestinationPortRange "Use generic-lens or generic-optics with 'destinationPortRange' instead." #-}

-- | The source port range.
--
-- /Note:/ Consider using 'sourcePortRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfrSourcePortRange :: Lens.Lens' CreateTrafficMirrorFilterRule (Lude.Maybe TrafficMirrorPortRangeRequest)
ctmfrSourcePortRange = Lens.lens (sourcePortRange :: CreateTrafficMirrorFilterRule -> Lude.Maybe TrafficMirrorPortRangeRequest) (\s a -> s {sourcePortRange = a} :: CreateTrafficMirrorFilterRule)
{-# DEPRECATED ctmfrSourcePortRange "Use generic-lens or generic-optics with 'sourcePortRange' instead." #-}

-- | The description of the Traffic Mirror rule.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfrDescription :: Lens.Lens' CreateTrafficMirrorFilterRule (Lude.Maybe Lude.Text)
ctmfrDescription = Lens.lens (description :: CreateTrafficMirrorFilterRule -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateTrafficMirrorFilterRule)
{-# DEPRECATED ctmfrDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfrDryRun :: Lens.Lens' CreateTrafficMirrorFilterRule (Lude.Maybe Lude.Bool)
ctmfrDryRun = Lens.lens (dryRun :: CreateTrafficMirrorFilterRule -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateTrafficMirrorFilterRule)
{-# DEPRECATED ctmfrDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the filter that this rule is associated with.
--
-- /Note:/ Consider using 'trafficMirrorFilterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfrTrafficMirrorFilterId :: Lens.Lens' CreateTrafficMirrorFilterRule Lude.Text
ctmfrTrafficMirrorFilterId = Lens.lens (trafficMirrorFilterId :: CreateTrafficMirrorFilterRule -> Lude.Text) (\s a -> s {trafficMirrorFilterId = a} :: CreateTrafficMirrorFilterRule)
{-# DEPRECATED ctmfrTrafficMirrorFilterId "Use generic-lens or generic-optics with 'trafficMirrorFilterId' instead." #-}

-- | The type of traffic (@ingress@ | @egress@ ).
--
-- /Note:/ Consider using 'trafficDirection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfrTrafficDirection :: Lens.Lens' CreateTrafficMirrorFilterRule TrafficDirection
ctmfrTrafficDirection = Lens.lens (trafficDirection :: CreateTrafficMirrorFilterRule -> TrafficDirection) (\s a -> s {trafficDirection = a} :: CreateTrafficMirrorFilterRule)
{-# DEPRECATED ctmfrTrafficDirection "Use generic-lens or generic-optics with 'trafficDirection' instead." #-}

-- | The number of the Traffic Mirror rule. This number must be unique for each Traffic Mirror rule in a given direction. The rules are processed in ascending order by rule number.
--
-- /Note:/ Consider using 'ruleNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfrRuleNumber :: Lens.Lens' CreateTrafficMirrorFilterRule Lude.Int
ctmfrRuleNumber = Lens.lens (ruleNumber :: CreateTrafficMirrorFilterRule -> Lude.Int) (\s a -> s {ruleNumber = a} :: CreateTrafficMirrorFilterRule)
{-# DEPRECATED ctmfrRuleNumber "Use generic-lens or generic-optics with 'ruleNumber' instead." #-}

-- | The action to take (@accept@ | @reject@ ) on the filtered traffic.
--
-- /Note:/ Consider using 'ruleAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfrRuleAction :: Lens.Lens' CreateTrafficMirrorFilterRule TrafficMirrorRuleAction
ctmfrRuleAction = Lens.lens (ruleAction :: CreateTrafficMirrorFilterRule -> TrafficMirrorRuleAction) (\s a -> s {ruleAction = a} :: CreateTrafficMirrorFilterRule)
{-# DEPRECATED ctmfrRuleAction "Use generic-lens or generic-optics with 'ruleAction' instead." #-}

-- | The destination CIDR block to assign to the Traffic Mirror rule.
--
-- /Note:/ Consider using 'destinationCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfrDestinationCidrBlock :: Lens.Lens' CreateTrafficMirrorFilterRule Lude.Text
ctmfrDestinationCidrBlock = Lens.lens (destinationCidrBlock :: CreateTrafficMirrorFilterRule -> Lude.Text) (\s a -> s {destinationCidrBlock = a} :: CreateTrafficMirrorFilterRule)
{-# DEPRECATED ctmfrDestinationCidrBlock "Use generic-lens or generic-optics with 'destinationCidrBlock' instead." #-}

-- | The source CIDR block to assign to the Traffic Mirror rule.
--
-- /Note:/ Consider using 'sourceCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfrSourceCidrBlock :: Lens.Lens' CreateTrafficMirrorFilterRule Lude.Text
ctmfrSourceCidrBlock = Lens.lens (sourceCidrBlock :: CreateTrafficMirrorFilterRule -> Lude.Text) (\s a -> s {sourceCidrBlock = a} :: CreateTrafficMirrorFilterRule)
{-# DEPRECATED ctmfrSourceCidrBlock "Use generic-lens or generic-optics with 'sourceCidrBlock' instead." #-}

instance Lude.AWSRequest CreateTrafficMirrorFilterRule where
  type
    Rs CreateTrafficMirrorFilterRule =
      CreateTrafficMirrorFilterRuleResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateTrafficMirrorFilterRuleResponse'
            Lude.<$> (x Lude..@? "trafficMirrorFilterRule")
            Lude.<*> (x Lude..@? "clientToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateTrafficMirrorFilterRule where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateTrafficMirrorFilterRule where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateTrafficMirrorFilterRule where
  toQuery CreateTrafficMirrorFilterRule' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("CreateTrafficMirrorFilterRule" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "ClientToken" Lude.=: clientToken,
        "Protocol" Lude.=: protocol,
        "DestinationPortRange" Lude.=: destinationPortRange,
        "SourcePortRange" Lude.=: sourcePortRange,
        "Description" Lude.=: description,
        "DryRun" Lude.=: dryRun,
        "TrafficMirrorFilterId" Lude.=: trafficMirrorFilterId,
        "TrafficDirection" Lude.=: trafficDirection,
        "RuleNumber" Lude.=: ruleNumber,
        "RuleAction" Lude.=: ruleAction,
        "DestinationCidrBlock" Lude.=: destinationCidrBlock,
        "SourceCidrBlock" Lude.=: sourceCidrBlock
      ]

-- | /See:/ 'mkCreateTrafficMirrorFilterRuleResponse' smart constructor.
data CreateTrafficMirrorFilterRuleResponse = CreateTrafficMirrorFilterRuleResponse'
  { trafficMirrorFilterRule ::
      Lude.Maybe
        TrafficMirrorFilterRule,
    clientToken ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTrafficMirrorFilterRuleResponse' with the minimum fields required to make a request.
--
-- * 'clientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
-- * 'responseStatus' - The response status code.
-- * 'trafficMirrorFilterRule' - The Traffic Mirror rule.
mkCreateTrafficMirrorFilterRuleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateTrafficMirrorFilterRuleResponse
mkCreateTrafficMirrorFilterRuleResponse pResponseStatus_ =
  CreateTrafficMirrorFilterRuleResponse'
    { trafficMirrorFilterRule =
        Lude.Nothing,
      clientToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Traffic Mirror rule.
--
-- /Note:/ Consider using 'trafficMirrorFilterRule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfrrsTrafficMirrorFilterRule :: Lens.Lens' CreateTrafficMirrorFilterRuleResponse (Lude.Maybe TrafficMirrorFilterRule)
ctmfrrsTrafficMirrorFilterRule = Lens.lens (trafficMirrorFilterRule :: CreateTrafficMirrorFilterRuleResponse -> Lude.Maybe TrafficMirrorFilterRule) (\s a -> s {trafficMirrorFilterRule = a} :: CreateTrafficMirrorFilterRuleResponse)
{-# DEPRECATED ctmfrrsTrafficMirrorFilterRule "Use generic-lens or generic-optics with 'trafficMirrorFilterRule' instead." #-}

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfrrsClientToken :: Lens.Lens' CreateTrafficMirrorFilterRuleResponse (Lude.Maybe Lude.Text)
ctmfrrsClientToken = Lens.lens (clientToken :: CreateTrafficMirrorFilterRuleResponse -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: CreateTrafficMirrorFilterRuleResponse)
{-# DEPRECATED ctmfrrsClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfrrsResponseStatus :: Lens.Lens' CreateTrafficMirrorFilterRuleResponse Lude.Int
ctmfrrsResponseStatus = Lens.lens (responseStatus :: CreateTrafficMirrorFilterRuleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateTrafficMirrorFilterRuleResponse)
{-# DEPRECATED ctmfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
