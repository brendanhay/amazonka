{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyTrafficMirrorFilterRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified Traffic Mirror rule.
--
-- @DestinationCidrBlock@ and @SourceCidrBlock@ must both be an IPv4 range
-- or an IPv6 range.
module Network.AWS.EC2.ModifyTrafficMirrorFilterRule
  ( -- * Creating a Request
    ModifyTrafficMirrorFilterRule (..),
    newModifyTrafficMirrorFilterRule,

    -- * Request Lenses
    modifyTrafficMirrorFilterRule_removeFields,
    modifyTrafficMirrorFilterRule_dryRun,
    modifyTrafficMirrorFilterRule_sourcePortRange,
    modifyTrafficMirrorFilterRule_trafficDirection,
    modifyTrafficMirrorFilterRule_ruleAction,
    modifyTrafficMirrorFilterRule_sourceCidrBlock,
    modifyTrafficMirrorFilterRule_destinationCidrBlock,
    modifyTrafficMirrorFilterRule_protocol,
    modifyTrafficMirrorFilterRule_description,
    modifyTrafficMirrorFilterRule_ruleNumber,
    modifyTrafficMirrorFilterRule_destinationPortRange,
    modifyTrafficMirrorFilterRule_trafficMirrorFilterRuleId,

    -- * Destructuring the Response
    ModifyTrafficMirrorFilterRuleResponse (..),
    newModifyTrafficMirrorFilterRuleResponse,

    -- * Response Lenses
    modifyTrafficMirrorFilterRuleResponse_trafficMirrorFilterRule,
    modifyTrafficMirrorFilterRuleResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyTrafficMirrorFilterRule' smart constructor.
data ModifyTrafficMirrorFilterRule = ModifyTrafficMirrorFilterRule'
  { -- | The properties that you want to remove from the Traffic Mirror filter
    -- rule.
    --
    -- When you remove a property from a Traffic Mirror filter rule, the
    -- property is set to the default.
    removeFields :: Core.Maybe [TrafficMirrorFilterRuleField],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The port range to assign to the Traffic Mirror rule.
    sourcePortRange :: Core.Maybe TrafficMirrorPortRangeRequest,
    -- | The type of traffic (@ingress@ | @egress@) to assign to the rule.
    trafficDirection :: Core.Maybe TrafficDirection,
    -- | The action to assign to the rule.
    ruleAction :: Core.Maybe TrafficMirrorRuleAction,
    -- | The source CIDR block to assign to the Traffic Mirror rule.
    sourceCidrBlock :: Core.Maybe Core.Text,
    -- | The destination CIDR block to assign to the Traffic Mirror rule.
    destinationCidrBlock :: Core.Maybe Core.Text,
    -- | The protocol, for example TCP, to assign to the Traffic Mirror rule.
    protocol :: Core.Maybe Core.Int,
    -- | The description to assign to the Traffic Mirror rule.
    description :: Core.Maybe Core.Text,
    -- | The number of the Traffic Mirror rule. This number must be unique for
    -- each Traffic Mirror rule in a given direction. The rules are processed
    -- in ascending order by rule number.
    ruleNumber :: Core.Maybe Core.Int,
    -- | The destination ports that are associated with the Traffic Mirror rule.
    destinationPortRange :: Core.Maybe TrafficMirrorPortRangeRequest,
    -- | The ID of the Traffic Mirror rule.
    trafficMirrorFilterRuleId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyTrafficMirrorFilterRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'removeFields', 'modifyTrafficMirrorFilterRule_removeFields' - The properties that you want to remove from the Traffic Mirror filter
-- rule.
--
-- When you remove a property from a Traffic Mirror filter rule, the
-- property is set to the default.
--
-- 'dryRun', 'modifyTrafficMirrorFilterRule_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'sourcePortRange', 'modifyTrafficMirrorFilterRule_sourcePortRange' - The port range to assign to the Traffic Mirror rule.
--
-- 'trafficDirection', 'modifyTrafficMirrorFilterRule_trafficDirection' - The type of traffic (@ingress@ | @egress@) to assign to the rule.
--
-- 'ruleAction', 'modifyTrafficMirrorFilterRule_ruleAction' - The action to assign to the rule.
--
-- 'sourceCidrBlock', 'modifyTrafficMirrorFilterRule_sourceCidrBlock' - The source CIDR block to assign to the Traffic Mirror rule.
--
-- 'destinationCidrBlock', 'modifyTrafficMirrorFilterRule_destinationCidrBlock' - The destination CIDR block to assign to the Traffic Mirror rule.
--
-- 'protocol', 'modifyTrafficMirrorFilterRule_protocol' - The protocol, for example TCP, to assign to the Traffic Mirror rule.
--
-- 'description', 'modifyTrafficMirrorFilterRule_description' - The description to assign to the Traffic Mirror rule.
--
-- 'ruleNumber', 'modifyTrafficMirrorFilterRule_ruleNumber' - The number of the Traffic Mirror rule. This number must be unique for
-- each Traffic Mirror rule in a given direction. The rules are processed
-- in ascending order by rule number.
--
-- 'destinationPortRange', 'modifyTrafficMirrorFilterRule_destinationPortRange' - The destination ports that are associated with the Traffic Mirror rule.
--
-- 'trafficMirrorFilterRuleId', 'modifyTrafficMirrorFilterRule_trafficMirrorFilterRuleId' - The ID of the Traffic Mirror rule.
newModifyTrafficMirrorFilterRule ::
  -- | 'trafficMirrorFilterRuleId'
  Core.Text ->
  ModifyTrafficMirrorFilterRule
newModifyTrafficMirrorFilterRule
  pTrafficMirrorFilterRuleId_ =
    ModifyTrafficMirrorFilterRule'
      { removeFields =
          Core.Nothing,
        dryRun = Core.Nothing,
        sourcePortRange = Core.Nothing,
        trafficDirection = Core.Nothing,
        ruleAction = Core.Nothing,
        sourceCidrBlock = Core.Nothing,
        destinationCidrBlock = Core.Nothing,
        protocol = Core.Nothing,
        description = Core.Nothing,
        ruleNumber = Core.Nothing,
        destinationPortRange = Core.Nothing,
        trafficMirrorFilterRuleId =
          pTrafficMirrorFilterRuleId_
      }

-- | The properties that you want to remove from the Traffic Mirror filter
-- rule.
--
-- When you remove a property from a Traffic Mirror filter rule, the
-- property is set to the default.
modifyTrafficMirrorFilterRule_removeFields :: Lens.Lens' ModifyTrafficMirrorFilterRule (Core.Maybe [TrafficMirrorFilterRuleField])
modifyTrafficMirrorFilterRule_removeFields = Lens.lens (\ModifyTrafficMirrorFilterRule' {removeFields} -> removeFields) (\s@ModifyTrafficMirrorFilterRule' {} a -> s {removeFields = a} :: ModifyTrafficMirrorFilterRule) Core.. Lens.mapping Lens._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyTrafficMirrorFilterRule_dryRun :: Lens.Lens' ModifyTrafficMirrorFilterRule (Core.Maybe Core.Bool)
modifyTrafficMirrorFilterRule_dryRun = Lens.lens (\ModifyTrafficMirrorFilterRule' {dryRun} -> dryRun) (\s@ModifyTrafficMirrorFilterRule' {} a -> s {dryRun = a} :: ModifyTrafficMirrorFilterRule)

-- | The port range to assign to the Traffic Mirror rule.
modifyTrafficMirrorFilterRule_sourcePortRange :: Lens.Lens' ModifyTrafficMirrorFilterRule (Core.Maybe TrafficMirrorPortRangeRequest)
modifyTrafficMirrorFilterRule_sourcePortRange = Lens.lens (\ModifyTrafficMirrorFilterRule' {sourcePortRange} -> sourcePortRange) (\s@ModifyTrafficMirrorFilterRule' {} a -> s {sourcePortRange = a} :: ModifyTrafficMirrorFilterRule)

-- | The type of traffic (@ingress@ | @egress@) to assign to the rule.
modifyTrafficMirrorFilterRule_trafficDirection :: Lens.Lens' ModifyTrafficMirrorFilterRule (Core.Maybe TrafficDirection)
modifyTrafficMirrorFilterRule_trafficDirection = Lens.lens (\ModifyTrafficMirrorFilterRule' {trafficDirection} -> trafficDirection) (\s@ModifyTrafficMirrorFilterRule' {} a -> s {trafficDirection = a} :: ModifyTrafficMirrorFilterRule)

-- | The action to assign to the rule.
modifyTrafficMirrorFilterRule_ruleAction :: Lens.Lens' ModifyTrafficMirrorFilterRule (Core.Maybe TrafficMirrorRuleAction)
modifyTrafficMirrorFilterRule_ruleAction = Lens.lens (\ModifyTrafficMirrorFilterRule' {ruleAction} -> ruleAction) (\s@ModifyTrafficMirrorFilterRule' {} a -> s {ruleAction = a} :: ModifyTrafficMirrorFilterRule)

-- | The source CIDR block to assign to the Traffic Mirror rule.
modifyTrafficMirrorFilterRule_sourceCidrBlock :: Lens.Lens' ModifyTrafficMirrorFilterRule (Core.Maybe Core.Text)
modifyTrafficMirrorFilterRule_sourceCidrBlock = Lens.lens (\ModifyTrafficMirrorFilterRule' {sourceCidrBlock} -> sourceCidrBlock) (\s@ModifyTrafficMirrorFilterRule' {} a -> s {sourceCidrBlock = a} :: ModifyTrafficMirrorFilterRule)

-- | The destination CIDR block to assign to the Traffic Mirror rule.
modifyTrafficMirrorFilterRule_destinationCidrBlock :: Lens.Lens' ModifyTrafficMirrorFilterRule (Core.Maybe Core.Text)
modifyTrafficMirrorFilterRule_destinationCidrBlock = Lens.lens (\ModifyTrafficMirrorFilterRule' {destinationCidrBlock} -> destinationCidrBlock) (\s@ModifyTrafficMirrorFilterRule' {} a -> s {destinationCidrBlock = a} :: ModifyTrafficMirrorFilterRule)

-- | The protocol, for example TCP, to assign to the Traffic Mirror rule.
modifyTrafficMirrorFilterRule_protocol :: Lens.Lens' ModifyTrafficMirrorFilterRule (Core.Maybe Core.Int)
modifyTrafficMirrorFilterRule_protocol = Lens.lens (\ModifyTrafficMirrorFilterRule' {protocol} -> protocol) (\s@ModifyTrafficMirrorFilterRule' {} a -> s {protocol = a} :: ModifyTrafficMirrorFilterRule)

-- | The description to assign to the Traffic Mirror rule.
modifyTrafficMirrorFilterRule_description :: Lens.Lens' ModifyTrafficMirrorFilterRule (Core.Maybe Core.Text)
modifyTrafficMirrorFilterRule_description = Lens.lens (\ModifyTrafficMirrorFilterRule' {description} -> description) (\s@ModifyTrafficMirrorFilterRule' {} a -> s {description = a} :: ModifyTrafficMirrorFilterRule)

-- | The number of the Traffic Mirror rule. This number must be unique for
-- each Traffic Mirror rule in a given direction. The rules are processed
-- in ascending order by rule number.
modifyTrafficMirrorFilterRule_ruleNumber :: Lens.Lens' ModifyTrafficMirrorFilterRule (Core.Maybe Core.Int)
modifyTrafficMirrorFilterRule_ruleNumber = Lens.lens (\ModifyTrafficMirrorFilterRule' {ruleNumber} -> ruleNumber) (\s@ModifyTrafficMirrorFilterRule' {} a -> s {ruleNumber = a} :: ModifyTrafficMirrorFilterRule)

-- | The destination ports that are associated with the Traffic Mirror rule.
modifyTrafficMirrorFilterRule_destinationPortRange :: Lens.Lens' ModifyTrafficMirrorFilterRule (Core.Maybe TrafficMirrorPortRangeRequest)
modifyTrafficMirrorFilterRule_destinationPortRange = Lens.lens (\ModifyTrafficMirrorFilterRule' {destinationPortRange} -> destinationPortRange) (\s@ModifyTrafficMirrorFilterRule' {} a -> s {destinationPortRange = a} :: ModifyTrafficMirrorFilterRule)

-- | The ID of the Traffic Mirror rule.
modifyTrafficMirrorFilterRule_trafficMirrorFilterRuleId :: Lens.Lens' ModifyTrafficMirrorFilterRule Core.Text
modifyTrafficMirrorFilterRule_trafficMirrorFilterRuleId = Lens.lens (\ModifyTrafficMirrorFilterRule' {trafficMirrorFilterRuleId} -> trafficMirrorFilterRuleId) (\s@ModifyTrafficMirrorFilterRule' {} a -> s {trafficMirrorFilterRuleId = a} :: ModifyTrafficMirrorFilterRule)

instance
  Core.AWSRequest
    ModifyTrafficMirrorFilterRule
  where
  type
    AWSResponse ModifyTrafficMirrorFilterRule =
      ModifyTrafficMirrorFilterRuleResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyTrafficMirrorFilterRuleResponse'
            Core.<$> (x Core..@? "trafficMirrorFilterRule")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ModifyTrafficMirrorFilterRule

instance Core.NFData ModifyTrafficMirrorFilterRule

instance Core.ToHeaders ModifyTrafficMirrorFilterRule where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ModifyTrafficMirrorFilterRule where
  toPath = Core.const "/"

instance Core.ToQuery ModifyTrafficMirrorFilterRule where
  toQuery ModifyTrafficMirrorFilterRule' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ModifyTrafficMirrorFilterRule" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        Core.toQuery
          ( Core.toQueryList "RemoveField"
              Core.<$> removeFields
          ),
        "DryRun" Core.=: dryRun,
        "SourcePortRange" Core.=: sourcePortRange,
        "TrafficDirection" Core.=: trafficDirection,
        "RuleAction" Core.=: ruleAction,
        "SourceCidrBlock" Core.=: sourceCidrBlock,
        "DestinationCidrBlock" Core.=: destinationCidrBlock,
        "Protocol" Core.=: protocol,
        "Description" Core.=: description,
        "RuleNumber" Core.=: ruleNumber,
        "DestinationPortRange" Core.=: destinationPortRange,
        "TrafficMirrorFilterRuleId"
          Core.=: trafficMirrorFilterRuleId
      ]

-- | /See:/ 'newModifyTrafficMirrorFilterRuleResponse' smart constructor.
data ModifyTrafficMirrorFilterRuleResponse = ModifyTrafficMirrorFilterRuleResponse'
  { -- | Modifies a Traffic Mirror rule.
    trafficMirrorFilterRule :: Core.Maybe TrafficMirrorFilterRule,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyTrafficMirrorFilterRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trafficMirrorFilterRule', 'modifyTrafficMirrorFilterRuleResponse_trafficMirrorFilterRule' - Modifies a Traffic Mirror rule.
--
-- 'httpStatus', 'modifyTrafficMirrorFilterRuleResponse_httpStatus' - The response's http status code.
newModifyTrafficMirrorFilterRuleResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ModifyTrafficMirrorFilterRuleResponse
newModifyTrafficMirrorFilterRuleResponse pHttpStatus_ =
  ModifyTrafficMirrorFilterRuleResponse'
    { trafficMirrorFilterRule =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Modifies a Traffic Mirror rule.
modifyTrafficMirrorFilterRuleResponse_trafficMirrorFilterRule :: Lens.Lens' ModifyTrafficMirrorFilterRuleResponse (Core.Maybe TrafficMirrorFilterRule)
modifyTrafficMirrorFilterRuleResponse_trafficMirrorFilterRule = Lens.lens (\ModifyTrafficMirrorFilterRuleResponse' {trafficMirrorFilterRule} -> trafficMirrorFilterRule) (\s@ModifyTrafficMirrorFilterRuleResponse' {} a -> s {trafficMirrorFilterRule = a} :: ModifyTrafficMirrorFilterRuleResponse)

-- | The response's http status code.
modifyTrafficMirrorFilterRuleResponse_httpStatus :: Lens.Lens' ModifyTrafficMirrorFilterRuleResponse Core.Int
modifyTrafficMirrorFilterRuleResponse_httpStatus = Lens.lens (\ModifyTrafficMirrorFilterRuleResponse' {httpStatus} -> httpStatus) (\s@ModifyTrafficMirrorFilterRuleResponse' {} a -> s {httpStatus = a} :: ModifyTrafficMirrorFilterRuleResponse)

instance
  Core.NFData
    ModifyTrafficMirrorFilterRuleResponse
