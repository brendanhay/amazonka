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
-- Module      : Amazonka.NetworkFirewall.DescribeRuleGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the data objects for the specified rule group.
module Amazonka.NetworkFirewall.DescribeRuleGroup
  ( -- * Creating a Request
    DescribeRuleGroup (..),
    newDescribeRuleGroup,

    -- * Request Lenses
    describeRuleGroup_ruleGroupArn,
    describeRuleGroup_ruleGroupName,
    describeRuleGroup_type,

    -- * Destructuring the Response
    DescribeRuleGroupResponse (..),
    newDescribeRuleGroupResponse,

    -- * Response Lenses
    describeRuleGroupResponse_ruleGroup,
    describeRuleGroupResponse_httpStatus,
    describeRuleGroupResponse_updateToken,
    describeRuleGroupResponse_ruleGroupResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkFirewall.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeRuleGroup' smart constructor.
data DescribeRuleGroup = DescribeRuleGroup'
  { -- | The Amazon Resource Name (ARN) of the rule group.
    --
    -- You must specify the ARN or the name, and you can specify both.
    ruleGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The descriptive name of the rule group. You can\'t change the name of a
    -- rule group after you create it.
    --
    -- You must specify the ARN or the name, and you can specify both.
    ruleGroupName :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the rule group is stateless or stateful. If the rule
    -- group is stateless, it contains stateless rules. If it is stateful, it
    -- contains stateful rules.
    --
    -- This setting is required for requests that do not include the
    -- @RuleGroupARN@.
    type' :: Prelude.Maybe RuleGroupType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRuleGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleGroupArn', 'describeRuleGroup_ruleGroupArn' - The Amazon Resource Name (ARN) of the rule group.
--
-- You must specify the ARN or the name, and you can specify both.
--
-- 'ruleGroupName', 'describeRuleGroup_ruleGroupName' - The descriptive name of the rule group. You can\'t change the name of a
-- rule group after you create it.
--
-- You must specify the ARN or the name, and you can specify both.
--
-- 'type'', 'describeRuleGroup_type' - Indicates whether the rule group is stateless or stateful. If the rule
-- group is stateless, it contains stateless rules. If it is stateful, it
-- contains stateful rules.
--
-- This setting is required for requests that do not include the
-- @RuleGroupARN@.
newDescribeRuleGroup ::
  DescribeRuleGroup
newDescribeRuleGroup =
  DescribeRuleGroup'
    { ruleGroupArn = Prelude.Nothing,
      ruleGroupName = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the rule group.
--
-- You must specify the ARN or the name, and you can specify both.
describeRuleGroup_ruleGroupArn :: Lens.Lens' DescribeRuleGroup (Prelude.Maybe Prelude.Text)
describeRuleGroup_ruleGroupArn = Lens.lens (\DescribeRuleGroup' {ruleGroupArn} -> ruleGroupArn) (\s@DescribeRuleGroup' {} a -> s {ruleGroupArn = a} :: DescribeRuleGroup)

-- | The descriptive name of the rule group. You can\'t change the name of a
-- rule group after you create it.
--
-- You must specify the ARN or the name, and you can specify both.
describeRuleGroup_ruleGroupName :: Lens.Lens' DescribeRuleGroup (Prelude.Maybe Prelude.Text)
describeRuleGroup_ruleGroupName = Lens.lens (\DescribeRuleGroup' {ruleGroupName} -> ruleGroupName) (\s@DescribeRuleGroup' {} a -> s {ruleGroupName = a} :: DescribeRuleGroup)

-- | Indicates whether the rule group is stateless or stateful. If the rule
-- group is stateless, it contains stateless rules. If it is stateful, it
-- contains stateful rules.
--
-- This setting is required for requests that do not include the
-- @RuleGroupARN@.
describeRuleGroup_type :: Lens.Lens' DescribeRuleGroup (Prelude.Maybe RuleGroupType)
describeRuleGroup_type = Lens.lens (\DescribeRuleGroup' {type'} -> type') (\s@DescribeRuleGroup' {} a -> s {type' = a} :: DescribeRuleGroup)

instance Core.AWSRequest DescribeRuleGroup where
  type
    AWSResponse DescribeRuleGroup =
      DescribeRuleGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRuleGroupResponse'
            Prelude.<$> (x Data..?> "RuleGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "UpdateToken")
            Prelude.<*> (x Data..:> "RuleGroupResponse")
      )

instance Prelude.Hashable DescribeRuleGroup where
  hashWithSalt _salt DescribeRuleGroup' {..} =
    _salt
      `Prelude.hashWithSalt` ruleGroupArn
      `Prelude.hashWithSalt` ruleGroupName
      `Prelude.hashWithSalt` type'

instance Prelude.NFData DescribeRuleGroup where
  rnf DescribeRuleGroup' {..} =
    Prelude.rnf ruleGroupArn
      `Prelude.seq` Prelude.rnf ruleGroupName
      `Prelude.seq` Prelude.rnf type'

instance Data.ToHeaders DescribeRuleGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "NetworkFirewall_20201112.DescribeRuleGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeRuleGroup where
  toJSON DescribeRuleGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RuleGroupArn" Data..=) Prelude.<$> ruleGroupArn,
            ("RuleGroupName" Data..=) Prelude.<$> ruleGroupName,
            ("Type" Data..=) Prelude.<$> type'
          ]
      )

instance Data.ToPath DescribeRuleGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeRuleGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeRuleGroupResponse' smart constructor.
data DescribeRuleGroupResponse = DescribeRuleGroupResponse'
  { -- | The object that defines the rules in a rule group. This, along with
    -- RuleGroupResponse, define the rule group. You can retrieve all objects
    -- for a rule group by calling DescribeRuleGroup.
    --
    -- Network Firewall uses a rule group to inspect and control network
    -- traffic. You define stateless rule groups to inspect individual packets
    -- and you define stateful rule groups to inspect packets in the context of
    -- their traffic flow.
    --
    -- To use a rule group, you include it by reference in an Network Firewall
    -- firewall policy, then you use the policy in a firewall. You can
    -- reference a rule group from more than one firewall policy, and you can
    -- use a firewall policy in more than one firewall.
    ruleGroup :: Prelude.Maybe RuleGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A token used for optimistic locking. Network Firewall returns a token to
    -- your requests that access the rule group. The token marks the state of
    -- the rule group resource at the time of the request.
    --
    -- To make changes to the rule group, you provide the token in your
    -- request. Network Firewall uses the token to ensure that the rule group
    -- hasn\'t changed since you last retrieved it. If it has changed, the
    -- operation fails with an @InvalidTokenException@. If this happens,
    -- retrieve the rule group again to get a current copy of it with a current
    -- token. Reapply your changes as needed, then try the operation again
    -- using the new token.
    updateToken :: Prelude.Text,
    -- | The high-level properties of a rule group. This, along with the
    -- RuleGroup, define the rule group. You can retrieve all objects for a
    -- rule group by calling DescribeRuleGroup.
    ruleGroupResponse :: RuleGroupResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRuleGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleGroup', 'describeRuleGroupResponse_ruleGroup' - The object that defines the rules in a rule group. This, along with
-- RuleGroupResponse, define the rule group. You can retrieve all objects
-- for a rule group by calling DescribeRuleGroup.
--
-- Network Firewall uses a rule group to inspect and control network
-- traffic. You define stateless rule groups to inspect individual packets
-- and you define stateful rule groups to inspect packets in the context of
-- their traffic flow.
--
-- To use a rule group, you include it by reference in an Network Firewall
-- firewall policy, then you use the policy in a firewall. You can
-- reference a rule group from more than one firewall policy, and you can
-- use a firewall policy in more than one firewall.
--
-- 'httpStatus', 'describeRuleGroupResponse_httpStatus' - The response's http status code.
--
-- 'updateToken', 'describeRuleGroupResponse_updateToken' - A token used for optimistic locking. Network Firewall returns a token to
-- your requests that access the rule group. The token marks the state of
-- the rule group resource at the time of the request.
--
-- To make changes to the rule group, you provide the token in your
-- request. Network Firewall uses the token to ensure that the rule group
-- hasn\'t changed since you last retrieved it. If it has changed, the
-- operation fails with an @InvalidTokenException@. If this happens,
-- retrieve the rule group again to get a current copy of it with a current
-- token. Reapply your changes as needed, then try the operation again
-- using the new token.
--
-- 'ruleGroupResponse', 'describeRuleGroupResponse_ruleGroupResponse' - The high-level properties of a rule group. This, along with the
-- RuleGroup, define the rule group. You can retrieve all objects for a
-- rule group by calling DescribeRuleGroup.
newDescribeRuleGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'updateToken'
  Prelude.Text ->
  -- | 'ruleGroupResponse'
  RuleGroupResponse ->
  DescribeRuleGroupResponse
newDescribeRuleGroupResponse
  pHttpStatus_
  pUpdateToken_
  pRuleGroupResponse_ =
    DescribeRuleGroupResponse'
      { ruleGroup =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        updateToken = pUpdateToken_,
        ruleGroupResponse = pRuleGroupResponse_
      }

-- | The object that defines the rules in a rule group. This, along with
-- RuleGroupResponse, define the rule group. You can retrieve all objects
-- for a rule group by calling DescribeRuleGroup.
--
-- Network Firewall uses a rule group to inspect and control network
-- traffic. You define stateless rule groups to inspect individual packets
-- and you define stateful rule groups to inspect packets in the context of
-- their traffic flow.
--
-- To use a rule group, you include it by reference in an Network Firewall
-- firewall policy, then you use the policy in a firewall. You can
-- reference a rule group from more than one firewall policy, and you can
-- use a firewall policy in more than one firewall.
describeRuleGroupResponse_ruleGroup :: Lens.Lens' DescribeRuleGroupResponse (Prelude.Maybe RuleGroup)
describeRuleGroupResponse_ruleGroup = Lens.lens (\DescribeRuleGroupResponse' {ruleGroup} -> ruleGroup) (\s@DescribeRuleGroupResponse' {} a -> s {ruleGroup = a} :: DescribeRuleGroupResponse)

-- | The response's http status code.
describeRuleGroupResponse_httpStatus :: Lens.Lens' DescribeRuleGroupResponse Prelude.Int
describeRuleGroupResponse_httpStatus = Lens.lens (\DescribeRuleGroupResponse' {httpStatus} -> httpStatus) (\s@DescribeRuleGroupResponse' {} a -> s {httpStatus = a} :: DescribeRuleGroupResponse)

-- | A token used for optimistic locking. Network Firewall returns a token to
-- your requests that access the rule group. The token marks the state of
-- the rule group resource at the time of the request.
--
-- To make changes to the rule group, you provide the token in your
-- request. Network Firewall uses the token to ensure that the rule group
-- hasn\'t changed since you last retrieved it. If it has changed, the
-- operation fails with an @InvalidTokenException@. If this happens,
-- retrieve the rule group again to get a current copy of it with a current
-- token. Reapply your changes as needed, then try the operation again
-- using the new token.
describeRuleGroupResponse_updateToken :: Lens.Lens' DescribeRuleGroupResponse Prelude.Text
describeRuleGroupResponse_updateToken = Lens.lens (\DescribeRuleGroupResponse' {updateToken} -> updateToken) (\s@DescribeRuleGroupResponse' {} a -> s {updateToken = a} :: DescribeRuleGroupResponse)

-- | The high-level properties of a rule group. This, along with the
-- RuleGroup, define the rule group. You can retrieve all objects for a
-- rule group by calling DescribeRuleGroup.
describeRuleGroupResponse_ruleGroupResponse :: Lens.Lens' DescribeRuleGroupResponse RuleGroupResponse
describeRuleGroupResponse_ruleGroupResponse = Lens.lens (\DescribeRuleGroupResponse' {ruleGroupResponse} -> ruleGroupResponse) (\s@DescribeRuleGroupResponse' {} a -> s {ruleGroupResponse = a} :: DescribeRuleGroupResponse)

instance Prelude.NFData DescribeRuleGroupResponse where
  rnf DescribeRuleGroupResponse' {..} =
    Prelude.rnf ruleGroup
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf updateToken
      `Prelude.seq` Prelude.rnf ruleGroupResponse
