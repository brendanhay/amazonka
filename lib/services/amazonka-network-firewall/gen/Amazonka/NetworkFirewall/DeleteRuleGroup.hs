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
-- Module      : Amazonka.NetworkFirewall.DeleteRuleGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified RuleGroup.
module Amazonka.NetworkFirewall.DeleteRuleGroup
  ( -- * Creating a Request
    DeleteRuleGroup (..),
    newDeleteRuleGroup,

    -- * Request Lenses
    deleteRuleGroup_ruleGroupName,
    deleteRuleGroup_type,
    deleteRuleGroup_ruleGroupArn,

    -- * Destructuring the Response
    DeleteRuleGroupResponse (..),
    newDeleteRuleGroupResponse,

    -- * Response Lenses
    deleteRuleGroupResponse_httpStatus,
    deleteRuleGroupResponse_ruleGroupResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkFirewall.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteRuleGroup' smart constructor.
data DeleteRuleGroup = DeleteRuleGroup'
  { -- | The descriptive name of the rule group. You can\'t change the name of a
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
    type' :: Prelude.Maybe RuleGroupType,
    -- | The Amazon Resource Name (ARN) of the rule group.
    --
    -- You must specify the ARN or the name, and you can specify both.
    ruleGroupArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRuleGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleGroupName', 'deleteRuleGroup_ruleGroupName' - The descriptive name of the rule group. You can\'t change the name of a
-- rule group after you create it.
--
-- You must specify the ARN or the name, and you can specify both.
--
-- 'type'', 'deleteRuleGroup_type' - Indicates whether the rule group is stateless or stateful. If the rule
-- group is stateless, it contains stateless rules. If it is stateful, it
-- contains stateful rules.
--
-- This setting is required for requests that do not include the
-- @RuleGroupARN@.
--
-- 'ruleGroupArn', 'deleteRuleGroup_ruleGroupArn' - The Amazon Resource Name (ARN) of the rule group.
--
-- You must specify the ARN or the name, and you can specify both.
newDeleteRuleGroup ::
  DeleteRuleGroup
newDeleteRuleGroup =
  DeleteRuleGroup'
    { ruleGroupName = Prelude.Nothing,
      type' = Prelude.Nothing,
      ruleGroupArn = Prelude.Nothing
    }

-- | The descriptive name of the rule group. You can\'t change the name of a
-- rule group after you create it.
--
-- You must specify the ARN or the name, and you can specify both.
deleteRuleGroup_ruleGroupName :: Lens.Lens' DeleteRuleGroup (Prelude.Maybe Prelude.Text)
deleteRuleGroup_ruleGroupName = Lens.lens (\DeleteRuleGroup' {ruleGroupName} -> ruleGroupName) (\s@DeleteRuleGroup' {} a -> s {ruleGroupName = a} :: DeleteRuleGroup)

-- | Indicates whether the rule group is stateless or stateful. If the rule
-- group is stateless, it contains stateless rules. If it is stateful, it
-- contains stateful rules.
--
-- This setting is required for requests that do not include the
-- @RuleGroupARN@.
deleteRuleGroup_type :: Lens.Lens' DeleteRuleGroup (Prelude.Maybe RuleGroupType)
deleteRuleGroup_type = Lens.lens (\DeleteRuleGroup' {type'} -> type') (\s@DeleteRuleGroup' {} a -> s {type' = a} :: DeleteRuleGroup)

-- | The Amazon Resource Name (ARN) of the rule group.
--
-- You must specify the ARN or the name, and you can specify both.
deleteRuleGroup_ruleGroupArn :: Lens.Lens' DeleteRuleGroup (Prelude.Maybe Prelude.Text)
deleteRuleGroup_ruleGroupArn = Lens.lens (\DeleteRuleGroup' {ruleGroupArn} -> ruleGroupArn) (\s@DeleteRuleGroup' {} a -> s {ruleGroupArn = a} :: DeleteRuleGroup)

instance Core.AWSRequest DeleteRuleGroup where
  type
    AWSResponse DeleteRuleGroup =
      DeleteRuleGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteRuleGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "RuleGroupResponse")
      )

instance Prelude.Hashable DeleteRuleGroup where
  hashWithSalt _salt DeleteRuleGroup' {..} =
    _salt `Prelude.hashWithSalt` ruleGroupName
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` ruleGroupArn

instance Prelude.NFData DeleteRuleGroup where
  rnf DeleteRuleGroup' {..} =
    Prelude.rnf ruleGroupName
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf ruleGroupArn

instance Data.ToHeaders DeleteRuleGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "NetworkFirewall_20201112.DeleteRuleGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteRuleGroup where
  toJSON DeleteRuleGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RuleGroupName" Data..=) Prelude.<$> ruleGroupName,
            ("Type" Data..=) Prelude.<$> type',
            ("RuleGroupArn" Data..=) Prelude.<$> ruleGroupArn
          ]
      )

instance Data.ToPath DeleteRuleGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteRuleGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRuleGroupResponse' smart constructor.
data DeleteRuleGroupResponse = DeleteRuleGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The high-level properties of a rule group. This, along with the
    -- RuleGroup, define the rule group. You can retrieve all objects for a
    -- rule group by calling DescribeRuleGroup.
    ruleGroupResponse :: RuleGroupResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRuleGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteRuleGroupResponse_httpStatus' - The response's http status code.
--
-- 'ruleGroupResponse', 'deleteRuleGroupResponse_ruleGroupResponse' - The high-level properties of a rule group. This, along with the
-- RuleGroup, define the rule group. You can retrieve all objects for a
-- rule group by calling DescribeRuleGroup.
newDeleteRuleGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'ruleGroupResponse'
  RuleGroupResponse ->
  DeleteRuleGroupResponse
newDeleteRuleGroupResponse
  pHttpStatus_
  pRuleGroupResponse_ =
    DeleteRuleGroupResponse'
      { httpStatus = pHttpStatus_,
        ruleGroupResponse = pRuleGroupResponse_
      }

-- | The response's http status code.
deleteRuleGroupResponse_httpStatus :: Lens.Lens' DeleteRuleGroupResponse Prelude.Int
deleteRuleGroupResponse_httpStatus = Lens.lens (\DeleteRuleGroupResponse' {httpStatus} -> httpStatus) (\s@DeleteRuleGroupResponse' {} a -> s {httpStatus = a} :: DeleteRuleGroupResponse)

-- | The high-level properties of a rule group. This, along with the
-- RuleGroup, define the rule group. You can retrieve all objects for a
-- rule group by calling DescribeRuleGroup.
deleteRuleGroupResponse_ruleGroupResponse :: Lens.Lens' DeleteRuleGroupResponse RuleGroupResponse
deleteRuleGroupResponse_ruleGroupResponse = Lens.lens (\DeleteRuleGroupResponse' {ruleGroupResponse} -> ruleGroupResponse) (\s@DeleteRuleGroupResponse' {} a -> s {ruleGroupResponse = a} :: DeleteRuleGroupResponse)

instance Prelude.NFData DeleteRuleGroupResponse where
  rnf DeleteRuleGroupResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf ruleGroupResponse
