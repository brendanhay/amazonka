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
-- Module      : Network.AWS.ELBv2.DeleteRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified rule.
--
-- You can\'t delete the default rule.
module Network.AWS.ELBv2.DeleteRule
  ( -- * Creating a Request
    DeleteRule (..),
    newDeleteRule,

    -- * Request Lenses
    deleteRule_ruleArn,

    -- * Destructuring the Response
    DeleteRuleResponse (..),
    newDeleteRuleResponse,

    -- * Response Lenses
    deleteRuleResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteRule' smart constructor.
data DeleteRule = DeleteRule'
  { -- | The Amazon Resource Name (ARN) of the rule.
    ruleArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleArn', 'deleteRule_ruleArn' - The Amazon Resource Name (ARN) of the rule.
newDeleteRule ::
  -- | 'ruleArn'
  Core.Text ->
  DeleteRule
newDeleteRule pRuleArn_ =
  DeleteRule' {ruleArn = pRuleArn_}

-- | The Amazon Resource Name (ARN) of the rule.
deleteRule_ruleArn :: Lens.Lens' DeleteRule Core.Text
deleteRule_ruleArn = Lens.lens (\DeleteRule' {ruleArn} -> ruleArn) (\s@DeleteRule' {} a -> s {ruleArn = a} :: DeleteRule)

instance Core.AWSRequest DeleteRule where
  type AWSResponse DeleteRule = DeleteRuleResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DeleteRuleResult"
      ( \s h x ->
          DeleteRuleResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteRule

instance Core.NFData DeleteRule

instance Core.ToHeaders DeleteRule where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteRule where
  toPath = Core.const "/"

instance Core.ToQuery DeleteRule where
  toQuery DeleteRule' {..} =
    Core.mconcat
      [ "Action" Core.=: ("DeleteRule" :: Core.ByteString),
        "Version" Core.=: ("2015-12-01" :: Core.ByteString),
        "RuleArn" Core.=: ruleArn
      ]

-- | /See:/ 'newDeleteRuleResponse' smart constructor.
data DeleteRuleResponse = DeleteRuleResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteRuleResponse_httpStatus' - The response's http status code.
newDeleteRuleResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteRuleResponse
newDeleteRuleResponse pHttpStatus_ =
  DeleteRuleResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteRuleResponse_httpStatus :: Lens.Lens' DeleteRuleResponse Core.Int
deleteRuleResponse_httpStatus = Lens.lens (\DeleteRuleResponse' {httpStatus} -> httpStatus) (\s@DeleteRuleResponse' {} a -> s {httpStatus = a} :: DeleteRuleResponse)

instance Core.NFData DeleteRuleResponse
