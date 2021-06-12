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
-- Module      : Network.AWS.WAF.DeleteRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- Permanently deletes a Rule. You can\'t delete a @Rule@ if it\'s still
-- used in any @WebACL@ objects or if it still includes any predicates,
-- such as @ByteMatchSet@ objects.
--
-- If you just want to remove a @Rule@ from a @WebACL@, use UpdateWebACL.
--
-- To permanently delete a @Rule@ from AWS WAF, perform the following
-- steps:
--
-- 1.  Update the @Rule@ to remove predicates, if any. For more
--     information, see UpdateRule.
--
-- 2.  Use GetChangeToken to get the change token that you provide in the
--     @ChangeToken@ parameter of a @DeleteRule@ request.
--
-- 3.  Submit a @DeleteRule@ request.
module Network.AWS.WAF.DeleteRule
  ( -- * Creating a Request
    DeleteRule (..),
    newDeleteRule,

    -- * Request Lenses
    deleteRule_ruleId,
    deleteRule_changeToken,

    -- * Destructuring the Response
    DeleteRuleResponse (..),
    newDeleteRuleResponse,

    -- * Response Lenses
    deleteRuleResponse_changeToken,
    deleteRuleResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAF.Types

-- | /See:/ 'newDeleteRule' smart constructor.
data DeleteRule = DeleteRule'
  { -- | The @RuleId@ of the Rule that you want to delete. @RuleId@ is returned
    -- by CreateRule and by ListRules.
    ruleId :: Core.Text,
    -- | The value returned by the most recent call to GetChangeToken.
    changeToken :: Core.Text
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
-- 'ruleId', 'deleteRule_ruleId' - The @RuleId@ of the Rule that you want to delete. @RuleId@ is returned
-- by CreateRule and by ListRules.
--
-- 'changeToken', 'deleteRule_changeToken' - The value returned by the most recent call to GetChangeToken.
newDeleteRule ::
  -- | 'ruleId'
  Core.Text ->
  -- | 'changeToken'
  Core.Text ->
  DeleteRule
newDeleteRule pRuleId_ pChangeToken_ =
  DeleteRule'
    { ruleId = pRuleId_,
      changeToken = pChangeToken_
    }

-- | The @RuleId@ of the Rule that you want to delete. @RuleId@ is returned
-- by CreateRule and by ListRules.
deleteRule_ruleId :: Lens.Lens' DeleteRule Core.Text
deleteRule_ruleId = Lens.lens (\DeleteRule' {ruleId} -> ruleId) (\s@DeleteRule' {} a -> s {ruleId = a} :: DeleteRule)

-- | The value returned by the most recent call to GetChangeToken.
deleteRule_changeToken :: Lens.Lens' DeleteRule Core.Text
deleteRule_changeToken = Lens.lens (\DeleteRule' {changeToken} -> changeToken) (\s@DeleteRule' {} a -> s {changeToken = a} :: DeleteRule)

instance Core.AWSRequest DeleteRule where
  type AWSResponse DeleteRule = DeleteRuleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteRuleResponse'
            Core.<$> (x Core..?> "ChangeToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteRule

instance Core.NFData DeleteRule

instance Core.ToHeaders DeleteRule where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSWAF_20150824.DeleteRule" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteRule where
  toJSON DeleteRule' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("RuleId" Core..= ruleId),
            Core.Just ("ChangeToken" Core..= changeToken)
          ]
      )

instance Core.ToPath DeleteRule where
  toPath = Core.const "/"

instance Core.ToQuery DeleteRule where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteRuleResponse' smart constructor.
data DeleteRuleResponse = DeleteRuleResponse'
  { -- | The @ChangeToken@ that you used to submit the @DeleteRule@ request. You
    -- can also use this value to query the status of the request. For more
    -- information, see GetChangeTokenStatus.
    changeToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
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
-- 'changeToken', 'deleteRuleResponse_changeToken' - The @ChangeToken@ that you used to submit the @DeleteRule@ request. You
-- can also use this value to query the status of the request. For more
-- information, see GetChangeTokenStatus.
--
-- 'httpStatus', 'deleteRuleResponse_httpStatus' - The response's http status code.
newDeleteRuleResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteRuleResponse
newDeleteRuleResponse pHttpStatus_ =
  DeleteRuleResponse'
    { changeToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @ChangeToken@ that you used to submit the @DeleteRule@ request. You
-- can also use this value to query the status of the request. For more
-- information, see GetChangeTokenStatus.
deleteRuleResponse_changeToken :: Lens.Lens' DeleteRuleResponse (Core.Maybe Core.Text)
deleteRuleResponse_changeToken = Lens.lens (\DeleteRuleResponse' {changeToken} -> changeToken) (\s@DeleteRuleResponse' {} a -> s {changeToken = a} :: DeleteRuleResponse)

-- | The response's http status code.
deleteRuleResponse_httpStatus :: Lens.Lens' DeleteRuleResponse Core.Int
deleteRuleResponse_httpStatus = Lens.lens (\DeleteRuleResponse' {httpStatus} -> httpStatus) (\s@DeleteRuleResponse' {} a -> s {httpStatus = a} :: DeleteRuleResponse)

instance Core.NFData DeleteRuleResponse
