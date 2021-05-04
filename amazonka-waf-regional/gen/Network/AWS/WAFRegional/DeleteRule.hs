{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.WAFRegional.DeleteRule
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
module Network.AWS.WAFRegional.DeleteRule
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAFRegional.Types

-- | /See:/ 'newDeleteRule' smart constructor.
data DeleteRule = DeleteRule'
  { -- | The @RuleId@ of the Rule that you want to delete. @RuleId@ is returned
    -- by CreateRule and by ListRules.
    ruleId :: Prelude.Text,
    -- | The value returned by the most recent call to GetChangeToken.
    changeToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'changeToken'
  Prelude.Text ->
  DeleteRule
newDeleteRule pRuleId_ pChangeToken_ =
  DeleteRule'
    { ruleId = pRuleId_,
      changeToken = pChangeToken_
    }

-- | The @RuleId@ of the Rule that you want to delete. @RuleId@ is returned
-- by CreateRule and by ListRules.
deleteRule_ruleId :: Lens.Lens' DeleteRule Prelude.Text
deleteRule_ruleId = Lens.lens (\DeleteRule' {ruleId} -> ruleId) (\s@DeleteRule' {} a -> s {ruleId = a} :: DeleteRule)

-- | The value returned by the most recent call to GetChangeToken.
deleteRule_changeToken :: Lens.Lens' DeleteRule Prelude.Text
deleteRule_changeToken = Lens.lens (\DeleteRule' {changeToken} -> changeToken) (\s@DeleteRule' {} a -> s {changeToken = a} :: DeleteRule)

instance Prelude.AWSRequest DeleteRule where
  type Rs DeleteRule = DeleteRuleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteRuleResponse'
            Prelude.<$> (x Prelude..?> "ChangeToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteRule

instance Prelude.NFData DeleteRule

instance Prelude.ToHeaders DeleteRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSWAF_Regional_20161128.DeleteRule" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteRule where
  toJSON DeleteRule' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("RuleId" Prelude..= ruleId),
            Prelude.Just ("ChangeToken" Prelude..= changeToken)
          ]
      )

instance Prelude.ToPath DeleteRule where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRuleResponse' smart constructor.
data DeleteRuleResponse = DeleteRuleResponse'
  { -- | The @ChangeToken@ that you used to submit the @DeleteRule@ request. You
    -- can also use this value to query the status of the request. For more
    -- information, see GetChangeTokenStatus.
    changeToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DeleteRuleResponse
newDeleteRuleResponse pHttpStatus_ =
  DeleteRuleResponse'
    { changeToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @ChangeToken@ that you used to submit the @DeleteRule@ request. You
-- can also use this value to query the status of the request. For more
-- information, see GetChangeTokenStatus.
deleteRuleResponse_changeToken :: Lens.Lens' DeleteRuleResponse (Prelude.Maybe Prelude.Text)
deleteRuleResponse_changeToken = Lens.lens (\DeleteRuleResponse' {changeToken} -> changeToken) (\s@DeleteRuleResponse' {} a -> s {changeToken = a} :: DeleteRuleResponse)

-- | The response's http status code.
deleteRuleResponse_httpStatus :: Lens.Lens' DeleteRuleResponse Prelude.Int
deleteRuleResponse_httpStatus = Lens.lens (\DeleteRuleResponse' {httpStatus} -> httpStatus) (\s@DeleteRuleResponse' {} a -> s {httpStatus = a} :: DeleteRuleResponse)

instance Prelude.NFData DeleteRuleResponse
