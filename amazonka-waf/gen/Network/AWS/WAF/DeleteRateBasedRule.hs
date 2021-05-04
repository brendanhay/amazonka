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
-- Module      : Network.AWS.WAF.DeleteRateBasedRule
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
-- Permanently deletes a RateBasedRule. You can\'t delete a rule if it\'s
-- still used in any @WebACL@ objects or if it still includes any
-- predicates, such as @ByteMatchSet@ objects.
--
-- If you just want to remove a rule from a @WebACL@, use UpdateWebACL.
--
-- To permanently delete a @RateBasedRule@ from AWS WAF, perform the
-- following steps:
--
-- 1.  Update the @RateBasedRule@ to remove predicates, if any. For more
--     information, see UpdateRateBasedRule.
--
-- 2.  Use GetChangeToken to get the change token that you provide in the
--     @ChangeToken@ parameter of a @DeleteRateBasedRule@ request.
--
-- 3.  Submit a @DeleteRateBasedRule@ request.
module Network.AWS.WAF.DeleteRateBasedRule
  ( -- * Creating a Request
    DeleteRateBasedRule (..),
    newDeleteRateBasedRule,

    -- * Request Lenses
    deleteRateBasedRule_ruleId,
    deleteRateBasedRule_changeToken,

    -- * Destructuring the Response
    DeleteRateBasedRuleResponse (..),
    newDeleteRateBasedRuleResponse,

    -- * Response Lenses
    deleteRateBasedRuleResponse_changeToken,
    deleteRateBasedRuleResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAF.Types

-- | /See:/ 'newDeleteRateBasedRule' smart constructor.
data DeleteRateBasedRule = DeleteRateBasedRule'
  { -- | The @RuleId@ of the RateBasedRule that you want to delete. @RuleId@ is
    -- returned by CreateRateBasedRule and by ListRateBasedRules.
    ruleId :: Prelude.Text,
    -- | The value returned by the most recent call to GetChangeToken.
    changeToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteRateBasedRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleId', 'deleteRateBasedRule_ruleId' - The @RuleId@ of the RateBasedRule that you want to delete. @RuleId@ is
-- returned by CreateRateBasedRule and by ListRateBasedRules.
--
-- 'changeToken', 'deleteRateBasedRule_changeToken' - The value returned by the most recent call to GetChangeToken.
newDeleteRateBasedRule ::
  -- | 'ruleId'
  Prelude.Text ->
  -- | 'changeToken'
  Prelude.Text ->
  DeleteRateBasedRule
newDeleteRateBasedRule pRuleId_ pChangeToken_ =
  DeleteRateBasedRule'
    { ruleId = pRuleId_,
      changeToken = pChangeToken_
    }

-- | The @RuleId@ of the RateBasedRule that you want to delete. @RuleId@ is
-- returned by CreateRateBasedRule and by ListRateBasedRules.
deleteRateBasedRule_ruleId :: Lens.Lens' DeleteRateBasedRule Prelude.Text
deleteRateBasedRule_ruleId = Lens.lens (\DeleteRateBasedRule' {ruleId} -> ruleId) (\s@DeleteRateBasedRule' {} a -> s {ruleId = a} :: DeleteRateBasedRule)

-- | The value returned by the most recent call to GetChangeToken.
deleteRateBasedRule_changeToken :: Lens.Lens' DeleteRateBasedRule Prelude.Text
deleteRateBasedRule_changeToken = Lens.lens (\DeleteRateBasedRule' {changeToken} -> changeToken) (\s@DeleteRateBasedRule' {} a -> s {changeToken = a} :: DeleteRateBasedRule)

instance Prelude.AWSRequest DeleteRateBasedRule where
  type
    Rs DeleteRateBasedRule =
      DeleteRateBasedRuleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteRateBasedRuleResponse'
            Prelude.<$> (x Prelude..?> "ChangeToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteRateBasedRule

instance Prelude.NFData DeleteRateBasedRule

instance Prelude.ToHeaders DeleteRateBasedRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSWAF_20150824.DeleteRateBasedRule" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteRateBasedRule where
  toJSON DeleteRateBasedRule' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("RuleId" Prelude..= ruleId),
            Prelude.Just ("ChangeToken" Prelude..= changeToken)
          ]
      )

instance Prelude.ToPath DeleteRateBasedRule where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteRateBasedRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRateBasedRuleResponse' smart constructor.
data DeleteRateBasedRuleResponse = DeleteRateBasedRuleResponse'
  { -- | The @ChangeToken@ that you used to submit the @DeleteRateBasedRule@
    -- request. You can also use this value to query the status of the request.
    -- For more information, see GetChangeTokenStatus.
    changeToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteRateBasedRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'changeToken', 'deleteRateBasedRuleResponse_changeToken' - The @ChangeToken@ that you used to submit the @DeleteRateBasedRule@
-- request. You can also use this value to query the status of the request.
-- For more information, see GetChangeTokenStatus.
--
-- 'httpStatus', 'deleteRateBasedRuleResponse_httpStatus' - The response's http status code.
newDeleteRateBasedRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteRateBasedRuleResponse
newDeleteRateBasedRuleResponse pHttpStatus_ =
  DeleteRateBasedRuleResponse'
    { changeToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @ChangeToken@ that you used to submit the @DeleteRateBasedRule@
-- request. You can also use this value to query the status of the request.
-- For more information, see GetChangeTokenStatus.
deleteRateBasedRuleResponse_changeToken :: Lens.Lens' DeleteRateBasedRuleResponse (Prelude.Maybe Prelude.Text)
deleteRateBasedRuleResponse_changeToken = Lens.lens (\DeleteRateBasedRuleResponse' {changeToken} -> changeToken) (\s@DeleteRateBasedRuleResponse' {} a -> s {changeToken = a} :: DeleteRateBasedRuleResponse)

-- | The response's http status code.
deleteRateBasedRuleResponse_httpStatus :: Lens.Lens' DeleteRateBasedRuleResponse Prelude.Int
deleteRateBasedRuleResponse_httpStatus = Lens.lens (\DeleteRateBasedRuleResponse' {httpStatus} -> httpStatus) (\s@DeleteRateBasedRuleResponse' {} a -> s {httpStatus = a} :: DeleteRateBasedRuleResponse)

instance Prelude.NFData DeleteRateBasedRuleResponse
