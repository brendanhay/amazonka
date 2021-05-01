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
-- Module      : Network.AWS.WAFRegional.UpdateRule
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
-- Inserts or deletes Predicate objects in a @Rule@. Each @Predicate@
-- object identifies a predicate, such as a ByteMatchSet or an IPSet, that
-- specifies the web requests that you want to allow, block, or count. If
-- you add more than one predicate to a @Rule@, a request must match all of
-- the specifications to be allowed, blocked, or counted. For example,
-- suppose that you add the following to a @Rule@:
--
-- -   A @ByteMatchSet@ that matches the value @BadBot@ in the @User-Agent@
--     header
--
-- -   An @IPSet@ that matches the IP address @192.0.2.44@
--
-- You then add the @Rule@ to a @WebACL@ and specify that you want to block
-- requests that satisfy the @Rule@. For a request to be blocked, the
-- @User-Agent@ header in the request must contain the value @BadBot@ /and/
-- the request must originate from the IP address 192.0.2.44.
--
-- To create and configure a @Rule@, perform the following steps:
--
-- 1.  Create and update the predicates that you want to include in the
--     @Rule@.
--
-- 2.  Create the @Rule@. See CreateRule.
--
-- 3.  Use @GetChangeToken@ to get the change token that you provide in the
--     @ChangeToken@ parameter of an UpdateRule request.
--
-- 4.  Submit an @UpdateRule@ request to add predicates to the @Rule@.
--
-- 5.  Create and update a @WebACL@ that contains the @Rule@. See
--     CreateWebACL.
--
-- If you want to replace one @ByteMatchSet@ or @IPSet@ with another, you
-- delete the existing one and add the new one.
--
-- For more information about how to use the AWS WAF API to allow or block
-- HTTP requests, see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide>.
module Network.AWS.WAFRegional.UpdateRule
  ( -- * Creating a Request
    UpdateRule (..),
    newUpdateRule,

    -- * Request Lenses
    updateRule_ruleId,
    updateRule_changeToken,
    updateRule_updates,

    -- * Destructuring the Response
    UpdateRuleResponse (..),
    newUpdateRuleResponse,

    -- * Response Lenses
    updateRuleResponse_changeToken,
    updateRuleResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAFRegional.Types

-- | /See:/ 'newUpdateRule' smart constructor.
data UpdateRule = UpdateRule'
  { -- | The @RuleId@ of the @Rule@ that you want to update. @RuleId@ is returned
    -- by @CreateRule@ and by ListRules.
    ruleId :: Prelude.Text,
    -- | The value returned by the most recent call to GetChangeToken.
    changeToken :: Prelude.Text,
    -- | An array of @RuleUpdate@ objects that you want to insert into or delete
    -- from a Rule. For more information, see the applicable data types:
    --
    -- -   RuleUpdate: Contains @Action@ and @Predicate@
    --
    -- -   Predicate: Contains @DataId@, @Negated@, and @Type@
    --
    -- -   FieldToMatch: Contains @Data@ and @Type@
    updates :: [RuleUpdate]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleId', 'updateRule_ruleId' - The @RuleId@ of the @Rule@ that you want to update. @RuleId@ is returned
-- by @CreateRule@ and by ListRules.
--
-- 'changeToken', 'updateRule_changeToken' - The value returned by the most recent call to GetChangeToken.
--
-- 'updates', 'updateRule_updates' - An array of @RuleUpdate@ objects that you want to insert into or delete
-- from a Rule. For more information, see the applicable data types:
--
-- -   RuleUpdate: Contains @Action@ and @Predicate@
--
-- -   Predicate: Contains @DataId@, @Negated@, and @Type@
--
-- -   FieldToMatch: Contains @Data@ and @Type@
newUpdateRule ::
  -- | 'ruleId'
  Prelude.Text ->
  -- | 'changeToken'
  Prelude.Text ->
  UpdateRule
newUpdateRule pRuleId_ pChangeToken_ =
  UpdateRule'
    { ruleId = pRuleId_,
      changeToken = pChangeToken_,
      updates = Prelude.mempty
    }

-- | The @RuleId@ of the @Rule@ that you want to update. @RuleId@ is returned
-- by @CreateRule@ and by ListRules.
updateRule_ruleId :: Lens.Lens' UpdateRule Prelude.Text
updateRule_ruleId = Lens.lens (\UpdateRule' {ruleId} -> ruleId) (\s@UpdateRule' {} a -> s {ruleId = a} :: UpdateRule)

-- | The value returned by the most recent call to GetChangeToken.
updateRule_changeToken :: Lens.Lens' UpdateRule Prelude.Text
updateRule_changeToken = Lens.lens (\UpdateRule' {changeToken} -> changeToken) (\s@UpdateRule' {} a -> s {changeToken = a} :: UpdateRule)

-- | An array of @RuleUpdate@ objects that you want to insert into or delete
-- from a Rule. For more information, see the applicable data types:
--
-- -   RuleUpdate: Contains @Action@ and @Predicate@
--
-- -   Predicate: Contains @DataId@, @Negated@, and @Type@
--
-- -   FieldToMatch: Contains @Data@ and @Type@
updateRule_updates :: Lens.Lens' UpdateRule [RuleUpdate]
updateRule_updates = Lens.lens (\UpdateRule' {updates} -> updates) (\s@UpdateRule' {} a -> s {updates = a} :: UpdateRule) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest UpdateRule where
  type Rs UpdateRule = UpdateRuleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateRuleResponse'
            Prelude.<$> (x Prelude..?> "ChangeToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateRule

instance Prelude.NFData UpdateRule

instance Prelude.ToHeaders UpdateRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSWAF_Regional_20161128.UpdateRule" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateRule where
  toJSON UpdateRule' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("RuleId" Prelude..= ruleId),
            Prelude.Just ("ChangeToken" Prelude..= changeToken),
            Prelude.Just ("Updates" Prelude..= updates)
          ]
      )

instance Prelude.ToPath UpdateRule where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRuleResponse' smart constructor.
data UpdateRuleResponse = UpdateRuleResponse'
  { -- | The @ChangeToken@ that you used to submit the @UpdateRule@ request. You
    -- can also use this value to query the status of the request. For more
    -- information, see GetChangeTokenStatus.
    changeToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'changeToken', 'updateRuleResponse_changeToken' - The @ChangeToken@ that you used to submit the @UpdateRule@ request. You
-- can also use this value to query the status of the request. For more
-- information, see GetChangeTokenStatus.
--
-- 'httpStatus', 'updateRuleResponse_httpStatus' - The response's http status code.
newUpdateRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateRuleResponse
newUpdateRuleResponse pHttpStatus_ =
  UpdateRuleResponse'
    { changeToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @ChangeToken@ that you used to submit the @UpdateRule@ request. You
-- can also use this value to query the status of the request. For more
-- information, see GetChangeTokenStatus.
updateRuleResponse_changeToken :: Lens.Lens' UpdateRuleResponse (Prelude.Maybe Prelude.Text)
updateRuleResponse_changeToken = Lens.lens (\UpdateRuleResponse' {changeToken} -> changeToken) (\s@UpdateRuleResponse' {} a -> s {changeToken = a} :: UpdateRuleResponse)

-- | The response's http status code.
updateRuleResponse_httpStatus :: Lens.Lens' UpdateRuleResponse Prelude.Int
updateRuleResponse_httpStatus = Lens.lens (\UpdateRuleResponse' {httpStatus} -> httpStatus) (\s@UpdateRuleResponse' {} a -> s {httpStatus = a} :: UpdateRuleResponse)

instance Prelude.NFData UpdateRuleResponse
