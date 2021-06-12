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
-- Module      : Network.AWS.WAF.UpdateRule
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
module Network.AWS.WAF.UpdateRule
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAF.Types

-- | /See:/ 'newUpdateRule' smart constructor.
data UpdateRule = UpdateRule'
  { -- | The @RuleId@ of the @Rule@ that you want to update. @RuleId@ is returned
    -- by @CreateRule@ and by ListRules.
    ruleId :: Core.Text,
    -- | The value returned by the most recent call to GetChangeToken.
    changeToken :: Core.Text,
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
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'changeToken'
  Core.Text ->
  UpdateRule
newUpdateRule pRuleId_ pChangeToken_ =
  UpdateRule'
    { ruleId = pRuleId_,
      changeToken = pChangeToken_,
      updates = Core.mempty
    }

-- | The @RuleId@ of the @Rule@ that you want to update. @RuleId@ is returned
-- by @CreateRule@ and by ListRules.
updateRule_ruleId :: Lens.Lens' UpdateRule Core.Text
updateRule_ruleId = Lens.lens (\UpdateRule' {ruleId} -> ruleId) (\s@UpdateRule' {} a -> s {ruleId = a} :: UpdateRule)

-- | The value returned by the most recent call to GetChangeToken.
updateRule_changeToken :: Lens.Lens' UpdateRule Core.Text
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
updateRule_updates = Lens.lens (\UpdateRule' {updates} -> updates) (\s@UpdateRule' {} a -> s {updates = a} :: UpdateRule) Core.. Lens._Coerce

instance Core.AWSRequest UpdateRule where
  type AWSResponse UpdateRule = UpdateRuleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateRuleResponse'
            Core.<$> (x Core..?> "ChangeToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateRule

instance Core.NFData UpdateRule

instance Core.ToHeaders UpdateRule where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSWAF_20150824.UpdateRule" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateRule where
  toJSON UpdateRule' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("RuleId" Core..= ruleId),
            Core.Just ("ChangeToken" Core..= changeToken),
            Core.Just ("Updates" Core..= updates)
          ]
      )

instance Core.ToPath UpdateRule where
  toPath = Core.const "/"

instance Core.ToQuery UpdateRule where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateRuleResponse' smart constructor.
data UpdateRuleResponse = UpdateRuleResponse'
  { -- | The @ChangeToken@ that you used to submit the @UpdateRule@ request. You
    -- can also use this value to query the status of the request. For more
    -- information, see GetChangeTokenStatus.
    changeToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  UpdateRuleResponse
newUpdateRuleResponse pHttpStatus_ =
  UpdateRuleResponse'
    { changeToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @ChangeToken@ that you used to submit the @UpdateRule@ request. You
-- can also use this value to query the status of the request. For more
-- information, see GetChangeTokenStatus.
updateRuleResponse_changeToken :: Lens.Lens' UpdateRuleResponse (Core.Maybe Core.Text)
updateRuleResponse_changeToken = Lens.lens (\UpdateRuleResponse' {changeToken} -> changeToken) (\s@UpdateRuleResponse' {} a -> s {changeToken = a} :: UpdateRuleResponse)

-- | The response's http status code.
updateRuleResponse_httpStatus :: Lens.Lens' UpdateRuleResponse Core.Int
updateRuleResponse_httpStatus = Lens.lens (\UpdateRuleResponse' {httpStatus} -> httpStatus) (\s@UpdateRuleResponse' {} a -> s {httpStatus = a} :: UpdateRuleResponse)

instance Core.NFData UpdateRuleResponse
