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
-- Module      : Network.AWS.WAF.UpdateRateBasedRule
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
-- Inserts or deletes Predicate objects in a rule and updates the
-- @RateLimit@ in the rule.
--
-- Each @Predicate@ object identifies a predicate, such as a ByteMatchSet
-- or an IPSet, that specifies the web requests that you want to block or
-- count. The @RateLimit@ specifies the number of requests every five
-- minutes that triggers the rule.
--
-- If you add more than one predicate to a @RateBasedRule@, a request must
-- match all the predicates and exceed the @RateLimit@ to be counted or
-- blocked. For example, suppose you add the following to a
-- @RateBasedRule@:
--
-- -   An @IPSet@ that matches the IP address @192.0.2.44\/32@
--
-- -   A @ByteMatchSet@ that matches @BadBot@ in the @User-Agent@ header
--
-- Further, you specify a @RateLimit@ of 1,000.
--
-- You then add the @RateBasedRule@ to a @WebACL@ and specify that you want
-- to block requests that satisfy the rule. For a request to be blocked, it
-- must come from the IP address 192.0.2.44 /and/ the @User-Agent@ header
-- in the request must contain the value @BadBot@. Further, requests that
-- match these two conditions much be received at a rate of more than 1,000
-- every five minutes. If the rate drops below this limit, AWS WAF no
-- longer blocks the requests.
--
-- As a second example, suppose you want to limit requests to a particular
-- page on your site. To do this, you could add the following to a
-- @RateBasedRule@:
--
-- -   A @ByteMatchSet@ with @FieldToMatch@ of @URI@
--
-- -   A @PositionalConstraint@ of @STARTS_WITH@
--
-- -   A @TargetString@ of @login@
--
-- Further, you specify a @RateLimit@ of 1,000.
--
-- By adding this @RateBasedRule@ to a @WebACL@, you could limit requests
-- to your login page without affecting the rest of your site.
module Network.AWS.WAF.UpdateRateBasedRule
  ( -- * Creating a Request
    UpdateRateBasedRule (..),
    newUpdateRateBasedRule,

    -- * Request Lenses
    updateRateBasedRule_ruleId,
    updateRateBasedRule_changeToken,
    updateRateBasedRule_updates,
    updateRateBasedRule_rateLimit,

    -- * Destructuring the Response
    UpdateRateBasedRuleResponse (..),
    newUpdateRateBasedRuleResponse,

    -- * Response Lenses
    updateRateBasedRuleResponse_changeToken,
    updateRateBasedRuleResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAF.Types

-- | /See:/ 'newUpdateRateBasedRule' smart constructor.
data UpdateRateBasedRule = UpdateRateBasedRule'
  { -- | The @RuleId@ of the @RateBasedRule@ that you want to update. @RuleId@ is
    -- returned by @CreateRateBasedRule@ and by ListRateBasedRules.
    ruleId :: Prelude.Text,
    -- | The value returned by the most recent call to GetChangeToken.
    changeToken :: Prelude.Text,
    -- | An array of @RuleUpdate@ objects that you want to insert into or delete
    -- from a RateBasedRule.
    updates :: [RuleUpdate],
    -- | The maximum number of requests, which have an identical value in the
    -- field specified by the @RateKey@, allowed in a five-minute period. If
    -- the number of requests exceeds the @RateLimit@ and the other predicates
    -- specified in the rule are also met, AWS WAF triggers the action that is
    -- specified for this rule.
    rateLimit :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRateBasedRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleId', 'updateRateBasedRule_ruleId' - The @RuleId@ of the @RateBasedRule@ that you want to update. @RuleId@ is
-- returned by @CreateRateBasedRule@ and by ListRateBasedRules.
--
-- 'changeToken', 'updateRateBasedRule_changeToken' - The value returned by the most recent call to GetChangeToken.
--
-- 'updates', 'updateRateBasedRule_updates' - An array of @RuleUpdate@ objects that you want to insert into or delete
-- from a RateBasedRule.
--
-- 'rateLimit', 'updateRateBasedRule_rateLimit' - The maximum number of requests, which have an identical value in the
-- field specified by the @RateKey@, allowed in a five-minute period. If
-- the number of requests exceeds the @RateLimit@ and the other predicates
-- specified in the rule are also met, AWS WAF triggers the action that is
-- specified for this rule.
newUpdateRateBasedRule ::
  -- | 'ruleId'
  Prelude.Text ->
  -- | 'changeToken'
  Prelude.Text ->
  -- | 'rateLimit'
  Prelude.Natural ->
  UpdateRateBasedRule
newUpdateRateBasedRule
  pRuleId_
  pChangeToken_
  pRateLimit_ =
    UpdateRateBasedRule'
      { ruleId = pRuleId_,
        changeToken = pChangeToken_,
        updates = Prelude.mempty,
        rateLimit = pRateLimit_
      }

-- | The @RuleId@ of the @RateBasedRule@ that you want to update. @RuleId@ is
-- returned by @CreateRateBasedRule@ and by ListRateBasedRules.
updateRateBasedRule_ruleId :: Lens.Lens' UpdateRateBasedRule Prelude.Text
updateRateBasedRule_ruleId = Lens.lens (\UpdateRateBasedRule' {ruleId} -> ruleId) (\s@UpdateRateBasedRule' {} a -> s {ruleId = a} :: UpdateRateBasedRule)

-- | The value returned by the most recent call to GetChangeToken.
updateRateBasedRule_changeToken :: Lens.Lens' UpdateRateBasedRule Prelude.Text
updateRateBasedRule_changeToken = Lens.lens (\UpdateRateBasedRule' {changeToken} -> changeToken) (\s@UpdateRateBasedRule' {} a -> s {changeToken = a} :: UpdateRateBasedRule)

-- | An array of @RuleUpdate@ objects that you want to insert into or delete
-- from a RateBasedRule.
updateRateBasedRule_updates :: Lens.Lens' UpdateRateBasedRule [RuleUpdate]
updateRateBasedRule_updates = Lens.lens (\UpdateRateBasedRule' {updates} -> updates) (\s@UpdateRateBasedRule' {} a -> s {updates = a} :: UpdateRateBasedRule) Prelude.. Lens._Coerce

-- | The maximum number of requests, which have an identical value in the
-- field specified by the @RateKey@, allowed in a five-minute period. If
-- the number of requests exceeds the @RateLimit@ and the other predicates
-- specified in the rule are also met, AWS WAF triggers the action that is
-- specified for this rule.
updateRateBasedRule_rateLimit :: Lens.Lens' UpdateRateBasedRule Prelude.Natural
updateRateBasedRule_rateLimit = Lens.lens (\UpdateRateBasedRule' {rateLimit} -> rateLimit) (\s@UpdateRateBasedRule' {} a -> s {rateLimit = a} :: UpdateRateBasedRule)

instance Core.AWSRequest UpdateRateBasedRule where
  type
    AWSResponse UpdateRateBasedRule =
      UpdateRateBasedRuleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateRateBasedRuleResponse'
            Prelude.<$> (x Core..?> "ChangeToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateRateBasedRule

instance Prelude.NFData UpdateRateBasedRule

instance Core.ToHeaders UpdateRateBasedRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSWAF_20150824.UpdateRateBasedRule" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateRateBasedRule where
  toJSON UpdateRateBasedRule' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("RuleId" Core..= ruleId),
            Prelude.Just ("ChangeToken" Core..= changeToken),
            Prelude.Just ("Updates" Core..= updates),
            Prelude.Just ("RateLimit" Core..= rateLimit)
          ]
      )

instance Core.ToPath UpdateRateBasedRule where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateRateBasedRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRateBasedRuleResponse' smart constructor.
data UpdateRateBasedRuleResponse = UpdateRateBasedRuleResponse'
  { -- | The @ChangeToken@ that you used to submit the @UpdateRateBasedRule@
    -- request. You can also use this value to query the status of the request.
    -- For more information, see GetChangeTokenStatus.
    changeToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRateBasedRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'changeToken', 'updateRateBasedRuleResponse_changeToken' - The @ChangeToken@ that you used to submit the @UpdateRateBasedRule@
-- request. You can also use this value to query the status of the request.
-- For more information, see GetChangeTokenStatus.
--
-- 'httpStatus', 'updateRateBasedRuleResponse_httpStatus' - The response's http status code.
newUpdateRateBasedRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateRateBasedRuleResponse
newUpdateRateBasedRuleResponse pHttpStatus_ =
  UpdateRateBasedRuleResponse'
    { changeToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @ChangeToken@ that you used to submit the @UpdateRateBasedRule@
-- request. You can also use this value to query the status of the request.
-- For more information, see GetChangeTokenStatus.
updateRateBasedRuleResponse_changeToken :: Lens.Lens' UpdateRateBasedRuleResponse (Prelude.Maybe Prelude.Text)
updateRateBasedRuleResponse_changeToken = Lens.lens (\UpdateRateBasedRuleResponse' {changeToken} -> changeToken) (\s@UpdateRateBasedRuleResponse' {} a -> s {changeToken = a} :: UpdateRateBasedRuleResponse)

-- | The response's http status code.
updateRateBasedRuleResponse_httpStatus :: Lens.Lens' UpdateRateBasedRuleResponse Prelude.Int
updateRateBasedRuleResponse_httpStatus = Lens.lens (\UpdateRateBasedRuleResponse' {httpStatus} -> httpStatus) (\s@UpdateRateBasedRuleResponse' {} a -> s {httpStatus = a} :: UpdateRateBasedRuleResponse)

instance Prelude.NFData UpdateRateBasedRuleResponse
