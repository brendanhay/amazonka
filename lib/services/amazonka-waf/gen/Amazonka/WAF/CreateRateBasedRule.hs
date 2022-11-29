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
-- Module      : Amazonka.WAF.CreateRateBasedRule
-- Copyright   : (c) 2013-2022 Brendan Hay
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
-- Creates a RateBasedRule. The @RateBasedRule@ contains a @RateLimit@,
-- which specifies the maximum number of requests that AWS WAF allows from
-- a specified IP address in a five-minute period. The @RateBasedRule@ also
-- contains the @IPSet@ objects, @ByteMatchSet@ objects, and other
-- predicates that identify the requests that you want to count or block if
-- these requests exceed the @RateLimit@.
--
-- If you add more than one predicate to a @RateBasedRule@, a request not
-- only must exceed the @RateLimit@, but it also must match all the
-- conditions to be counted or blocked. For example, suppose you add the
-- following to a @RateBasedRule@:
--
-- -   An @IPSet@ that matches the IP address @192.0.2.44\/32@
--
-- -   A @ByteMatchSet@ that matches @BadBot@ in the @User-Agent@ header
--
-- Further, you specify a @RateLimit@ of 1,000.
--
-- You then add the @RateBasedRule@ to a @WebACL@ and specify that you want
-- to block requests that meet the conditions in the rule. For a request to
-- be blocked, it must come from the IP address 192.0.2.44 /and/ the
-- @User-Agent@ header in the request must contain the value @BadBot@.
-- Further, requests that match these two conditions must be received at a
-- rate of more than 1,000 requests every five minutes. If both conditions
-- are met and the rate is exceeded, AWS WAF blocks the requests. If the
-- rate drops below 1,000 for a five-minute period, AWS WAF no longer
-- blocks the requests.
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
--
-- To create and configure a @RateBasedRule@, perform the following steps:
--
-- 1.  Create and update the predicates that you want to include in the
--     rule. For more information, see CreateByteMatchSet, CreateIPSet, and
--     CreateSqlInjectionMatchSet.
--
-- 2.  Use GetChangeToken to get the change token that you provide in the
--     @ChangeToken@ parameter of a @CreateRule@ request.
--
-- 3.  Submit a @CreateRateBasedRule@ request.
--
-- 4.  Use @GetChangeToken@ to get the change token that you provide in the
--     @ChangeToken@ parameter of an UpdateRule request.
--
-- 5.  Submit an @UpdateRateBasedRule@ request to specify the predicates
--     that you want to include in the rule.
--
-- 6.  Create and update a @WebACL@ that contains the @RateBasedRule@. For
--     more information, see CreateWebACL.
--
-- For more information about how to use the AWS WAF API to allow or block
-- HTTP requests, see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide>.
module Amazonka.WAF.CreateRateBasedRule
  ( -- * Creating a Request
    CreateRateBasedRule (..),
    newCreateRateBasedRule,

    -- * Request Lenses
    createRateBasedRule_tags,
    createRateBasedRule_name,
    createRateBasedRule_metricName,
    createRateBasedRule_rateKey,
    createRateBasedRule_rateLimit,
    createRateBasedRule_changeToken,

    -- * Destructuring the Response
    CreateRateBasedRuleResponse (..),
    newCreateRateBasedRuleResponse,

    -- * Response Lenses
    createRateBasedRuleResponse_rule,
    createRateBasedRuleResponse_changeToken,
    createRateBasedRuleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAF.Types

-- | /See:/ 'newCreateRateBasedRule' smart constructor.
data CreateRateBasedRule = CreateRateBasedRule'
  { tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | A friendly name or description of the RateBasedRule. You can\'t change
    -- the name of a @RateBasedRule@ after you create it.
    name :: Prelude.Text,
    -- | A friendly name or description for the metrics for this @RateBasedRule@.
    -- The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with
    -- maximum length 128 and minimum length one. It can\'t contain whitespace
    -- or metric names reserved for AWS WAF, including \"All\" and
    -- \"Default_Action.\" You can\'t change the name of the metric after you
    -- create the @RateBasedRule@.
    metricName :: Prelude.Text,
    -- | The field that AWS WAF uses to determine if requests are likely arriving
    -- from a single source and thus subject to rate monitoring. The only valid
    -- value for @RateKey@ is @IP@. @IP@ indicates that requests that arrive
    -- from the same IP address are subject to the @RateLimit@ that is
    -- specified in the @RateBasedRule@.
    rateKey :: RateKey,
    -- | The maximum number of requests, which have an identical value in the
    -- field that is specified by @RateKey@, allowed in a five-minute period.
    -- If the number of requests exceeds the @RateLimit@ and the other
    -- predicates specified in the rule are also met, AWS WAF triggers the
    -- action that is specified for this rule.
    rateLimit :: Prelude.Natural,
    -- | The @ChangeToken@ that you used to submit the @CreateRateBasedRule@
    -- request. You can also use this value to query the status of the request.
    -- For more information, see GetChangeTokenStatus.
    changeToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRateBasedRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createRateBasedRule_tags' -
--
-- 'name', 'createRateBasedRule_name' - A friendly name or description of the RateBasedRule. You can\'t change
-- the name of a @RateBasedRule@ after you create it.
--
-- 'metricName', 'createRateBasedRule_metricName' - A friendly name or description for the metrics for this @RateBasedRule@.
-- The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with
-- maximum length 128 and minimum length one. It can\'t contain whitespace
-- or metric names reserved for AWS WAF, including \"All\" and
-- \"Default_Action.\" You can\'t change the name of the metric after you
-- create the @RateBasedRule@.
--
-- 'rateKey', 'createRateBasedRule_rateKey' - The field that AWS WAF uses to determine if requests are likely arriving
-- from a single source and thus subject to rate monitoring. The only valid
-- value for @RateKey@ is @IP@. @IP@ indicates that requests that arrive
-- from the same IP address are subject to the @RateLimit@ that is
-- specified in the @RateBasedRule@.
--
-- 'rateLimit', 'createRateBasedRule_rateLimit' - The maximum number of requests, which have an identical value in the
-- field that is specified by @RateKey@, allowed in a five-minute period.
-- If the number of requests exceeds the @RateLimit@ and the other
-- predicates specified in the rule are also met, AWS WAF triggers the
-- action that is specified for this rule.
--
-- 'changeToken', 'createRateBasedRule_changeToken' - The @ChangeToken@ that you used to submit the @CreateRateBasedRule@
-- request. You can also use this value to query the status of the request.
-- For more information, see GetChangeTokenStatus.
newCreateRateBasedRule ::
  -- | 'name'
  Prelude.Text ->
  -- | 'metricName'
  Prelude.Text ->
  -- | 'rateKey'
  RateKey ->
  -- | 'rateLimit'
  Prelude.Natural ->
  -- | 'changeToken'
  Prelude.Text ->
  CreateRateBasedRule
newCreateRateBasedRule
  pName_
  pMetricName_
  pRateKey_
  pRateLimit_
  pChangeToken_ =
    CreateRateBasedRule'
      { tags = Prelude.Nothing,
        name = pName_,
        metricName = pMetricName_,
        rateKey = pRateKey_,
        rateLimit = pRateLimit_,
        changeToken = pChangeToken_
      }

-- |
createRateBasedRule_tags :: Lens.Lens' CreateRateBasedRule (Prelude.Maybe (Prelude.NonEmpty Tag))
createRateBasedRule_tags = Lens.lens (\CreateRateBasedRule' {tags} -> tags) (\s@CreateRateBasedRule' {} a -> s {tags = a} :: CreateRateBasedRule) Prelude.. Lens.mapping Lens.coerced

-- | A friendly name or description of the RateBasedRule. You can\'t change
-- the name of a @RateBasedRule@ after you create it.
createRateBasedRule_name :: Lens.Lens' CreateRateBasedRule Prelude.Text
createRateBasedRule_name = Lens.lens (\CreateRateBasedRule' {name} -> name) (\s@CreateRateBasedRule' {} a -> s {name = a} :: CreateRateBasedRule)

-- | A friendly name or description for the metrics for this @RateBasedRule@.
-- The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with
-- maximum length 128 and minimum length one. It can\'t contain whitespace
-- or metric names reserved for AWS WAF, including \"All\" and
-- \"Default_Action.\" You can\'t change the name of the metric after you
-- create the @RateBasedRule@.
createRateBasedRule_metricName :: Lens.Lens' CreateRateBasedRule Prelude.Text
createRateBasedRule_metricName = Lens.lens (\CreateRateBasedRule' {metricName} -> metricName) (\s@CreateRateBasedRule' {} a -> s {metricName = a} :: CreateRateBasedRule)

-- | The field that AWS WAF uses to determine if requests are likely arriving
-- from a single source and thus subject to rate monitoring. The only valid
-- value for @RateKey@ is @IP@. @IP@ indicates that requests that arrive
-- from the same IP address are subject to the @RateLimit@ that is
-- specified in the @RateBasedRule@.
createRateBasedRule_rateKey :: Lens.Lens' CreateRateBasedRule RateKey
createRateBasedRule_rateKey = Lens.lens (\CreateRateBasedRule' {rateKey} -> rateKey) (\s@CreateRateBasedRule' {} a -> s {rateKey = a} :: CreateRateBasedRule)

-- | The maximum number of requests, which have an identical value in the
-- field that is specified by @RateKey@, allowed in a five-minute period.
-- If the number of requests exceeds the @RateLimit@ and the other
-- predicates specified in the rule are also met, AWS WAF triggers the
-- action that is specified for this rule.
createRateBasedRule_rateLimit :: Lens.Lens' CreateRateBasedRule Prelude.Natural
createRateBasedRule_rateLimit = Lens.lens (\CreateRateBasedRule' {rateLimit} -> rateLimit) (\s@CreateRateBasedRule' {} a -> s {rateLimit = a} :: CreateRateBasedRule)

-- | The @ChangeToken@ that you used to submit the @CreateRateBasedRule@
-- request. You can also use this value to query the status of the request.
-- For more information, see GetChangeTokenStatus.
createRateBasedRule_changeToken :: Lens.Lens' CreateRateBasedRule Prelude.Text
createRateBasedRule_changeToken = Lens.lens (\CreateRateBasedRule' {changeToken} -> changeToken) (\s@CreateRateBasedRule' {} a -> s {changeToken = a} :: CreateRateBasedRule)

instance Core.AWSRequest CreateRateBasedRule where
  type
    AWSResponse CreateRateBasedRule =
      CreateRateBasedRuleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRateBasedRuleResponse'
            Prelude.<$> (x Core..?> "Rule")
            Prelude.<*> (x Core..?> "ChangeToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateRateBasedRule where
  hashWithSalt _salt CreateRateBasedRule' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` metricName
      `Prelude.hashWithSalt` rateKey
      `Prelude.hashWithSalt` rateLimit
      `Prelude.hashWithSalt` changeToken

instance Prelude.NFData CreateRateBasedRule where
  rnf CreateRateBasedRule' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf rateKey
      `Prelude.seq` Prelude.rnf rateLimit
      `Prelude.seq` Prelude.rnf changeToken

instance Core.ToHeaders CreateRateBasedRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSWAF_20150824.CreateRateBasedRule" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateRateBasedRule where
  toJSON CreateRateBasedRule' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("Name" Core..= name),
            Prelude.Just ("MetricName" Core..= metricName),
            Prelude.Just ("RateKey" Core..= rateKey),
            Prelude.Just ("RateLimit" Core..= rateLimit),
            Prelude.Just ("ChangeToken" Core..= changeToken)
          ]
      )

instance Core.ToPath CreateRateBasedRule where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateRateBasedRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateRateBasedRuleResponse' smart constructor.
data CreateRateBasedRuleResponse = CreateRateBasedRuleResponse'
  { -- | The RateBasedRule that is returned in the @CreateRateBasedRule@
    -- response.
    rule :: Prelude.Maybe RateBasedRule,
    -- | The @ChangeToken@ that you used to submit the @CreateRateBasedRule@
    -- request. You can also use this value to query the status of the request.
    -- For more information, see GetChangeTokenStatus.
    changeToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRateBasedRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rule', 'createRateBasedRuleResponse_rule' - The RateBasedRule that is returned in the @CreateRateBasedRule@
-- response.
--
-- 'changeToken', 'createRateBasedRuleResponse_changeToken' - The @ChangeToken@ that you used to submit the @CreateRateBasedRule@
-- request. You can also use this value to query the status of the request.
-- For more information, see GetChangeTokenStatus.
--
-- 'httpStatus', 'createRateBasedRuleResponse_httpStatus' - The response's http status code.
newCreateRateBasedRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateRateBasedRuleResponse
newCreateRateBasedRuleResponse pHttpStatus_ =
  CreateRateBasedRuleResponse'
    { rule =
        Prelude.Nothing,
      changeToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The RateBasedRule that is returned in the @CreateRateBasedRule@
-- response.
createRateBasedRuleResponse_rule :: Lens.Lens' CreateRateBasedRuleResponse (Prelude.Maybe RateBasedRule)
createRateBasedRuleResponse_rule = Lens.lens (\CreateRateBasedRuleResponse' {rule} -> rule) (\s@CreateRateBasedRuleResponse' {} a -> s {rule = a} :: CreateRateBasedRuleResponse)

-- | The @ChangeToken@ that you used to submit the @CreateRateBasedRule@
-- request. You can also use this value to query the status of the request.
-- For more information, see GetChangeTokenStatus.
createRateBasedRuleResponse_changeToken :: Lens.Lens' CreateRateBasedRuleResponse (Prelude.Maybe Prelude.Text)
createRateBasedRuleResponse_changeToken = Lens.lens (\CreateRateBasedRuleResponse' {changeToken} -> changeToken) (\s@CreateRateBasedRuleResponse' {} a -> s {changeToken = a} :: CreateRateBasedRuleResponse)

-- | The response's http status code.
createRateBasedRuleResponse_httpStatus :: Lens.Lens' CreateRateBasedRuleResponse Prelude.Int
createRateBasedRuleResponse_httpStatus = Lens.lens (\CreateRateBasedRuleResponse' {httpStatus} -> httpStatus) (\s@CreateRateBasedRuleResponse' {} a -> s {httpStatus = a} :: CreateRateBasedRuleResponse)

instance Prelude.NFData CreateRateBasedRuleResponse where
  rnf CreateRateBasedRuleResponse' {..} =
    Prelude.rnf rule
      `Prelude.seq` Prelude.rnf changeToken
      `Prelude.seq` Prelude.rnf httpStatus
