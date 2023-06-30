{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.WAFV2.Types.SampledHTTPRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.SampledHTTPRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.CaptchaResponse
import Amazonka.WAFV2.Types.ChallengeResponse
import Amazonka.WAFV2.Types.HTTPHeader
import Amazonka.WAFV2.Types.HTTPRequest
import Amazonka.WAFV2.Types.Label

-- | Represents a single sampled web request. The response from
-- GetSampledRequests includes a @SampledHTTPRequests@ complex type that
-- appears as @SampledRequests@ in the response syntax.
-- @SampledHTTPRequests@ contains an array of @SampledHTTPRequest@ objects.
--
-- /See:/ 'newSampledHTTPRequest' smart constructor.
data SampledHTTPRequest = SampledHTTPRequest'
  { -- | The action that WAF applied to the request.
    action :: Prelude.Maybe Prelude.Text,
    -- | The @CAPTCHA@ response for the request.
    captchaResponse :: Prelude.Maybe CaptchaResponse,
    -- | The @Challenge@ response for the request.
    challengeResponse :: Prelude.Maybe ChallengeResponse,
    -- | Labels applied to the web request by matching rules. WAF applies fully
    -- qualified labels to matching web requests. A fully qualified label is
    -- the concatenation of a label namespace and a rule label. The rule\'s
    -- rule group or web ACL defines the label namespace.
    --
    -- For example,
    -- @awswaf:111122223333:myRuleGroup:testRules:testNS1:testNS2:labelNameA@
    -- or @awswaf:managed:aws:managed-rule-set:header:encoding:utf8@.
    labels :: Prelude.Maybe [Label],
    -- | Used only for rule group rules that have a rule action override in place
    -- in the web ACL. This is the action that the rule group rule is
    -- configured for, and not the action that was applied to the request. The
    -- action that WAF applied is the @Action@ value.
    overriddenAction :: Prelude.Maybe Prelude.Text,
    -- | Custom request headers inserted by WAF into the request, according to
    -- the custom request configuration for the matching rule action.
    requestHeadersInserted :: Prelude.Maybe [HTTPHeader],
    -- | The response code that was sent for the request.
    responseCodeSent :: Prelude.Maybe Prelude.Natural,
    -- | The name of the @Rule@ that the request matched. For managed rule
    -- groups, the format for this name is
    -- @\<vendor name>#\<managed rule group name>#\<rule name>@. For your own
    -- rule groups, the format for this name is
    -- @\<rule group name>#\<rule name>@. If the rule is not in a rule group,
    -- this field is absent.
    ruleNameWithinRuleGroup :: Prelude.Maybe Prelude.Text,
    -- | The time at which WAF received the request from your Amazon Web Services
    -- resource, in Unix time format (in seconds).
    timestamp :: Prelude.Maybe Data.POSIX,
    -- | A complex type that contains detailed information about the request.
    request :: HTTPRequest,
    -- | A value that indicates how one result in the response relates
    -- proportionally to other results in the response. For example, a result
    -- that has a weight of @2@ represents roughly twice as many web requests
    -- as a result that has a weight of @1@.
    weight :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SampledHTTPRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'sampledHTTPRequest_action' - The action that WAF applied to the request.
--
-- 'captchaResponse', 'sampledHTTPRequest_captchaResponse' - The @CAPTCHA@ response for the request.
--
-- 'challengeResponse', 'sampledHTTPRequest_challengeResponse' - The @Challenge@ response for the request.
--
-- 'labels', 'sampledHTTPRequest_labels' - Labels applied to the web request by matching rules. WAF applies fully
-- qualified labels to matching web requests. A fully qualified label is
-- the concatenation of a label namespace and a rule label. The rule\'s
-- rule group or web ACL defines the label namespace.
--
-- For example,
-- @awswaf:111122223333:myRuleGroup:testRules:testNS1:testNS2:labelNameA@
-- or @awswaf:managed:aws:managed-rule-set:header:encoding:utf8@.
--
-- 'overriddenAction', 'sampledHTTPRequest_overriddenAction' - Used only for rule group rules that have a rule action override in place
-- in the web ACL. This is the action that the rule group rule is
-- configured for, and not the action that was applied to the request. The
-- action that WAF applied is the @Action@ value.
--
-- 'requestHeadersInserted', 'sampledHTTPRequest_requestHeadersInserted' - Custom request headers inserted by WAF into the request, according to
-- the custom request configuration for the matching rule action.
--
-- 'responseCodeSent', 'sampledHTTPRequest_responseCodeSent' - The response code that was sent for the request.
--
-- 'ruleNameWithinRuleGroup', 'sampledHTTPRequest_ruleNameWithinRuleGroup' - The name of the @Rule@ that the request matched. For managed rule
-- groups, the format for this name is
-- @\<vendor name>#\<managed rule group name>#\<rule name>@. For your own
-- rule groups, the format for this name is
-- @\<rule group name>#\<rule name>@. If the rule is not in a rule group,
-- this field is absent.
--
-- 'timestamp', 'sampledHTTPRequest_timestamp' - The time at which WAF received the request from your Amazon Web Services
-- resource, in Unix time format (in seconds).
--
-- 'request', 'sampledHTTPRequest_request' - A complex type that contains detailed information about the request.
--
-- 'weight', 'sampledHTTPRequest_weight' - A value that indicates how one result in the response relates
-- proportionally to other results in the response. For example, a result
-- that has a weight of @2@ represents roughly twice as many web requests
-- as a result that has a weight of @1@.
newSampledHTTPRequest ::
  -- | 'request'
  HTTPRequest ->
  -- | 'weight'
  Prelude.Natural ->
  SampledHTTPRequest
newSampledHTTPRequest pRequest_ pWeight_ =
  SampledHTTPRequest'
    { action = Prelude.Nothing,
      captchaResponse = Prelude.Nothing,
      challengeResponse = Prelude.Nothing,
      labels = Prelude.Nothing,
      overriddenAction = Prelude.Nothing,
      requestHeadersInserted = Prelude.Nothing,
      responseCodeSent = Prelude.Nothing,
      ruleNameWithinRuleGroup = Prelude.Nothing,
      timestamp = Prelude.Nothing,
      request = pRequest_,
      weight = pWeight_
    }

-- | The action that WAF applied to the request.
sampledHTTPRequest_action :: Lens.Lens' SampledHTTPRequest (Prelude.Maybe Prelude.Text)
sampledHTTPRequest_action = Lens.lens (\SampledHTTPRequest' {action} -> action) (\s@SampledHTTPRequest' {} a -> s {action = a} :: SampledHTTPRequest)

-- | The @CAPTCHA@ response for the request.
sampledHTTPRequest_captchaResponse :: Lens.Lens' SampledHTTPRequest (Prelude.Maybe CaptchaResponse)
sampledHTTPRequest_captchaResponse = Lens.lens (\SampledHTTPRequest' {captchaResponse} -> captchaResponse) (\s@SampledHTTPRequest' {} a -> s {captchaResponse = a} :: SampledHTTPRequest)

-- | The @Challenge@ response for the request.
sampledHTTPRequest_challengeResponse :: Lens.Lens' SampledHTTPRequest (Prelude.Maybe ChallengeResponse)
sampledHTTPRequest_challengeResponse = Lens.lens (\SampledHTTPRequest' {challengeResponse} -> challengeResponse) (\s@SampledHTTPRequest' {} a -> s {challengeResponse = a} :: SampledHTTPRequest)

-- | Labels applied to the web request by matching rules. WAF applies fully
-- qualified labels to matching web requests. A fully qualified label is
-- the concatenation of a label namespace and a rule label. The rule\'s
-- rule group or web ACL defines the label namespace.
--
-- For example,
-- @awswaf:111122223333:myRuleGroup:testRules:testNS1:testNS2:labelNameA@
-- or @awswaf:managed:aws:managed-rule-set:header:encoding:utf8@.
sampledHTTPRequest_labels :: Lens.Lens' SampledHTTPRequest (Prelude.Maybe [Label])
sampledHTTPRequest_labels = Lens.lens (\SampledHTTPRequest' {labels} -> labels) (\s@SampledHTTPRequest' {} a -> s {labels = a} :: SampledHTTPRequest) Prelude.. Lens.mapping Lens.coerced

-- | Used only for rule group rules that have a rule action override in place
-- in the web ACL. This is the action that the rule group rule is
-- configured for, and not the action that was applied to the request. The
-- action that WAF applied is the @Action@ value.
sampledHTTPRequest_overriddenAction :: Lens.Lens' SampledHTTPRequest (Prelude.Maybe Prelude.Text)
sampledHTTPRequest_overriddenAction = Lens.lens (\SampledHTTPRequest' {overriddenAction} -> overriddenAction) (\s@SampledHTTPRequest' {} a -> s {overriddenAction = a} :: SampledHTTPRequest)

-- | Custom request headers inserted by WAF into the request, according to
-- the custom request configuration for the matching rule action.
sampledHTTPRequest_requestHeadersInserted :: Lens.Lens' SampledHTTPRequest (Prelude.Maybe [HTTPHeader])
sampledHTTPRequest_requestHeadersInserted = Lens.lens (\SampledHTTPRequest' {requestHeadersInserted} -> requestHeadersInserted) (\s@SampledHTTPRequest' {} a -> s {requestHeadersInserted = a} :: SampledHTTPRequest) Prelude.. Lens.mapping Lens.coerced

-- | The response code that was sent for the request.
sampledHTTPRequest_responseCodeSent :: Lens.Lens' SampledHTTPRequest (Prelude.Maybe Prelude.Natural)
sampledHTTPRequest_responseCodeSent = Lens.lens (\SampledHTTPRequest' {responseCodeSent} -> responseCodeSent) (\s@SampledHTTPRequest' {} a -> s {responseCodeSent = a} :: SampledHTTPRequest)

-- | The name of the @Rule@ that the request matched. For managed rule
-- groups, the format for this name is
-- @\<vendor name>#\<managed rule group name>#\<rule name>@. For your own
-- rule groups, the format for this name is
-- @\<rule group name>#\<rule name>@. If the rule is not in a rule group,
-- this field is absent.
sampledHTTPRequest_ruleNameWithinRuleGroup :: Lens.Lens' SampledHTTPRequest (Prelude.Maybe Prelude.Text)
sampledHTTPRequest_ruleNameWithinRuleGroup = Lens.lens (\SampledHTTPRequest' {ruleNameWithinRuleGroup} -> ruleNameWithinRuleGroup) (\s@SampledHTTPRequest' {} a -> s {ruleNameWithinRuleGroup = a} :: SampledHTTPRequest)

-- | The time at which WAF received the request from your Amazon Web Services
-- resource, in Unix time format (in seconds).
sampledHTTPRequest_timestamp :: Lens.Lens' SampledHTTPRequest (Prelude.Maybe Prelude.UTCTime)
sampledHTTPRequest_timestamp = Lens.lens (\SampledHTTPRequest' {timestamp} -> timestamp) (\s@SampledHTTPRequest' {} a -> s {timestamp = a} :: SampledHTTPRequest) Prelude.. Lens.mapping Data._Time

-- | A complex type that contains detailed information about the request.
sampledHTTPRequest_request :: Lens.Lens' SampledHTTPRequest HTTPRequest
sampledHTTPRequest_request = Lens.lens (\SampledHTTPRequest' {request} -> request) (\s@SampledHTTPRequest' {} a -> s {request = a} :: SampledHTTPRequest)

-- | A value that indicates how one result in the response relates
-- proportionally to other results in the response. For example, a result
-- that has a weight of @2@ represents roughly twice as many web requests
-- as a result that has a weight of @1@.
sampledHTTPRequest_weight :: Lens.Lens' SampledHTTPRequest Prelude.Natural
sampledHTTPRequest_weight = Lens.lens (\SampledHTTPRequest' {weight} -> weight) (\s@SampledHTTPRequest' {} a -> s {weight = a} :: SampledHTTPRequest)

instance Data.FromJSON SampledHTTPRequest where
  parseJSON =
    Data.withObject
      "SampledHTTPRequest"
      ( \x ->
          SampledHTTPRequest'
            Prelude.<$> (x Data..:? "Action")
            Prelude.<*> (x Data..:? "CaptchaResponse")
            Prelude.<*> (x Data..:? "ChallengeResponse")
            Prelude.<*> (x Data..:? "Labels" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "OverriddenAction")
            Prelude.<*> ( x
                            Data..:? "RequestHeadersInserted"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ResponseCodeSent")
            Prelude.<*> (x Data..:? "RuleNameWithinRuleGroup")
            Prelude.<*> (x Data..:? "Timestamp")
            Prelude.<*> (x Data..: "Request")
            Prelude.<*> (x Data..: "Weight")
      )

instance Prelude.Hashable SampledHTTPRequest where
  hashWithSalt _salt SampledHTTPRequest' {..} =
    _salt
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` captchaResponse
      `Prelude.hashWithSalt` challengeResponse
      `Prelude.hashWithSalt` labels
      `Prelude.hashWithSalt` overriddenAction
      `Prelude.hashWithSalt` requestHeadersInserted
      `Prelude.hashWithSalt` responseCodeSent
      `Prelude.hashWithSalt` ruleNameWithinRuleGroup
      `Prelude.hashWithSalt` timestamp
      `Prelude.hashWithSalt` request
      `Prelude.hashWithSalt` weight

instance Prelude.NFData SampledHTTPRequest where
  rnf SampledHTTPRequest' {..} =
    Prelude.rnf action
      `Prelude.seq` Prelude.rnf captchaResponse
      `Prelude.seq` Prelude.rnf challengeResponse
      `Prelude.seq` Prelude.rnf labels
      `Prelude.seq` Prelude.rnf overriddenAction
      `Prelude.seq` Prelude.rnf requestHeadersInserted
      `Prelude.seq` Prelude.rnf responseCodeSent
      `Prelude.seq` Prelude.rnf ruleNameWithinRuleGroup
      `Prelude.seq` Prelude.rnf timestamp
      `Prelude.seq` Prelude.rnf request
      `Prelude.seq` Prelude.rnf weight
