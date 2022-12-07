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
-- Module      : Amazonka.WAF.Types.SampledHTTPRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAF.Types.SampledHTTPRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAF.Types.HTTPRequest

-- | This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- The response from a GetSampledRequests request includes a
-- @SampledHTTPRequests@ complex type that appears as @SampledRequests@ in
-- the response syntax. @SampledHTTPRequests@ contains one
-- @SampledHTTPRequest@ object for each web request that is returned by
-- @GetSampledRequests@.
--
-- /See:/ 'newSampledHTTPRequest' smart constructor.
data SampledHTTPRequest = SampledHTTPRequest'
  { -- | This value is returned if the @GetSampledRequests@ request specifies the
    -- ID of a @RuleGroup@ rather than the ID of an individual rule.
    -- @RuleWithinRuleGroup@ is the rule within the specified @RuleGroup@ that
    -- matched the request listed in the response.
    ruleWithinRuleGroup :: Prelude.Maybe Prelude.Text,
    -- | The time at which AWS WAF received the request from your AWS resource,
    -- in Unix time format (in seconds).
    timestamp :: Prelude.Maybe Data.POSIX,
    -- | The action for the @Rule@ that the request matched: @ALLOW@, @BLOCK@, or
    -- @COUNT@.
    action :: Prelude.Maybe Prelude.Text,
    -- | A complex type that contains detailed information about the request.
    request :: HTTPRequest,
    -- | A value that indicates how one result in the response relates
    -- proportionally to other results in the response. A result that has a
    -- weight of @2@ represents roughly twice as many CloudFront web requests
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
-- 'ruleWithinRuleGroup', 'sampledHTTPRequest_ruleWithinRuleGroup' - This value is returned if the @GetSampledRequests@ request specifies the
-- ID of a @RuleGroup@ rather than the ID of an individual rule.
-- @RuleWithinRuleGroup@ is the rule within the specified @RuleGroup@ that
-- matched the request listed in the response.
--
-- 'timestamp', 'sampledHTTPRequest_timestamp' - The time at which AWS WAF received the request from your AWS resource,
-- in Unix time format (in seconds).
--
-- 'action', 'sampledHTTPRequest_action' - The action for the @Rule@ that the request matched: @ALLOW@, @BLOCK@, or
-- @COUNT@.
--
-- 'request', 'sampledHTTPRequest_request' - A complex type that contains detailed information about the request.
--
-- 'weight', 'sampledHTTPRequest_weight' - A value that indicates how one result in the response relates
-- proportionally to other results in the response. A result that has a
-- weight of @2@ represents roughly twice as many CloudFront web requests
-- as a result that has a weight of @1@.
newSampledHTTPRequest ::
  -- | 'request'
  HTTPRequest ->
  -- | 'weight'
  Prelude.Natural ->
  SampledHTTPRequest
newSampledHTTPRequest pRequest_ pWeight_ =
  SampledHTTPRequest'
    { ruleWithinRuleGroup =
        Prelude.Nothing,
      timestamp = Prelude.Nothing,
      action = Prelude.Nothing,
      request = pRequest_,
      weight = pWeight_
    }

-- | This value is returned if the @GetSampledRequests@ request specifies the
-- ID of a @RuleGroup@ rather than the ID of an individual rule.
-- @RuleWithinRuleGroup@ is the rule within the specified @RuleGroup@ that
-- matched the request listed in the response.
sampledHTTPRequest_ruleWithinRuleGroup :: Lens.Lens' SampledHTTPRequest (Prelude.Maybe Prelude.Text)
sampledHTTPRequest_ruleWithinRuleGroup = Lens.lens (\SampledHTTPRequest' {ruleWithinRuleGroup} -> ruleWithinRuleGroup) (\s@SampledHTTPRequest' {} a -> s {ruleWithinRuleGroup = a} :: SampledHTTPRequest)

-- | The time at which AWS WAF received the request from your AWS resource,
-- in Unix time format (in seconds).
sampledHTTPRequest_timestamp :: Lens.Lens' SampledHTTPRequest (Prelude.Maybe Prelude.UTCTime)
sampledHTTPRequest_timestamp = Lens.lens (\SampledHTTPRequest' {timestamp} -> timestamp) (\s@SampledHTTPRequest' {} a -> s {timestamp = a} :: SampledHTTPRequest) Prelude.. Lens.mapping Data._Time

-- | The action for the @Rule@ that the request matched: @ALLOW@, @BLOCK@, or
-- @COUNT@.
sampledHTTPRequest_action :: Lens.Lens' SampledHTTPRequest (Prelude.Maybe Prelude.Text)
sampledHTTPRequest_action = Lens.lens (\SampledHTTPRequest' {action} -> action) (\s@SampledHTTPRequest' {} a -> s {action = a} :: SampledHTTPRequest)

-- | A complex type that contains detailed information about the request.
sampledHTTPRequest_request :: Lens.Lens' SampledHTTPRequest HTTPRequest
sampledHTTPRequest_request = Lens.lens (\SampledHTTPRequest' {request} -> request) (\s@SampledHTTPRequest' {} a -> s {request = a} :: SampledHTTPRequest)

-- | A value that indicates how one result in the response relates
-- proportionally to other results in the response. A result that has a
-- weight of @2@ represents roughly twice as many CloudFront web requests
-- as a result that has a weight of @1@.
sampledHTTPRequest_weight :: Lens.Lens' SampledHTTPRequest Prelude.Natural
sampledHTTPRequest_weight = Lens.lens (\SampledHTTPRequest' {weight} -> weight) (\s@SampledHTTPRequest' {} a -> s {weight = a} :: SampledHTTPRequest)

instance Data.FromJSON SampledHTTPRequest where
  parseJSON =
    Data.withObject
      "SampledHTTPRequest"
      ( \x ->
          SampledHTTPRequest'
            Prelude.<$> (x Data..:? "RuleWithinRuleGroup")
            Prelude.<*> (x Data..:? "Timestamp")
            Prelude.<*> (x Data..:? "Action")
            Prelude.<*> (x Data..: "Request")
            Prelude.<*> (x Data..: "Weight")
      )

instance Prelude.Hashable SampledHTTPRequest where
  hashWithSalt _salt SampledHTTPRequest' {..} =
    _salt `Prelude.hashWithSalt` ruleWithinRuleGroup
      `Prelude.hashWithSalt` timestamp
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` request
      `Prelude.hashWithSalt` weight

instance Prelude.NFData SampledHTTPRequest where
  rnf SampledHTTPRequest' {..} =
    Prelude.rnf ruleWithinRuleGroup
      `Prelude.seq` Prelude.rnf timestamp
      `Prelude.seq` Prelude.rnf action
      `Prelude.seq` Prelude.rnf request
      `Prelude.seq` Prelude.rnf weight
