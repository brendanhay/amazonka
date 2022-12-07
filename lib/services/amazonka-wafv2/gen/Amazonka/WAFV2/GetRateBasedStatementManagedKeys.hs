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
-- Module      : Amazonka.WAFV2.GetRateBasedStatementManagedKeys
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the keys that are currently blocked by a rate-based rule
-- instance. The maximum number of managed keys that can be blocked for a
-- single rate-based rule instance is 10,000. If more than 10,000 addresses
-- exceed the rate limit, those with the highest rates are blocked.
--
-- For a rate-based rule that you\'ve defined inside a rule group, provide
-- the name of the rule group reference statement in your request, in
-- addition to the rate-based rule name and the web ACL name.
--
-- WAF monitors web requests and manages keys independently for each unique
-- combination of web ACL, optional rule group, and rate-based rule. For
-- example, if you define a rate-based rule inside a rule group, and then
-- use the rule group in a web ACL, WAF monitors web requests and manages
-- keys for that web ACL, rule group reference statement, and rate-based
-- rule instance. If you use the same rule group in a second web ACL, WAF
-- monitors web requests and manages keys for this second usage completely
-- independent of your first.
module Amazonka.WAFV2.GetRateBasedStatementManagedKeys
  ( -- * Creating a Request
    GetRateBasedStatementManagedKeys (..),
    newGetRateBasedStatementManagedKeys,

    -- * Request Lenses
    getRateBasedStatementManagedKeys_ruleGroupRuleName,
    getRateBasedStatementManagedKeys_scope,
    getRateBasedStatementManagedKeys_webACLName,
    getRateBasedStatementManagedKeys_webACLId,
    getRateBasedStatementManagedKeys_ruleName,

    -- * Destructuring the Response
    GetRateBasedStatementManagedKeysResponse (..),
    newGetRateBasedStatementManagedKeysResponse,

    -- * Response Lenses
    getRateBasedStatementManagedKeysResponse_managedKeysIPV6,
    getRateBasedStatementManagedKeysResponse_managedKeysIPV4,
    getRateBasedStatementManagedKeysResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFV2.Types

-- | /See:/ 'newGetRateBasedStatementManagedKeys' smart constructor.
data GetRateBasedStatementManagedKeys = GetRateBasedStatementManagedKeys'
  { -- | The name of the rule group reference statement in your web ACL. This is
    -- required only when you have the rate-based rule nested inside a rule
    -- group.
    ruleGroupRuleName :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether this is for an Amazon CloudFront distribution or for a
    -- regional application. A regional application can be an Application Load
    -- Balancer (ALB), an Amazon API Gateway REST API, an AppSync GraphQL API,
    -- or an Amazon Cognito user pool.
    --
    -- To work with CloudFront, you must also specify the Region US East (N.
    -- Virginia) as follows:
    --
    -- -   CLI - Specify the Region when you use the CloudFront scope:
    --     @--scope=CLOUDFRONT --region=us-east-1@.
    --
    -- -   API and SDKs - For all calls, use the Region endpoint us-east-1.
    scope :: Scope,
    -- | The name of the web ACL. You cannot change the name of a web ACL after
    -- you create it.
    webACLName :: Prelude.Text,
    -- | The unique identifier for the web ACL. This ID is returned in the
    -- responses to create and list commands. You provide it to operations like
    -- update and delete.
    webACLId :: Prelude.Text,
    -- | The name of the rate-based rule to get the keys for. If you have the
    -- rule defined inside a rule group that you\'re using in your web ACL,
    -- also provide the name of the rule group reference statement in the
    -- request parameter @RuleGroupRuleName@.
    ruleName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRateBasedStatementManagedKeys' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleGroupRuleName', 'getRateBasedStatementManagedKeys_ruleGroupRuleName' - The name of the rule group reference statement in your web ACL. This is
-- required only when you have the rate-based rule nested inside a rule
-- group.
--
-- 'scope', 'getRateBasedStatementManagedKeys_scope' - Specifies whether this is for an Amazon CloudFront distribution or for a
-- regional application. A regional application can be an Application Load
-- Balancer (ALB), an Amazon API Gateway REST API, an AppSync GraphQL API,
-- or an Amazon Cognito user pool.
--
-- To work with CloudFront, you must also specify the Region US East (N.
-- Virginia) as follows:
--
-- -   CLI - Specify the Region when you use the CloudFront scope:
--     @--scope=CLOUDFRONT --region=us-east-1@.
--
-- -   API and SDKs - For all calls, use the Region endpoint us-east-1.
--
-- 'webACLName', 'getRateBasedStatementManagedKeys_webACLName' - The name of the web ACL. You cannot change the name of a web ACL after
-- you create it.
--
-- 'webACLId', 'getRateBasedStatementManagedKeys_webACLId' - The unique identifier for the web ACL. This ID is returned in the
-- responses to create and list commands. You provide it to operations like
-- update and delete.
--
-- 'ruleName', 'getRateBasedStatementManagedKeys_ruleName' - The name of the rate-based rule to get the keys for. If you have the
-- rule defined inside a rule group that you\'re using in your web ACL,
-- also provide the name of the rule group reference statement in the
-- request parameter @RuleGroupRuleName@.
newGetRateBasedStatementManagedKeys ::
  -- | 'scope'
  Scope ->
  -- | 'webACLName'
  Prelude.Text ->
  -- | 'webACLId'
  Prelude.Text ->
  -- | 'ruleName'
  Prelude.Text ->
  GetRateBasedStatementManagedKeys
newGetRateBasedStatementManagedKeys
  pScope_
  pWebACLName_
  pWebACLId_
  pRuleName_ =
    GetRateBasedStatementManagedKeys'
      { ruleGroupRuleName =
          Prelude.Nothing,
        scope = pScope_,
        webACLName = pWebACLName_,
        webACLId = pWebACLId_,
        ruleName = pRuleName_
      }

-- | The name of the rule group reference statement in your web ACL. This is
-- required only when you have the rate-based rule nested inside a rule
-- group.
getRateBasedStatementManagedKeys_ruleGroupRuleName :: Lens.Lens' GetRateBasedStatementManagedKeys (Prelude.Maybe Prelude.Text)
getRateBasedStatementManagedKeys_ruleGroupRuleName = Lens.lens (\GetRateBasedStatementManagedKeys' {ruleGroupRuleName} -> ruleGroupRuleName) (\s@GetRateBasedStatementManagedKeys' {} a -> s {ruleGroupRuleName = a} :: GetRateBasedStatementManagedKeys)

-- | Specifies whether this is for an Amazon CloudFront distribution or for a
-- regional application. A regional application can be an Application Load
-- Balancer (ALB), an Amazon API Gateway REST API, an AppSync GraphQL API,
-- or an Amazon Cognito user pool.
--
-- To work with CloudFront, you must also specify the Region US East (N.
-- Virginia) as follows:
--
-- -   CLI - Specify the Region when you use the CloudFront scope:
--     @--scope=CLOUDFRONT --region=us-east-1@.
--
-- -   API and SDKs - For all calls, use the Region endpoint us-east-1.
getRateBasedStatementManagedKeys_scope :: Lens.Lens' GetRateBasedStatementManagedKeys Scope
getRateBasedStatementManagedKeys_scope = Lens.lens (\GetRateBasedStatementManagedKeys' {scope} -> scope) (\s@GetRateBasedStatementManagedKeys' {} a -> s {scope = a} :: GetRateBasedStatementManagedKeys)

-- | The name of the web ACL. You cannot change the name of a web ACL after
-- you create it.
getRateBasedStatementManagedKeys_webACLName :: Lens.Lens' GetRateBasedStatementManagedKeys Prelude.Text
getRateBasedStatementManagedKeys_webACLName = Lens.lens (\GetRateBasedStatementManagedKeys' {webACLName} -> webACLName) (\s@GetRateBasedStatementManagedKeys' {} a -> s {webACLName = a} :: GetRateBasedStatementManagedKeys)

-- | The unique identifier for the web ACL. This ID is returned in the
-- responses to create and list commands. You provide it to operations like
-- update and delete.
getRateBasedStatementManagedKeys_webACLId :: Lens.Lens' GetRateBasedStatementManagedKeys Prelude.Text
getRateBasedStatementManagedKeys_webACLId = Lens.lens (\GetRateBasedStatementManagedKeys' {webACLId} -> webACLId) (\s@GetRateBasedStatementManagedKeys' {} a -> s {webACLId = a} :: GetRateBasedStatementManagedKeys)

-- | The name of the rate-based rule to get the keys for. If you have the
-- rule defined inside a rule group that you\'re using in your web ACL,
-- also provide the name of the rule group reference statement in the
-- request parameter @RuleGroupRuleName@.
getRateBasedStatementManagedKeys_ruleName :: Lens.Lens' GetRateBasedStatementManagedKeys Prelude.Text
getRateBasedStatementManagedKeys_ruleName = Lens.lens (\GetRateBasedStatementManagedKeys' {ruleName} -> ruleName) (\s@GetRateBasedStatementManagedKeys' {} a -> s {ruleName = a} :: GetRateBasedStatementManagedKeys)

instance
  Core.AWSRequest
    GetRateBasedStatementManagedKeys
  where
  type
    AWSResponse GetRateBasedStatementManagedKeys =
      GetRateBasedStatementManagedKeysResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRateBasedStatementManagedKeysResponse'
            Prelude.<$> (x Data..?> "ManagedKeysIPV6")
            Prelude.<*> (x Data..?> "ManagedKeysIPV4")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetRateBasedStatementManagedKeys
  where
  hashWithSalt
    _salt
    GetRateBasedStatementManagedKeys' {..} =
      _salt `Prelude.hashWithSalt` ruleGroupRuleName
        `Prelude.hashWithSalt` scope
        `Prelude.hashWithSalt` webACLName
        `Prelude.hashWithSalt` webACLId
        `Prelude.hashWithSalt` ruleName

instance
  Prelude.NFData
    GetRateBasedStatementManagedKeys
  where
  rnf GetRateBasedStatementManagedKeys' {..} =
    Prelude.rnf ruleGroupRuleName
      `Prelude.seq` Prelude.rnf scope
      `Prelude.seq` Prelude.rnf webACLName
      `Prelude.seq` Prelude.rnf webACLId
      `Prelude.seq` Prelude.rnf ruleName

instance
  Data.ToHeaders
    GetRateBasedStatementManagedKeys
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_20190729.GetRateBasedStatementManagedKeys" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetRateBasedStatementManagedKeys where
  toJSON GetRateBasedStatementManagedKeys' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RuleGroupRuleName" Data..=)
              Prelude.<$> ruleGroupRuleName,
            Prelude.Just ("Scope" Data..= scope),
            Prelude.Just ("WebACLName" Data..= webACLName),
            Prelude.Just ("WebACLId" Data..= webACLId),
            Prelude.Just ("RuleName" Data..= ruleName)
          ]
      )

instance Data.ToPath GetRateBasedStatementManagedKeys where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    GetRateBasedStatementManagedKeys
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRateBasedStatementManagedKeysResponse' smart constructor.
data GetRateBasedStatementManagedKeysResponse = GetRateBasedStatementManagedKeysResponse'
  { -- | The keys that are of Internet Protocol version 6 (IPv6).
    managedKeysIPV6 :: Prelude.Maybe RateBasedStatementManagedKeysIPSet,
    -- | The keys that are of Internet Protocol version 4 (IPv4).
    managedKeysIPV4 :: Prelude.Maybe RateBasedStatementManagedKeysIPSet,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRateBasedStatementManagedKeysResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'managedKeysIPV6', 'getRateBasedStatementManagedKeysResponse_managedKeysIPV6' - The keys that are of Internet Protocol version 6 (IPv6).
--
-- 'managedKeysIPV4', 'getRateBasedStatementManagedKeysResponse_managedKeysIPV4' - The keys that are of Internet Protocol version 4 (IPv4).
--
-- 'httpStatus', 'getRateBasedStatementManagedKeysResponse_httpStatus' - The response's http status code.
newGetRateBasedStatementManagedKeysResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRateBasedStatementManagedKeysResponse
newGetRateBasedStatementManagedKeysResponse
  pHttpStatus_ =
    GetRateBasedStatementManagedKeysResponse'
      { managedKeysIPV6 =
          Prelude.Nothing,
        managedKeysIPV4 = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The keys that are of Internet Protocol version 6 (IPv6).
getRateBasedStatementManagedKeysResponse_managedKeysIPV6 :: Lens.Lens' GetRateBasedStatementManagedKeysResponse (Prelude.Maybe RateBasedStatementManagedKeysIPSet)
getRateBasedStatementManagedKeysResponse_managedKeysIPV6 = Lens.lens (\GetRateBasedStatementManagedKeysResponse' {managedKeysIPV6} -> managedKeysIPV6) (\s@GetRateBasedStatementManagedKeysResponse' {} a -> s {managedKeysIPV6 = a} :: GetRateBasedStatementManagedKeysResponse)

-- | The keys that are of Internet Protocol version 4 (IPv4).
getRateBasedStatementManagedKeysResponse_managedKeysIPV4 :: Lens.Lens' GetRateBasedStatementManagedKeysResponse (Prelude.Maybe RateBasedStatementManagedKeysIPSet)
getRateBasedStatementManagedKeysResponse_managedKeysIPV4 = Lens.lens (\GetRateBasedStatementManagedKeysResponse' {managedKeysIPV4} -> managedKeysIPV4) (\s@GetRateBasedStatementManagedKeysResponse' {} a -> s {managedKeysIPV4 = a} :: GetRateBasedStatementManagedKeysResponse)

-- | The response's http status code.
getRateBasedStatementManagedKeysResponse_httpStatus :: Lens.Lens' GetRateBasedStatementManagedKeysResponse Prelude.Int
getRateBasedStatementManagedKeysResponse_httpStatus = Lens.lens (\GetRateBasedStatementManagedKeysResponse' {httpStatus} -> httpStatus) (\s@GetRateBasedStatementManagedKeysResponse' {} a -> s {httpStatus = a} :: GetRateBasedStatementManagedKeysResponse)

instance
  Prelude.NFData
    GetRateBasedStatementManagedKeysResponse
  where
  rnf GetRateBasedStatementManagedKeysResponse' {..} =
    Prelude.rnf managedKeysIPV6
      `Prelude.seq` Prelude.rnf managedKeysIPV4
      `Prelude.seq` Prelude.rnf httpStatus
