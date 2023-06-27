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
-- Module      : Amazonka.WAFV2.CreateWebACL
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a WebACL per the specifications provided.
--
-- A web ACL defines a collection of rules to use to inspect and control
-- web requests. Each rule has an action defined (allow, block, or count)
-- for requests that match the statement of the rule. In the web ACL, you
-- assign a default action to take (allow, block) for any request that does
-- not match any of the rules. The rules in a web ACL can be a combination
-- of the types Rule, RuleGroup, and managed rule group. You can associate
-- a web ACL with one or more Amazon Web Services resources to protect. The
-- resources can be an Amazon CloudFront distribution, an Amazon API
-- Gateway REST API, an Application Load Balancer, an AppSync GraphQL API,
-- an Amazon Cognito user pool, an App Runner service, or an Amazon Web
-- Services Verified Access instance.
module Amazonka.WAFV2.CreateWebACL
  ( -- * Creating a Request
    CreateWebACL (..),
    newCreateWebACL,

    -- * Request Lenses
    createWebACL_associationConfig,
    createWebACL_captchaConfig,
    createWebACL_challengeConfig,
    createWebACL_customResponseBodies,
    createWebACL_description,
    createWebACL_rules,
    createWebACL_tags,
    createWebACL_tokenDomains,
    createWebACL_name,
    createWebACL_scope,
    createWebACL_defaultAction,
    createWebACL_visibilityConfig,

    -- * Destructuring the Response
    CreateWebACLResponse (..),
    newCreateWebACLResponse,

    -- * Response Lenses
    createWebACLResponse_summary,
    createWebACLResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFV2.Types

-- | /See:/ 'newCreateWebACL' smart constructor.
data CreateWebACL = CreateWebACL'
  { -- | Specifies custom configurations for the associations between the web ACL
    -- and protected resources.
    --
    -- Use this to customize the maximum size of the request body that your
    -- protected CloudFront distributions forward to WAF for inspection. The
    -- default is 16 KB (16,384 kilobytes).
    --
    -- You are charged additional fees when your protected resources forward
    -- body sizes that are larger than the default. For more information, see
    -- <http://aws.amazon.com/waf/pricing/ WAF Pricing>.
    associationConfig :: Prelude.Maybe AssociationConfig,
    -- | Specifies how WAF should handle @CAPTCHA@ evaluations for rules that
    -- don\'t have their own @CaptchaConfig@ settings. If you don\'t specify
    -- this, WAF uses its default settings for @CaptchaConfig@.
    captchaConfig :: Prelude.Maybe CaptchaConfig,
    -- | Specifies how WAF should handle challenge evaluations for rules that
    -- don\'t have their own @ChallengeConfig@ settings. If you don\'t specify
    -- this, WAF uses its default settings for @ChallengeConfig@.
    challengeConfig :: Prelude.Maybe ChallengeConfig,
    -- | A map of custom response keys and content bodies. When you create a rule
    -- with a block action, you can send a custom response to the web request.
    -- You define these for the web ACL, and then use them in the rules and
    -- default actions that you define in the web ACL.
    --
    -- For information about customizing web requests and responses, see
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-custom-request-response.html Customizing web requests and responses in WAF>
    -- in the /WAF Developer Guide/.
    --
    -- For information about the limits on count and size for custom request
    -- and response settings, see
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/limits.html WAF quotas>
    -- in the /WAF Developer Guide/.
    customResponseBodies :: Prelude.Maybe (Prelude.HashMap Prelude.Text CustomResponseBody),
    -- | A description of the web ACL that helps with identification.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Rule statements used to identify the web requests that you want to
    -- allow, block, or count. Each rule includes one top-level statement that
    -- WAF uses to identify matching web requests, and parameters that govern
    -- how WAF handles them.
    rules :: Prelude.Maybe [Rule],
    -- | An array of key:value pairs to associate with the resource.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | Specifies the domains that WAF should accept in a web request token.
    -- This enables the use of tokens across multiple protected websites. When
    -- WAF provides a token, it uses the domain of the Amazon Web Services
    -- resource that the web ACL is protecting. If you don\'t specify a list of
    -- token domains, WAF accepts tokens only for the domain of the protected
    -- resource. With a token domain list, WAF accepts the resource\'s host
    -- domain plus all domains in the token domain list, including their
    -- prefixed subdomains.
    --
    -- Example JSON:
    -- @\"TokenDomains\": { \"mywebsite.com\", \"myotherwebsite.com\" }@
    --
    -- Public suffixes aren\'t allowed. For example, you can\'t use @usa.gov@
    -- or @co.uk@ as token domains.
    tokenDomains :: Prelude.Maybe [Prelude.Text],
    -- | The name of the web ACL. You cannot change the name of a web ACL after
    -- you create it.
    name :: Prelude.Text,
    -- | Specifies whether this is for an Amazon CloudFront distribution or for a
    -- regional application. A regional application can be an Application Load
    -- Balancer (ALB), an Amazon API Gateway REST API, an AppSync GraphQL API,
    -- an Amazon Cognito user pool, an App Runner service, or an Amazon Web
    -- Services Verified Access instance.
    --
    -- To work with CloudFront, you must also specify the Region US East (N.
    -- Virginia) as follows:
    --
    -- -   CLI - Specify the Region when you use the CloudFront scope:
    --     @--scope=CLOUDFRONT --region=us-east-1@.
    --
    -- -   API and SDKs - For all calls, use the Region endpoint us-east-1.
    scope :: Scope,
    -- | The action to perform if none of the @Rules@ contained in the @WebACL@
    -- match.
    defaultAction :: DefaultAction,
    -- | Defines and enables Amazon CloudWatch metrics and web request sample
    -- collection.
    visibilityConfig :: VisibilityConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWebACL' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationConfig', 'createWebACL_associationConfig' - Specifies custom configurations for the associations between the web ACL
-- and protected resources.
--
-- Use this to customize the maximum size of the request body that your
-- protected CloudFront distributions forward to WAF for inspection. The
-- default is 16 KB (16,384 kilobytes).
--
-- You are charged additional fees when your protected resources forward
-- body sizes that are larger than the default. For more information, see
-- <http://aws.amazon.com/waf/pricing/ WAF Pricing>.
--
-- 'captchaConfig', 'createWebACL_captchaConfig' - Specifies how WAF should handle @CAPTCHA@ evaluations for rules that
-- don\'t have their own @CaptchaConfig@ settings. If you don\'t specify
-- this, WAF uses its default settings for @CaptchaConfig@.
--
-- 'challengeConfig', 'createWebACL_challengeConfig' - Specifies how WAF should handle challenge evaluations for rules that
-- don\'t have their own @ChallengeConfig@ settings. If you don\'t specify
-- this, WAF uses its default settings for @ChallengeConfig@.
--
-- 'customResponseBodies', 'createWebACL_customResponseBodies' - A map of custom response keys and content bodies. When you create a rule
-- with a block action, you can send a custom response to the web request.
-- You define these for the web ACL, and then use them in the rules and
-- default actions that you define in the web ACL.
--
-- For information about customizing web requests and responses, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-custom-request-response.html Customizing web requests and responses in WAF>
-- in the /WAF Developer Guide/.
--
-- For information about the limits on count and size for custom request
-- and response settings, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/limits.html WAF quotas>
-- in the /WAF Developer Guide/.
--
-- 'description', 'createWebACL_description' - A description of the web ACL that helps with identification.
--
-- 'rules', 'createWebACL_rules' - The Rule statements used to identify the web requests that you want to
-- allow, block, or count. Each rule includes one top-level statement that
-- WAF uses to identify matching web requests, and parameters that govern
-- how WAF handles them.
--
-- 'tags', 'createWebACL_tags' - An array of key:value pairs to associate with the resource.
--
-- 'tokenDomains', 'createWebACL_tokenDomains' - Specifies the domains that WAF should accept in a web request token.
-- This enables the use of tokens across multiple protected websites. When
-- WAF provides a token, it uses the domain of the Amazon Web Services
-- resource that the web ACL is protecting. If you don\'t specify a list of
-- token domains, WAF accepts tokens only for the domain of the protected
-- resource. With a token domain list, WAF accepts the resource\'s host
-- domain plus all domains in the token domain list, including their
-- prefixed subdomains.
--
-- Example JSON:
-- @\"TokenDomains\": { \"mywebsite.com\", \"myotherwebsite.com\" }@
--
-- Public suffixes aren\'t allowed. For example, you can\'t use @usa.gov@
-- or @co.uk@ as token domains.
--
-- 'name', 'createWebACL_name' - The name of the web ACL. You cannot change the name of a web ACL after
-- you create it.
--
-- 'scope', 'createWebACL_scope' - Specifies whether this is for an Amazon CloudFront distribution or for a
-- regional application. A regional application can be an Application Load
-- Balancer (ALB), an Amazon API Gateway REST API, an AppSync GraphQL API,
-- an Amazon Cognito user pool, an App Runner service, or an Amazon Web
-- Services Verified Access instance.
--
-- To work with CloudFront, you must also specify the Region US East (N.
-- Virginia) as follows:
--
-- -   CLI - Specify the Region when you use the CloudFront scope:
--     @--scope=CLOUDFRONT --region=us-east-1@.
--
-- -   API and SDKs - For all calls, use the Region endpoint us-east-1.
--
-- 'defaultAction', 'createWebACL_defaultAction' - The action to perform if none of the @Rules@ contained in the @WebACL@
-- match.
--
-- 'visibilityConfig', 'createWebACL_visibilityConfig' - Defines and enables Amazon CloudWatch metrics and web request sample
-- collection.
newCreateWebACL ::
  -- | 'name'
  Prelude.Text ->
  -- | 'scope'
  Scope ->
  -- | 'defaultAction'
  DefaultAction ->
  -- | 'visibilityConfig'
  VisibilityConfig ->
  CreateWebACL
newCreateWebACL
  pName_
  pScope_
  pDefaultAction_
  pVisibilityConfig_ =
    CreateWebACL'
      { associationConfig = Prelude.Nothing,
        captchaConfig = Prelude.Nothing,
        challengeConfig = Prelude.Nothing,
        customResponseBodies = Prelude.Nothing,
        description = Prelude.Nothing,
        rules = Prelude.Nothing,
        tags = Prelude.Nothing,
        tokenDomains = Prelude.Nothing,
        name = pName_,
        scope = pScope_,
        defaultAction = pDefaultAction_,
        visibilityConfig = pVisibilityConfig_
      }

-- | Specifies custom configurations for the associations between the web ACL
-- and protected resources.
--
-- Use this to customize the maximum size of the request body that your
-- protected CloudFront distributions forward to WAF for inspection. The
-- default is 16 KB (16,384 kilobytes).
--
-- You are charged additional fees when your protected resources forward
-- body sizes that are larger than the default. For more information, see
-- <http://aws.amazon.com/waf/pricing/ WAF Pricing>.
createWebACL_associationConfig :: Lens.Lens' CreateWebACL (Prelude.Maybe AssociationConfig)
createWebACL_associationConfig = Lens.lens (\CreateWebACL' {associationConfig} -> associationConfig) (\s@CreateWebACL' {} a -> s {associationConfig = a} :: CreateWebACL)

-- | Specifies how WAF should handle @CAPTCHA@ evaluations for rules that
-- don\'t have their own @CaptchaConfig@ settings. If you don\'t specify
-- this, WAF uses its default settings for @CaptchaConfig@.
createWebACL_captchaConfig :: Lens.Lens' CreateWebACL (Prelude.Maybe CaptchaConfig)
createWebACL_captchaConfig = Lens.lens (\CreateWebACL' {captchaConfig} -> captchaConfig) (\s@CreateWebACL' {} a -> s {captchaConfig = a} :: CreateWebACL)

-- | Specifies how WAF should handle challenge evaluations for rules that
-- don\'t have their own @ChallengeConfig@ settings. If you don\'t specify
-- this, WAF uses its default settings for @ChallengeConfig@.
createWebACL_challengeConfig :: Lens.Lens' CreateWebACL (Prelude.Maybe ChallengeConfig)
createWebACL_challengeConfig = Lens.lens (\CreateWebACL' {challengeConfig} -> challengeConfig) (\s@CreateWebACL' {} a -> s {challengeConfig = a} :: CreateWebACL)

-- | A map of custom response keys and content bodies. When you create a rule
-- with a block action, you can send a custom response to the web request.
-- You define these for the web ACL, and then use them in the rules and
-- default actions that you define in the web ACL.
--
-- For information about customizing web requests and responses, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-custom-request-response.html Customizing web requests and responses in WAF>
-- in the /WAF Developer Guide/.
--
-- For information about the limits on count and size for custom request
-- and response settings, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/limits.html WAF quotas>
-- in the /WAF Developer Guide/.
createWebACL_customResponseBodies :: Lens.Lens' CreateWebACL (Prelude.Maybe (Prelude.HashMap Prelude.Text CustomResponseBody))
createWebACL_customResponseBodies = Lens.lens (\CreateWebACL' {customResponseBodies} -> customResponseBodies) (\s@CreateWebACL' {} a -> s {customResponseBodies = a} :: CreateWebACL) Prelude.. Lens.mapping Lens.coerced

-- | A description of the web ACL that helps with identification.
createWebACL_description :: Lens.Lens' CreateWebACL (Prelude.Maybe Prelude.Text)
createWebACL_description = Lens.lens (\CreateWebACL' {description} -> description) (\s@CreateWebACL' {} a -> s {description = a} :: CreateWebACL)

-- | The Rule statements used to identify the web requests that you want to
-- allow, block, or count. Each rule includes one top-level statement that
-- WAF uses to identify matching web requests, and parameters that govern
-- how WAF handles them.
createWebACL_rules :: Lens.Lens' CreateWebACL (Prelude.Maybe [Rule])
createWebACL_rules = Lens.lens (\CreateWebACL' {rules} -> rules) (\s@CreateWebACL' {} a -> s {rules = a} :: CreateWebACL) Prelude.. Lens.mapping Lens.coerced

-- | An array of key:value pairs to associate with the resource.
createWebACL_tags :: Lens.Lens' CreateWebACL (Prelude.Maybe (Prelude.NonEmpty Tag))
createWebACL_tags = Lens.lens (\CreateWebACL' {tags} -> tags) (\s@CreateWebACL' {} a -> s {tags = a} :: CreateWebACL) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the domains that WAF should accept in a web request token.
-- This enables the use of tokens across multiple protected websites. When
-- WAF provides a token, it uses the domain of the Amazon Web Services
-- resource that the web ACL is protecting. If you don\'t specify a list of
-- token domains, WAF accepts tokens only for the domain of the protected
-- resource. With a token domain list, WAF accepts the resource\'s host
-- domain plus all domains in the token domain list, including their
-- prefixed subdomains.
--
-- Example JSON:
-- @\"TokenDomains\": { \"mywebsite.com\", \"myotherwebsite.com\" }@
--
-- Public suffixes aren\'t allowed. For example, you can\'t use @usa.gov@
-- or @co.uk@ as token domains.
createWebACL_tokenDomains :: Lens.Lens' CreateWebACL (Prelude.Maybe [Prelude.Text])
createWebACL_tokenDomains = Lens.lens (\CreateWebACL' {tokenDomains} -> tokenDomains) (\s@CreateWebACL' {} a -> s {tokenDomains = a} :: CreateWebACL) Prelude.. Lens.mapping Lens.coerced

-- | The name of the web ACL. You cannot change the name of a web ACL after
-- you create it.
createWebACL_name :: Lens.Lens' CreateWebACL Prelude.Text
createWebACL_name = Lens.lens (\CreateWebACL' {name} -> name) (\s@CreateWebACL' {} a -> s {name = a} :: CreateWebACL)

-- | Specifies whether this is for an Amazon CloudFront distribution or for a
-- regional application. A regional application can be an Application Load
-- Balancer (ALB), an Amazon API Gateway REST API, an AppSync GraphQL API,
-- an Amazon Cognito user pool, an App Runner service, or an Amazon Web
-- Services Verified Access instance.
--
-- To work with CloudFront, you must also specify the Region US East (N.
-- Virginia) as follows:
--
-- -   CLI - Specify the Region when you use the CloudFront scope:
--     @--scope=CLOUDFRONT --region=us-east-1@.
--
-- -   API and SDKs - For all calls, use the Region endpoint us-east-1.
createWebACL_scope :: Lens.Lens' CreateWebACL Scope
createWebACL_scope = Lens.lens (\CreateWebACL' {scope} -> scope) (\s@CreateWebACL' {} a -> s {scope = a} :: CreateWebACL)

-- | The action to perform if none of the @Rules@ contained in the @WebACL@
-- match.
createWebACL_defaultAction :: Lens.Lens' CreateWebACL DefaultAction
createWebACL_defaultAction = Lens.lens (\CreateWebACL' {defaultAction} -> defaultAction) (\s@CreateWebACL' {} a -> s {defaultAction = a} :: CreateWebACL)

-- | Defines and enables Amazon CloudWatch metrics and web request sample
-- collection.
createWebACL_visibilityConfig :: Lens.Lens' CreateWebACL VisibilityConfig
createWebACL_visibilityConfig = Lens.lens (\CreateWebACL' {visibilityConfig} -> visibilityConfig) (\s@CreateWebACL' {} a -> s {visibilityConfig = a} :: CreateWebACL)

instance Core.AWSRequest CreateWebACL where
  type AWSResponse CreateWebACL = CreateWebACLResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateWebACLResponse'
            Prelude.<$> (x Data..?> "Summary")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateWebACL where
  hashWithSalt _salt CreateWebACL' {..} =
    _salt
      `Prelude.hashWithSalt` associationConfig
      `Prelude.hashWithSalt` captchaConfig
      `Prelude.hashWithSalt` challengeConfig
      `Prelude.hashWithSalt` customResponseBodies
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` rules
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` tokenDomains
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` scope
      `Prelude.hashWithSalt` defaultAction
      `Prelude.hashWithSalt` visibilityConfig

instance Prelude.NFData CreateWebACL where
  rnf CreateWebACL' {..} =
    Prelude.rnf associationConfig
      `Prelude.seq` Prelude.rnf captchaConfig
      `Prelude.seq` Prelude.rnf challengeConfig
      `Prelude.seq` Prelude.rnf customResponseBodies
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf rules
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf tokenDomains
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf scope
      `Prelude.seq` Prelude.rnf defaultAction
      `Prelude.seq` Prelude.rnf visibilityConfig

instance Data.ToHeaders CreateWebACL where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_20190729.CreateWebACL" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateWebACL where
  toJSON CreateWebACL' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AssociationConfig" Data..=)
              Prelude.<$> associationConfig,
            ("CaptchaConfig" Data..=) Prelude.<$> captchaConfig,
            ("ChallengeConfig" Data..=)
              Prelude.<$> challengeConfig,
            ("CustomResponseBodies" Data..=)
              Prelude.<$> customResponseBodies,
            ("Description" Data..=) Prelude.<$> description,
            ("Rules" Data..=) Prelude.<$> rules,
            ("Tags" Data..=) Prelude.<$> tags,
            ("TokenDomains" Data..=) Prelude.<$> tokenDomains,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Scope" Data..= scope),
            Prelude.Just ("DefaultAction" Data..= defaultAction),
            Prelude.Just
              ("VisibilityConfig" Data..= visibilityConfig)
          ]
      )

instance Data.ToPath CreateWebACL where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateWebACL where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateWebACLResponse' smart constructor.
data CreateWebACLResponse = CreateWebACLResponse'
  { -- | High-level information about a WebACL, returned by operations like
    -- create and list. This provides information like the ID, that you can use
    -- to retrieve and manage a @WebACL@, and the ARN, that you provide to
    -- operations like AssociateWebACL.
    summary :: Prelude.Maybe WebACLSummary,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWebACLResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'summary', 'createWebACLResponse_summary' - High-level information about a WebACL, returned by operations like
-- create and list. This provides information like the ID, that you can use
-- to retrieve and manage a @WebACL@, and the ARN, that you provide to
-- operations like AssociateWebACL.
--
-- 'httpStatus', 'createWebACLResponse_httpStatus' - The response's http status code.
newCreateWebACLResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateWebACLResponse
newCreateWebACLResponse pHttpStatus_ =
  CreateWebACLResponse'
    { summary = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | High-level information about a WebACL, returned by operations like
-- create and list. This provides information like the ID, that you can use
-- to retrieve and manage a @WebACL@, and the ARN, that you provide to
-- operations like AssociateWebACL.
createWebACLResponse_summary :: Lens.Lens' CreateWebACLResponse (Prelude.Maybe WebACLSummary)
createWebACLResponse_summary = Lens.lens (\CreateWebACLResponse' {summary} -> summary) (\s@CreateWebACLResponse' {} a -> s {summary = a} :: CreateWebACLResponse)

-- | The response's http status code.
createWebACLResponse_httpStatus :: Lens.Lens' CreateWebACLResponse Prelude.Int
createWebACLResponse_httpStatus = Lens.lens (\CreateWebACLResponse' {httpStatus} -> httpStatus) (\s@CreateWebACLResponse' {} a -> s {httpStatus = a} :: CreateWebACLResponse)

instance Prelude.NFData CreateWebACLResponse where
  rnf CreateWebACLResponse' {..} =
    Prelude.rnf summary
      `Prelude.seq` Prelude.rnf httpStatus
