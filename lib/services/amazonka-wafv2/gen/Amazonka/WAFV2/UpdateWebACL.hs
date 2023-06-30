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
-- Module      : Amazonka.WAFV2.UpdateWebACL
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified WebACL. While updating a web ACL, WAF provides
-- continuous coverage to the resources that you have associated with the
-- web ACL.
--
-- When you make changes to web ACLs or web ACL components, like rules and
-- rule groups, WAF propagates the changes everywhere that the web ACL and
-- its components are stored and used. Your changes are applied within
-- seconds, but there might be a brief period of inconsistency when the
-- changes have arrived in some places and not in others. So, for example,
-- if you change a rule action setting, the action might be the old action
-- in one area and the new action in another area. Or if you add an IP
-- address to an IP set used in a blocking rule, the new address might
-- briefly be blocked in one area while still allowed in another. This
-- temporary inconsistency can occur when you first associate a web ACL
-- with an Amazon Web Services resource and when you change a web ACL that
-- is already associated with a resource. Generally, any inconsistencies of
-- this type last only a few seconds.
--
-- This operation completely replaces the mutable specifications that you
-- already have for the web ACL with the ones that you provide to this
-- call. To modify the web ACL, retrieve it by calling GetWebACL, update
-- the settings as needed, and then provide the complete web ACL
-- specification to this call.
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
-- or an Amazon Cognito user pool.
module Amazonka.WAFV2.UpdateWebACL
  ( -- * Creating a Request
    UpdateWebACL (..),
    newUpdateWebACL,

    -- * Request Lenses
    updateWebACL_captchaConfig,
    updateWebACL_challengeConfig,
    updateWebACL_customResponseBodies,
    updateWebACL_description,
    updateWebACL_rules,
    updateWebACL_tokenDomains,
    updateWebACL_name,
    updateWebACL_scope,
    updateWebACL_id,
    updateWebACL_defaultAction,
    updateWebACL_visibilityConfig,
    updateWebACL_lockToken,

    -- * Destructuring the Response
    UpdateWebACLResponse (..),
    newUpdateWebACLResponse,

    -- * Response Lenses
    updateWebACLResponse_nextLockToken,
    updateWebACLResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFV2.Types

-- | /See:/ 'newUpdateWebACL' smart constructor.
data UpdateWebACL = UpdateWebACL'
  { -- | Specifies how WAF should handle @CAPTCHA@ evaluations for rules that
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
    -- in the
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html WAF Developer Guide>.
    --
    -- For information about the limits on count and size for custom request
    -- and response settings, see
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/limits.html WAF quotas>
    -- in the
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html WAF Developer Guide>.
    customResponseBodies :: Prelude.Maybe (Prelude.HashMap Prelude.Text CustomResponseBody),
    -- | A description of the web ACL that helps with identification.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Rule statements used to identify the web requests that you want to
    -- allow, block, or count. Each rule includes one top-level statement that
    -- WAF uses to identify matching web requests, and parameters that govern
    -- how WAF handles them.
    rules :: Prelude.Maybe [Rule],
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
    tokenDomains :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The name of the web ACL. You cannot change the name of a web ACL after
    -- you create it.
    name :: Prelude.Text,
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
    -- | The unique identifier for the web ACL. This ID is returned in the
    -- responses to create and list commands. You provide it to operations like
    -- update and delete.
    id :: Prelude.Text,
    -- | The action to perform if none of the @Rules@ contained in the @WebACL@
    -- match.
    defaultAction :: DefaultAction,
    -- | Defines and enables Amazon CloudWatch metrics and web request sample
    -- collection.
    visibilityConfig :: VisibilityConfig,
    -- | A token used for optimistic locking. WAF returns a token to your @get@
    -- and @list@ requests, to mark the state of the entity at the time of the
    -- request. To make changes to the entity associated with the token, you
    -- provide the token to operations like @update@ and @delete@. WAF uses the
    -- token to ensure that no changes have been made to the entity since you
    -- last retrieved it. If a change has been made, the update fails with a
    -- @WAFOptimisticLockException@. If this happens, perform another @get@,
    -- and use the new token returned by that operation.
    lockToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateWebACL' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'captchaConfig', 'updateWebACL_captchaConfig' - Specifies how WAF should handle @CAPTCHA@ evaluations for rules that
-- don\'t have their own @CaptchaConfig@ settings. If you don\'t specify
-- this, WAF uses its default settings for @CaptchaConfig@.
--
-- 'challengeConfig', 'updateWebACL_challengeConfig' - Specifies how WAF should handle challenge evaluations for rules that
-- don\'t have their own @ChallengeConfig@ settings. If you don\'t specify
-- this, WAF uses its default settings for @ChallengeConfig@.
--
-- 'customResponseBodies', 'updateWebACL_customResponseBodies' - A map of custom response keys and content bodies. When you create a rule
-- with a block action, you can send a custom response to the web request.
-- You define these for the web ACL, and then use them in the rules and
-- default actions that you define in the web ACL.
--
-- For information about customizing web requests and responses, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-custom-request-response.html Customizing web requests and responses in WAF>
-- in the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html WAF Developer Guide>.
--
-- For information about the limits on count and size for custom request
-- and response settings, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/limits.html WAF quotas>
-- in the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html WAF Developer Guide>.
--
-- 'description', 'updateWebACL_description' - A description of the web ACL that helps with identification.
--
-- 'rules', 'updateWebACL_rules' - The Rule statements used to identify the web requests that you want to
-- allow, block, or count. Each rule includes one top-level statement that
-- WAF uses to identify matching web requests, and parameters that govern
-- how WAF handles them.
--
-- 'tokenDomains', 'updateWebACL_tokenDomains' - Specifies the domains that WAF should accept in a web request token.
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
-- 'name', 'updateWebACL_name' - The name of the web ACL. You cannot change the name of a web ACL after
-- you create it.
--
-- 'scope', 'updateWebACL_scope' - Specifies whether this is for an Amazon CloudFront distribution or for a
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
-- 'id', 'updateWebACL_id' - The unique identifier for the web ACL. This ID is returned in the
-- responses to create and list commands. You provide it to operations like
-- update and delete.
--
-- 'defaultAction', 'updateWebACL_defaultAction' - The action to perform if none of the @Rules@ contained in the @WebACL@
-- match.
--
-- 'visibilityConfig', 'updateWebACL_visibilityConfig' - Defines and enables Amazon CloudWatch metrics and web request sample
-- collection.
--
-- 'lockToken', 'updateWebACL_lockToken' - A token used for optimistic locking. WAF returns a token to your @get@
-- and @list@ requests, to mark the state of the entity at the time of the
-- request. To make changes to the entity associated with the token, you
-- provide the token to operations like @update@ and @delete@. WAF uses the
-- token to ensure that no changes have been made to the entity since you
-- last retrieved it. If a change has been made, the update fails with a
-- @WAFOptimisticLockException@. If this happens, perform another @get@,
-- and use the new token returned by that operation.
newUpdateWebACL ::
  -- | 'name'
  Prelude.Text ->
  -- | 'scope'
  Scope ->
  -- | 'id'
  Prelude.Text ->
  -- | 'defaultAction'
  DefaultAction ->
  -- | 'visibilityConfig'
  VisibilityConfig ->
  -- | 'lockToken'
  Prelude.Text ->
  UpdateWebACL
newUpdateWebACL
  pName_
  pScope_
  pId_
  pDefaultAction_
  pVisibilityConfig_
  pLockToken_ =
    UpdateWebACL'
      { captchaConfig = Prelude.Nothing,
        challengeConfig = Prelude.Nothing,
        customResponseBodies = Prelude.Nothing,
        description = Prelude.Nothing,
        rules = Prelude.Nothing,
        tokenDomains = Prelude.Nothing,
        name = pName_,
        scope = pScope_,
        id = pId_,
        defaultAction = pDefaultAction_,
        visibilityConfig = pVisibilityConfig_,
        lockToken = pLockToken_
      }

-- | Specifies how WAF should handle @CAPTCHA@ evaluations for rules that
-- don\'t have their own @CaptchaConfig@ settings. If you don\'t specify
-- this, WAF uses its default settings for @CaptchaConfig@.
updateWebACL_captchaConfig :: Lens.Lens' UpdateWebACL (Prelude.Maybe CaptchaConfig)
updateWebACL_captchaConfig = Lens.lens (\UpdateWebACL' {captchaConfig} -> captchaConfig) (\s@UpdateWebACL' {} a -> s {captchaConfig = a} :: UpdateWebACL)

-- | Specifies how WAF should handle challenge evaluations for rules that
-- don\'t have their own @ChallengeConfig@ settings. If you don\'t specify
-- this, WAF uses its default settings for @ChallengeConfig@.
updateWebACL_challengeConfig :: Lens.Lens' UpdateWebACL (Prelude.Maybe ChallengeConfig)
updateWebACL_challengeConfig = Lens.lens (\UpdateWebACL' {challengeConfig} -> challengeConfig) (\s@UpdateWebACL' {} a -> s {challengeConfig = a} :: UpdateWebACL)

-- | A map of custom response keys and content bodies. When you create a rule
-- with a block action, you can send a custom response to the web request.
-- You define these for the web ACL, and then use them in the rules and
-- default actions that you define in the web ACL.
--
-- For information about customizing web requests and responses, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-custom-request-response.html Customizing web requests and responses in WAF>
-- in the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html WAF Developer Guide>.
--
-- For information about the limits on count and size for custom request
-- and response settings, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/limits.html WAF quotas>
-- in the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html WAF Developer Guide>.
updateWebACL_customResponseBodies :: Lens.Lens' UpdateWebACL (Prelude.Maybe (Prelude.HashMap Prelude.Text CustomResponseBody))
updateWebACL_customResponseBodies = Lens.lens (\UpdateWebACL' {customResponseBodies} -> customResponseBodies) (\s@UpdateWebACL' {} a -> s {customResponseBodies = a} :: UpdateWebACL) Prelude.. Lens.mapping Lens.coerced

-- | A description of the web ACL that helps with identification.
updateWebACL_description :: Lens.Lens' UpdateWebACL (Prelude.Maybe Prelude.Text)
updateWebACL_description = Lens.lens (\UpdateWebACL' {description} -> description) (\s@UpdateWebACL' {} a -> s {description = a} :: UpdateWebACL)

-- | The Rule statements used to identify the web requests that you want to
-- allow, block, or count. Each rule includes one top-level statement that
-- WAF uses to identify matching web requests, and parameters that govern
-- how WAF handles them.
updateWebACL_rules :: Lens.Lens' UpdateWebACL (Prelude.Maybe [Rule])
updateWebACL_rules = Lens.lens (\UpdateWebACL' {rules} -> rules) (\s@UpdateWebACL' {} a -> s {rules = a} :: UpdateWebACL) Prelude.. Lens.mapping Lens.coerced

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
updateWebACL_tokenDomains :: Lens.Lens' UpdateWebACL (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
updateWebACL_tokenDomains = Lens.lens (\UpdateWebACL' {tokenDomains} -> tokenDomains) (\s@UpdateWebACL' {} a -> s {tokenDomains = a} :: UpdateWebACL) Prelude.. Lens.mapping Lens.coerced

-- | The name of the web ACL. You cannot change the name of a web ACL after
-- you create it.
updateWebACL_name :: Lens.Lens' UpdateWebACL Prelude.Text
updateWebACL_name = Lens.lens (\UpdateWebACL' {name} -> name) (\s@UpdateWebACL' {} a -> s {name = a} :: UpdateWebACL)

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
updateWebACL_scope :: Lens.Lens' UpdateWebACL Scope
updateWebACL_scope = Lens.lens (\UpdateWebACL' {scope} -> scope) (\s@UpdateWebACL' {} a -> s {scope = a} :: UpdateWebACL)

-- | The unique identifier for the web ACL. This ID is returned in the
-- responses to create and list commands. You provide it to operations like
-- update and delete.
updateWebACL_id :: Lens.Lens' UpdateWebACL Prelude.Text
updateWebACL_id = Lens.lens (\UpdateWebACL' {id} -> id) (\s@UpdateWebACL' {} a -> s {id = a} :: UpdateWebACL)

-- | The action to perform if none of the @Rules@ contained in the @WebACL@
-- match.
updateWebACL_defaultAction :: Lens.Lens' UpdateWebACL DefaultAction
updateWebACL_defaultAction = Lens.lens (\UpdateWebACL' {defaultAction} -> defaultAction) (\s@UpdateWebACL' {} a -> s {defaultAction = a} :: UpdateWebACL)

-- | Defines and enables Amazon CloudWatch metrics and web request sample
-- collection.
updateWebACL_visibilityConfig :: Lens.Lens' UpdateWebACL VisibilityConfig
updateWebACL_visibilityConfig = Lens.lens (\UpdateWebACL' {visibilityConfig} -> visibilityConfig) (\s@UpdateWebACL' {} a -> s {visibilityConfig = a} :: UpdateWebACL)

-- | A token used for optimistic locking. WAF returns a token to your @get@
-- and @list@ requests, to mark the state of the entity at the time of the
-- request. To make changes to the entity associated with the token, you
-- provide the token to operations like @update@ and @delete@. WAF uses the
-- token to ensure that no changes have been made to the entity since you
-- last retrieved it. If a change has been made, the update fails with a
-- @WAFOptimisticLockException@. If this happens, perform another @get@,
-- and use the new token returned by that operation.
updateWebACL_lockToken :: Lens.Lens' UpdateWebACL Prelude.Text
updateWebACL_lockToken = Lens.lens (\UpdateWebACL' {lockToken} -> lockToken) (\s@UpdateWebACL' {} a -> s {lockToken = a} :: UpdateWebACL)

instance Core.AWSRequest UpdateWebACL where
  type AWSResponse UpdateWebACL = UpdateWebACLResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateWebACLResponse'
            Prelude.<$> (x Data..?> "NextLockToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateWebACL where
  hashWithSalt _salt UpdateWebACL' {..} =
    _salt
      `Prelude.hashWithSalt` captchaConfig
      `Prelude.hashWithSalt` challengeConfig
      `Prelude.hashWithSalt` customResponseBodies
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` rules
      `Prelude.hashWithSalt` tokenDomains
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` scope
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` defaultAction
      `Prelude.hashWithSalt` visibilityConfig
      `Prelude.hashWithSalt` lockToken

instance Prelude.NFData UpdateWebACL where
  rnf UpdateWebACL' {..} =
    Prelude.rnf captchaConfig
      `Prelude.seq` Prelude.rnf challengeConfig
      `Prelude.seq` Prelude.rnf customResponseBodies
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf rules
      `Prelude.seq` Prelude.rnf tokenDomains
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf scope
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf defaultAction
      `Prelude.seq` Prelude.rnf visibilityConfig
      `Prelude.seq` Prelude.rnf lockToken

instance Data.ToHeaders UpdateWebACL where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_20190729.UpdateWebACL" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateWebACL where
  toJSON UpdateWebACL' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CaptchaConfig" Data..=) Prelude.<$> captchaConfig,
            ("ChallengeConfig" Data..=)
              Prelude.<$> challengeConfig,
            ("CustomResponseBodies" Data..=)
              Prelude.<$> customResponseBodies,
            ("Description" Data..=) Prelude.<$> description,
            ("Rules" Data..=) Prelude.<$> rules,
            ("TokenDomains" Data..=) Prelude.<$> tokenDomains,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Scope" Data..= scope),
            Prelude.Just ("Id" Data..= id),
            Prelude.Just ("DefaultAction" Data..= defaultAction),
            Prelude.Just
              ("VisibilityConfig" Data..= visibilityConfig),
            Prelude.Just ("LockToken" Data..= lockToken)
          ]
      )

instance Data.ToPath UpdateWebACL where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateWebACL where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateWebACLResponse' smart constructor.
data UpdateWebACLResponse = UpdateWebACLResponse'
  { -- | A token used for optimistic locking. WAF returns this token to your
    -- @update@ requests. You use @NextLockToken@ in the same manner as you use
    -- @LockToken@.
    nextLockToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateWebACLResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextLockToken', 'updateWebACLResponse_nextLockToken' - A token used for optimistic locking. WAF returns this token to your
-- @update@ requests. You use @NextLockToken@ in the same manner as you use
-- @LockToken@.
--
-- 'httpStatus', 'updateWebACLResponse_httpStatus' - The response's http status code.
newUpdateWebACLResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateWebACLResponse
newUpdateWebACLResponse pHttpStatus_ =
  UpdateWebACLResponse'
    { nextLockToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token used for optimistic locking. WAF returns this token to your
-- @update@ requests. You use @NextLockToken@ in the same manner as you use
-- @LockToken@.
updateWebACLResponse_nextLockToken :: Lens.Lens' UpdateWebACLResponse (Prelude.Maybe Prelude.Text)
updateWebACLResponse_nextLockToken = Lens.lens (\UpdateWebACLResponse' {nextLockToken} -> nextLockToken) (\s@UpdateWebACLResponse' {} a -> s {nextLockToken = a} :: UpdateWebACLResponse)

-- | The response's http status code.
updateWebACLResponse_httpStatus :: Lens.Lens' UpdateWebACLResponse Prelude.Int
updateWebACLResponse_httpStatus = Lens.lens (\UpdateWebACLResponse' {httpStatus} -> httpStatus) (\s@UpdateWebACLResponse' {} a -> s {httpStatus = a} :: UpdateWebACLResponse)

instance Prelude.NFData UpdateWebACLResponse where
  rnf UpdateWebACLResponse' {..} =
    Prelude.rnf nextLockToken
      `Prelude.seq` Prelude.rnf httpStatus
