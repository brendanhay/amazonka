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
-- Module      : Amazonka.WAFV2.UpdateRuleGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified RuleGroup.
--
-- This operation completely replaces the mutable specifications that you
-- already have for the rule group with the ones that you provide to this
-- call. To modify the rule group, retrieve it by calling GetRuleGroup,
-- update the settings as needed, and then provide the complete rule group
-- specification to this call.
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
-- A rule group defines a collection of rules to inspect and control web
-- requests that you can use in a WebACL. When you create a rule group, you
-- define an immutable capacity limit. If you update a rule group, you must
-- stay within the capacity. This allows others to reuse the rule group
-- with confidence in its capacity requirements.
module Amazonka.WAFV2.UpdateRuleGroup
  ( -- * Creating a Request
    UpdateRuleGroup (..),
    newUpdateRuleGroup,

    -- * Request Lenses
    updateRuleGroup_customResponseBodies,
    updateRuleGroup_description,
    updateRuleGroup_rules,
    updateRuleGroup_name,
    updateRuleGroup_scope,
    updateRuleGroup_id,
    updateRuleGroup_visibilityConfig,
    updateRuleGroup_lockToken,

    -- * Destructuring the Response
    UpdateRuleGroupResponse (..),
    newUpdateRuleGroupResponse,

    -- * Response Lenses
    updateRuleGroupResponse_nextLockToken,
    updateRuleGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFV2.Types

-- | /See:/ 'newUpdateRuleGroup' smart constructor.
data UpdateRuleGroup = UpdateRuleGroup'
  { -- | A map of custom response keys and content bodies. When you create a rule
    -- with a block action, you can send a custom response to the web request.
    -- You define these for the rule group, and then use them in the rules that
    -- you define in the rule group.
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
    -- | A description of the rule group that helps with identification.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Rule statements used to identify the web requests that you want to
    -- allow, block, or count. Each rule includes one top-level statement that
    -- WAF uses to identify matching web requests, and parameters that govern
    -- how WAF handles them.
    rules :: Prelude.Maybe [Rule],
    -- | The name of the rule group. You cannot change the name of a rule group
    -- after you create it.
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
    -- | A unique identifier for the rule group. This ID is returned in the
    -- responses to create and list commands. You provide it to operations like
    -- update and delete.
    id :: Prelude.Text,
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
-- Create a value of 'UpdateRuleGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customResponseBodies', 'updateRuleGroup_customResponseBodies' - A map of custom response keys and content bodies. When you create a rule
-- with a block action, you can send a custom response to the web request.
-- You define these for the rule group, and then use them in the rules that
-- you define in the rule group.
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
-- 'description', 'updateRuleGroup_description' - A description of the rule group that helps with identification.
--
-- 'rules', 'updateRuleGroup_rules' - The Rule statements used to identify the web requests that you want to
-- allow, block, or count. Each rule includes one top-level statement that
-- WAF uses to identify matching web requests, and parameters that govern
-- how WAF handles them.
--
-- 'name', 'updateRuleGroup_name' - The name of the rule group. You cannot change the name of a rule group
-- after you create it.
--
-- 'scope', 'updateRuleGroup_scope' - Specifies whether this is for an Amazon CloudFront distribution or for a
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
-- 'id', 'updateRuleGroup_id' - A unique identifier for the rule group. This ID is returned in the
-- responses to create and list commands. You provide it to operations like
-- update and delete.
--
-- 'visibilityConfig', 'updateRuleGroup_visibilityConfig' - Defines and enables Amazon CloudWatch metrics and web request sample
-- collection.
--
-- 'lockToken', 'updateRuleGroup_lockToken' - A token used for optimistic locking. WAF returns a token to your @get@
-- and @list@ requests, to mark the state of the entity at the time of the
-- request. To make changes to the entity associated with the token, you
-- provide the token to operations like @update@ and @delete@. WAF uses the
-- token to ensure that no changes have been made to the entity since you
-- last retrieved it. If a change has been made, the update fails with a
-- @WAFOptimisticLockException@. If this happens, perform another @get@,
-- and use the new token returned by that operation.
newUpdateRuleGroup ::
  -- | 'name'
  Prelude.Text ->
  -- | 'scope'
  Scope ->
  -- | 'id'
  Prelude.Text ->
  -- | 'visibilityConfig'
  VisibilityConfig ->
  -- | 'lockToken'
  Prelude.Text ->
  UpdateRuleGroup
newUpdateRuleGroup
  pName_
  pScope_
  pId_
  pVisibilityConfig_
  pLockToken_ =
    UpdateRuleGroup'
      { customResponseBodies =
          Prelude.Nothing,
        description = Prelude.Nothing,
        rules = Prelude.Nothing,
        name = pName_,
        scope = pScope_,
        id = pId_,
        visibilityConfig = pVisibilityConfig_,
        lockToken = pLockToken_
      }

-- | A map of custom response keys and content bodies. When you create a rule
-- with a block action, you can send a custom response to the web request.
-- You define these for the rule group, and then use them in the rules that
-- you define in the rule group.
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
updateRuleGroup_customResponseBodies :: Lens.Lens' UpdateRuleGroup (Prelude.Maybe (Prelude.HashMap Prelude.Text CustomResponseBody))
updateRuleGroup_customResponseBodies = Lens.lens (\UpdateRuleGroup' {customResponseBodies} -> customResponseBodies) (\s@UpdateRuleGroup' {} a -> s {customResponseBodies = a} :: UpdateRuleGroup) Prelude.. Lens.mapping Lens.coerced

-- | A description of the rule group that helps with identification.
updateRuleGroup_description :: Lens.Lens' UpdateRuleGroup (Prelude.Maybe Prelude.Text)
updateRuleGroup_description = Lens.lens (\UpdateRuleGroup' {description} -> description) (\s@UpdateRuleGroup' {} a -> s {description = a} :: UpdateRuleGroup)

-- | The Rule statements used to identify the web requests that you want to
-- allow, block, or count. Each rule includes one top-level statement that
-- WAF uses to identify matching web requests, and parameters that govern
-- how WAF handles them.
updateRuleGroup_rules :: Lens.Lens' UpdateRuleGroup (Prelude.Maybe [Rule])
updateRuleGroup_rules = Lens.lens (\UpdateRuleGroup' {rules} -> rules) (\s@UpdateRuleGroup' {} a -> s {rules = a} :: UpdateRuleGroup) Prelude.. Lens.mapping Lens.coerced

-- | The name of the rule group. You cannot change the name of a rule group
-- after you create it.
updateRuleGroup_name :: Lens.Lens' UpdateRuleGroup Prelude.Text
updateRuleGroup_name = Lens.lens (\UpdateRuleGroup' {name} -> name) (\s@UpdateRuleGroup' {} a -> s {name = a} :: UpdateRuleGroup)

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
updateRuleGroup_scope :: Lens.Lens' UpdateRuleGroup Scope
updateRuleGroup_scope = Lens.lens (\UpdateRuleGroup' {scope} -> scope) (\s@UpdateRuleGroup' {} a -> s {scope = a} :: UpdateRuleGroup)

-- | A unique identifier for the rule group. This ID is returned in the
-- responses to create and list commands. You provide it to operations like
-- update and delete.
updateRuleGroup_id :: Lens.Lens' UpdateRuleGroup Prelude.Text
updateRuleGroup_id = Lens.lens (\UpdateRuleGroup' {id} -> id) (\s@UpdateRuleGroup' {} a -> s {id = a} :: UpdateRuleGroup)

-- | Defines and enables Amazon CloudWatch metrics and web request sample
-- collection.
updateRuleGroup_visibilityConfig :: Lens.Lens' UpdateRuleGroup VisibilityConfig
updateRuleGroup_visibilityConfig = Lens.lens (\UpdateRuleGroup' {visibilityConfig} -> visibilityConfig) (\s@UpdateRuleGroup' {} a -> s {visibilityConfig = a} :: UpdateRuleGroup)

-- | A token used for optimistic locking. WAF returns a token to your @get@
-- and @list@ requests, to mark the state of the entity at the time of the
-- request. To make changes to the entity associated with the token, you
-- provide the token to operations like @update@ and @delete@. WAF uses the
-- token to ensure that no changes have been made to the entity since you
-- last retrieved it. If a change has been made, the update fails with a
-- @WAFOptimisticLockException@. If this happens, perform another @get@,
-- and use the new token returned by that operation.
updateRuleGroup_lockToken :: Lens.Lens' UpdateRuleGroup Prelude.Text
updateRuleGroup_lockToken = Lens.lens (\UpdateRuleGroup' {lockToken} -> lockToken) (\s@UpdateRuleGroup' {} a -> s {lockToken = a} :: UpdateRuleGroup)

instance Core.AWSRequest UpdateRuleGroup where
  type
    AWSResponse UpdateRuleGroup =
      UpdateRuleGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateRuleGroupResponse'
            Prelude.<$> (x Data..?> "NextLockToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateRuleGroup where
  hashWithSalt _salt UpdateRuleGroup' {..} =
    _salt
      `Prelude.hashWithSalt` customResponseBodies
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` rules
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` scope
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` visibilityConfig
      `Prelude.hashWithSalt` lockToken

instance Prelude.NFData UpdateRuleGroup where
  rnf UpdateRuleGroup' {..} =
    Prelude.rnf customResponseBodies
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf rules
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf scope
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf visibilityConfig
      `Prelude.seq` Prelude.rnf lockToken

instance Data.ToHeaders UpdateRuleGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_20190729.UpdateRuleGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateRuleGroup where
  toJSON UpdateRuleGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CustomResponseBodies" Data..=)
              Prelude.<$> customResponseBodies,
            ("Description" Data..=) Prelude.<$> description,
            ("Rules" Data..=) Prelude.<$> rules,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Scope" Data..= scope),
            Prelude.Just ("Id" Data..= id),
            Prelude.Just
              ("VisibilityConfig" Data..= visibilityConfig),
            Prelude.Just ("LockToken" Data..= lockToken)
          ]
      )

instance Data.ToPath UpdateRuleGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateRuleGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRuleGroupResponse' smart constructor.
data UpdateRuleGroupResponse = UpdateRuleGroupResponse'
  { -- | A token used for optimistic locking. WAF returns this token to your
    -- @update@ requests. You use @NextLockToken@ in the same manner as you use
    -- @LockToken@.
    nextLockToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRuleGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextLockToken', 'updateRuleGroupResponse_nextLockToken' - A token used for optimistic locking. WAF returns this token to your
-- @update@ requests. You use @NextLockToken@ in the same manner as you use
-- @LockToken@.
--
-- 'httpStatus', 'updateRuleGroupResponse_httpStatus' - The response's http status code.
newUpdateRuleGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateRuleGroupResponse
newUpdateRuleGroupResponse pHttpStatus_ =
  UpdateRuleGroupResponse'
    { nextLockToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token used for optimistic locking. WAF returns this token to your
-- @update@ requests. You use @NextLockToken@ in the same manner as you use
-- @LockToken@.
updateRuleGroupResponse_nextLockToken :: Lens.Lens' UpdateRuleGroupResponse (Prelude.Maybe Prelude.Text)
updateRuleGroupResponse_nextLockToken = Lens.lens (\UpdateRuleGroupResponse' {nextLockToken} -> nextLockToken) (\s@UpdateRuleGroupResponse' {} a -> s {nextLockToken = a} :: UpdateRuleGroupResponse)

-- | The response's http status code.
updateRuleGroupResponse_httpStatus :: Lens.Lens' UpdateRuleGroupResponse Prelude.Int
updateRuleGroupResponse_httpStatus = Lens.lens (\UpdateRuleGroupResponse' {httpStatus} -> httpStatus) (\s@UpdateRuleGroupResponse' {} a -> s {httpStatus = a} :: UpdateRuleGroupResponse)

instance Prelude.NFData UpdateRuleGroupResponse where
  rnf UpdateRuleGroupResponse' {..} =
    Prelude.rnf nextLockToken
      `Prelude.seq` Prelude.rnf httpStatus
