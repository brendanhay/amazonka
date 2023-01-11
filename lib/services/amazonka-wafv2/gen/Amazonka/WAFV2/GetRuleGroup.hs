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
-- Module      : Amazonka.WAFV2.GetRuleGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the specified RuleGroup.
module Amazonka.WAFV2.GetRuleGroup
  ( -- * Creating a Request
    GetRuleGroup (..),
    newGetRuleGroup,

    -- * Request Lenses
    getRuleGroup_arn,
    getRuleGroup_id,
    getRuleGroup_name,
    getRuleGroup_scope,

    -- * Destructuring the Response
    GetRuleGroupResponse (..),
    newGetRuleGroupResponse,

    -- * Response Lenses
    getRuleGroupResponse_lockToken,
    getRuleGroupResponse_ruleGroup,
    getRuleGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFV2.Types

-- | /See:/ 'newGetRuleGroup' smart constructor.
data GetRuleGroup = GetRuleGroup'
  { -- | The Amazon Resource Name (ARN) of the entity.
    arn :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the rule group. This ID is returned in the
    -- responses to create and list commands. You provide it to operations like
    -- update and delete.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the rule group. You cannot change the name of a rule group
    -- after you create it.
    name :: Prelude.Maybe Prelude.Text,
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
    scope :: Prelude.Maybe Scope
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRuleGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getRuleGroup_arn' - The Amazon Resource Name (ARN) of the entity.
--
-- 'id', 'getRuleGroup_id' - A unique identifier for the rule group. This ID is returned in the
-- responses to create and list commands. You provide it to operations like
-- update and delete.
--
-- 'name', 'getRuleGroup_name' - The name of the rule group. You cannot change the name of a rule group
-- after you create it.
--
-- 'scope', 'getRuleGroup_scope' - Specifies whether this is for an Amazon CloudFront distribution or for a
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
newGetRuleGroup ::
  GetRuleGroup
newGetRuleGroup =
  GetRuleGroup'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      scope = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the entity.
getRuleGroup_arn :: Lens.Lens' GetRuleGroup (Prelude.Maybe Prelude.Text)
getRuleGroup_arn = Lens.lens (\GetRuleGroup' {arn} -> arn) (\s@GetRuleGroup' {} a -> s {arn = a} :: GetRuleGroup)

-- | A unique identifier for the rule group. This ID is returned in the
-- responses to create and list commands. You provide it to operations like
-- update and delete.
getRuleGroup_id :: Lens.Lens' GetRuleGroup (Prelude.Maybe Prelude.Text)
getRuleGroup_id = Lens.lens (\GetRuleGroup' {id} -> id) (\s@GetRuleGroup' {} a -> s {id = a} :: GetRuleGroup)

-- | The name of the rule group. You cannot change the name of a rule group
-- after you create it.
getRuleGroup_name :: Lens.Lens' GetRuleGroup (Prelude.Maybe Prelude.Text)
getRuleGroup_name = Lens.lens (\GetRuleGroup' {name} -> name) (\s@GetRuleGroup' {} a -> s {name = a} :: GetRuleGroup)

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
getRuleGroup_scope :: Lens.Lens' GetRuleGroup (Prelude.Maybe Scope)
getRuleGroup_scope = Lens.lens (\GetRuleGroup' {scope} -> scope) (\s@GetRuleGroup' {} a -> s {scope = a} :: GetRuleGroup)

instance Core.AWSRequest GetRuleGroup where
  type AWSResponse GetRuleGroup = GetRuleGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRuleGroupResponse'
            Prelude.<$> (x Data..?> "LockToken")
            Prelude.<*> (x Data..?> "RuleGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRuleGroup where
  hashWithSalt _salt GetRuleGroup' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` scope

instance Prelude.NFData GetRuleGroup where
  rnf GetRuleGroup' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf scope

instance Data.ToHeaders GetRuleGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_20190729.GetRuleGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetRuleGroup where
  toJSON GetRuleGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ARN" Data..=) Prelude.<$> arn,
            ("Id" Data..=) Prelude.<$> id,
            ("Name" Data..=) Prelude.<$> name,
            ("Scope" Data..=) Prelude.<$> scope
          ]
      )

instance Data.ToPath GetRuleGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery GetRuleGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRuleGroupResponse' smart constructor.
data GetRuleGroupResponse = GetRuleGroupResponse'
  { -- | A token used for optimistic locking. WAF returns a token to your @get@
    -- and @list@ requests, to mark the state of the entity at the time of the
    -- request. To make changes to the entity associated with the token, you
    -- provide the token to operations like @update@ and @delete@. WAF uses the
    -- token to ensure that no changes have been made to the entity since you
    -- last retrieved it. If a change has been made, the update fails with a
    -- @WAFOptimisticLockException@. If this happens, perform another @get@,
    -- and use the new token returned by that operation.
    lockToken :: Prelude.Maybe Prelude.Text,
    ruleGroup :: Prelude.Maybe RuleGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRuleGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lockToken', 'getRuleGroupResponse_lockToken' - A token used for optimistic locking. WAF returns a token to your @get@
-- and @list@ requests, to mark the state of the entity at the time of the
-- request. To make changes to the entity associated with the token, you
-- provide the token to operations like @update@ and @delete@. WAF uses the
-- token to ensure that no changes have been made to the entity since you
-- last retrieved it. If a change has been made, the update fails with a
-- @WAFOptimisticLockException@. If this happens, perform another @get@,
-- and use the new token returned by that operation.
--
-- 'ruleGroup', 'getRuleGroupResponse_ruleGroup' -
--
-- 'httpStatus', 'getRuleGroupResponse_httpStatus' - The response's http status code.
newGetRuleGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRuleGroupResponse
newGetRuleGroupResponse pHttpStatus_ =
  GetRuleGroupResponse'
    { lockToken = Prelude.Nothing,
      ruleGroup = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token used for optimistic locking. WAF returns a token to your @get@
-- and @list@ requests, to mark the state of the entity at the time of the
-- request. To make changes to the entity associated with the token, you
-- provide the token to operations like @update@ and @delete@. WAF uses the
-- token to ensure that no changes have been made to the entity since you
-- last retrieved it. If a change has been made, the update fails with a
-- @WAFOptimisticLockException@. If this happens, perform another @get@,
-- and use the new token returned by that operation.
getRuleGroupResponse_lockToken :: Lens.Lens' GetRuleGroupResponse (Prelude.Maybe Prelude.Text)
getRuleGroupResponse_lockToken = Lens.lens (\GetRuleGroupResponse' {lockToken} -> lockToken) (\s@GetRuleGroupResponse' {} a -> s {lockToken = a} :: GetRuleGroupResponse)

-- |
getRuleGroupResponse_ruleGroup :: Lens.Lens' GetRuleGroupResponse (Prelude.Maybe RuleGroup)
getRuleGroupResponse_ruleGroup = Lens.lens (\GetRuleGroupResponse' {ruleGroup} -> ruleGroup) (\s@GetRuleGroupResponse' {} a -> s {ruleGroup = a} :: GetRuleGroupResponse)

-- | The response's http status code.
getRuleGroupResponse_httpStatus :: Lens.Lens' GetRuleGroupResponse Prelude.Int
getRuleGroupResponse_httpStatus = Lens.lens (\GetRuleGroupResponse' {httpStatus} -> httpStatus) (\s@GetRuleGroupResponse' {} a -> s {httpStatus = a} :: GetRuleGroupResponse)

instance Prelude.NFData GetRuleGroupResponse where
  rnf GetRuleGroupResponse' {..} =
    Prelude.rnf lockToken
      `Prelude.seq` Prelude.rnf ruleGroup
      `Prelude.seq` Prelude.rnf httpStatus
