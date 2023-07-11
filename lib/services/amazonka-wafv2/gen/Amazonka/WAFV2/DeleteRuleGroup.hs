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
-- Module      : Amazonka.WAFV2.DeleteRuleGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified RuleGroup.
module Amazonka.WAFV2.DeleteRuleGroup
  ( -- * Creating a Request
    DeleteRuleGroup (..),
    newDeleteRuleGroup,

    -- * Request Lenses
    deleteRuleGroup_name,
    deleteRuleGroup_scope,
    deleteRuleGroup_id,
    deleteRuleGroup_lockToken,

    -- * Destructuring the Response
    DeleteRuleGroupResponse (..),
    newDeleteRuleGroupResponse,

    -- * Response Lenses
    deleteRuleGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFV2.Types

-- | /See:/ 'newDeleteRuleGroup' smart constructor.
data DeleteRuleGroup = DeleteRuleGroup'
  { -- | The name of the rule group. You cannot change the name of a rule group
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
-- Create a value of 'DeleteRuleGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteRuleGroup_name' - The name of the rule group. You cannot change the name of a rule group
-- after you create it.
--
-- 'scope', 'deleteRuleGroup_scope' - Specifies whether this is for an Amazon CloudFront distribution or for a
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
-- 'id', 'deleteRuleGroup_id' - A unique identifier for the rule group. This ID is returned in the
-- responses to create and list commands. You provide it to operations like
-- update and delete.
--
-- 'lockToken', 'deleteRuleGroup_lockToken' - A token used for optimistic locking. WAF returns a token to your @get@
-- and @list@ requests, to mark the state of the entity at the time of the
-- request. To make changes to the entity associated with the token, you
-- provide the token to operations like @update@ and @delete@. WAF uses the
-- token to ensure that no changes have been made to the entity since you
-- last retrieved it. If a change has been made, the update fails with a
-- @WAFOptimisticLockException@. If this happens, perform another @get@,
-- and use the new token returned by that operation.
newDeleteRuleGroup ::
  -- | 'name'
  Prelude.Text ->
  -- | 'scope'
  Scope ->
  -- | 'id'
  Prelude.Text ->
  -- | 'lockToken'
  Prelude.Text ->
  DeleteRuleGroup
newDeleteRuleGroup pName_ pScope_ pId_ pLockToken_ =
  DeleteRuleGroup'
    { name = pName_,
      scope = pScope_,
      id = pId_,
      lockToken = pLockToken_
    }

-- | The name of the rule group. You cannot change the name of a rule group
-- after you create it.
deleteRuleGroup_name :: Lens.Lens' DeleteRuleGroup Prelude.Text
deleteRuleGroup_name = Lens.lens (\DeleteRuleGroup' {name} -> name) (\s@DeleteRuleGroup' {} a -> s {name = a} :: DeleteRuleGroup)

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
deleteRuleGroup_scope :: Lens.Lens' DeleteRuleGroup Scope
deleteRuleGroup_scope = Lens.lens (\DeleteRuleGroup' {scope} -> scope) (\s@DeleteRuleGroup' {} a -> s {scope = a} :: DeleteRuleGroup)

-- | A unique identifier for the rule group. This ID is returned in the
-- responses to create and list commands. You provide it to operations like
-- update and delete.
deleteRuleGroup_id :: Lens.Lens' DeleteRuleGroup Prelude.Text
deleteRuleGroup_id = Lens.lens (\DeleteRuleGroup' {id} -> id) (\s@DeleteRuleGroup' {} a -> s {id = a} :: DeleteRuleGroup)

-- | A token used for optimistic locking. WAF returns a token to your @get@
-- and @list@ requests, to mark the state of the entity at the time of the
-- request. To make changes to the entity associated with the token, you
-- provide the token to operations like @update@ and @delete@. WAF uses the
-- token to ensure that no changes have been made to the entity since you
-- last retrieved it. If a change has been made, the update fails with a
-- @WAFOptimisticLockException@. If this happens, perform another @get@,
-- and use the new token returned by that operation.
deleteRuleGroup_lockToken :: Lens.Lens' DeleteRuleGroup Prelude.Text
deleteRuleGroup_lockToken = Lens.lens (\DeleteRuleGroup' {lockToken} -> lockToken) (\s@DeleteRuleGroup' {} a -> s {lockToken = a} :: DeleteRuleGroup)

instance Core.AWSRequest DeleteRuleGroup where
  type
    AWSResponse DeleteRuleGroup =
      DeleteRuleGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteRuleGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteRuleGroup where
  hashWithSalt _salt DeleteRuleGroup' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` scope
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` lockToken

instance Prelude.NFData DeleteRuleGroup where
  rnf DeleteRuleGroup' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf scope
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lockToken

instance Data.ToHeaders DeleteRuleGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_20190729.DeleteRuleGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteRuleGroup where
  toJSON DeleteRuleGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Scope" Data..= scope),
            Prelude.Just ("Id" Data..= id),
            Prelude.Just ("LockToken" Data..= lockToken)
          ]
      )

instance Data.ToPath DeleteRuleGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteRuleGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRuleGroupResponse' smart constructor.
data DeleteRuleGroupResponse = DeleteRuleGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRuleGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteRuleGroupResponse_httpStatus' - The response's http status code.
newDeleteRuleGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteRuleGroupResponse
newDeleteRuleGroupResponse pHttpStatus_ =
  DeleteRuleGroupResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteRuleGroupResponse_httpStatus :: Lens.Lens' DeleteRuleGroupResponse Prelude.Int
deleteRuleGroupResponse_httpStatus = Lens.lens (\DeleteRuleGroupResponse' {httpStatus} -> httpStatus) (\s@DeleteRuleGroupResponse' {} a -> s {httpStatus = a} :: DeleteRuleGroupResponse)

instance Prelude.NFData DeleteRuleGroupResponse where
  rnf DeleteRuleGroupResponse' {..} =
    Prelude.rnf httpStatus
