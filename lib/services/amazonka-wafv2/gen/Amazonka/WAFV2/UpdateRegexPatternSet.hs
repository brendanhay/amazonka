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
-- Module      : Amazonka.WAFV2.UpdateRegexPatternSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified RegexPatternSet.
--
-- This operation completely replaces the mutable specifications that you
-- already have for the regex pattern set with the ones that you provide to
-- this call. To modify the regex pattern set, retrieve it by calling
-- GetRegexPatternSet, update the settings as needed, and then provide the
-- complete regex pattern set specification to this call.
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
module Amazonka.WAFV2.UpdateRegexPatternSet
  ( -- * Creating a Request
    UpdateRegexPatternSet (..),
    newUpdateRegexPatternSet,

    -- * Request Lenses
    updateRegexPatternSet_description,
    updateRegexPatternSet_name,
    updateRegexPatternSet_scope,
    updateRegexPatternSet_id,
    updateRegexPatternSet_regularExpressionList,
    updateRegexPatternSet_lockToken,

    -- * Destructuring the Response
    UpdateRegexPatternSetResponse (..),
    newUpdateRegexPatternSetResponse,

    -- * Response Lenses
    updateRegexPatternSetResponse_nextLockToken,
    updateRegexPatternSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFV2.Types

-- | /See:/ 'newUpdateRegexPatternSet' smart constructor.
data UpdateRegexPatternSet = UpdateRegexPatternSet'
  { -- | A description of the set that helps with identification.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the set. You cannot change the name after you create the
    -- set.
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
    -- | A unique identifier for the set. This ID is returned in the responses to
    -- create and list commands. You provide it to operations like update and
    -- delete.
    id :: Prelude.Text,
    regularExpressionList :: [Regex],
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
-- Create a value of 'UpdateRegexPatternSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateRegexPatternSet_description' - A description of the set that helps with identification.
--
-- 'name', 'updateRegexPatternSet_name' - The name of the set. You cannot change the name after you create the
-- set.
--
-- 'scope', 'updateRegexPatternSet_scope' - Specifies whether this is for an Amazon CloudFront distribution or for a
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
-- 'id', 'updateRegexPatternSet_id' - A unique identifier for the set. This ID is returned in the responses to
-- create and list commands. You provide it to operations like update and
-- delete.
--
-- 'regularExpressionList', 'updateRegexPatternSet_regularExpressionList' -
--
-- 'lockToken', 'updateRegexPatternSet_lockToken' - A token used for optimistic locking. WAF returns a token to your @get@
-- and @list@ requests, to mark the state of the entity at the time of the
-- request. To make changes to the entity associated with the token, you
-- provide the token to operations like @update@ and @delete@. WAF uses the
-- token to ensure that no changes have been made to the entity since you
-- last retrieved it. If a change has been made, the update fails with a
-- @WAFOptimisticLockException@. If this happens, perform another @get@,
-- and use the new token returned by that operation.
newUpdateRegexPatternSet ::
  -- | 'name'
  Prelude.Text ->
  -- | 'scope'
  Scope ->
  -- | 'id'
  Prelude.Text ->
  -- | 'lockToken'
  Prelude.Text ->
  UpdateRegexPatternSet
newUpdateRegexPatternSet
  pName_
  pScope_
  pId_
  pLockToken_ =
    UpdateRegexPatternSet'
      { description =
          Prelude.Nothing,
        name = pName_,
        scope = pScope_,
        id = pId_,
        regularExpressionList = Prelude.mempty,
        lockToken = pLockToken_
      }

-- | A description of the set that helps with identification.
updateRegexPatternSet_description :: Lens.Lens' UpdateRegexPatternSet (Prelude.Maybe Prelude.Text)
updateRegexPatternSet_description = Lens.lens (\UpdateRegexPatternSet' {description} -> description) (\s@UpdateRegexPatternSet' {} a -> s {description = a} :: UpdateRegexPatternSet)

-- | The name of the set. You cannot change the name after you create the
-- set.
updateRegexPatternSet_name :: Lens.Lens' UpdateRegexPatternSet Prelude.Text
updateRegexPatternSet_name = Lens.lens (\UpdateRegexPatternSet' {name} -> name) (\s@UpdateRegexPatternSet' {} a -> s {name = a} :: UpdateRegexPatternSet)

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
updateRegexPatternSet_scope :: Lens.Lens' UpdateRegexPatternSet Scope
updateRegexPatternSet_scope = Lens.lens (\UpdateRegexPatternSet' {scope} -> scope) (\s@UpdateRegexPatternSet' {} a -> s {scope = a} :: UpdateRegexPatternSet)

-- | A unique identifier for the set. This ID is returned in the responses to
-- create and list commands. You provide it to operations like update and
-- delete.
updateRegexPatternSet_id :: Lens.Lens' UpdateRegexPatternSet Prelude.Text
updateRegexPatternSet_id = Lens.lens (\UpdateRegexPatternSet' {id} -> id) (\s@UpdateRegexPatternSet' {} a -> s {id = a} :: UpdateRegexPatternSet)

updateRegexPatternSet_regularExpressionList :: Lens.Lens' UpdateRegexPatternSet [Regex]
updateRegexPatternSet_regularExpressionList = Lens.lens (\UpdateRegexPatternSet' {regularExpressionList} -> regularExpressionList) (\s@UpdateRegexPatternSet' {} a -> s {regularExpressionList = a} :: UpdateRegexPatternSet) Prelude.. Lens.coerced

-- | A token used for optimistic locking. WAF returns a token to your @get@
-- and @list@ requests, to mark the state of the entity at the time of the
-- request. To make changes to the entity associated with the token, you
-- provide the token to operations like @update@ and @delete@. WAF uses the
-- token to ensure that no changes have been made to the entity since you
-- last retrieved it. If a change has been made, the update fails with a
-- @WAFOptimisticLockException@. If this happens, perform another @get@,
-- and use the new token returned by that operation.
updateRegexPatternSet_lockToken :: Lens.Lens' UpdateRegexPatternSet Prelude.Text
updateRegexPatternSet_lockToken = Lens.lens (\UpdateRegexPatternSet' {lockToken} -> lockToken) (\s@UpdateRegexPatternSet' {} a -> s {lockToken = a} :: UpdateRegexPatternSet)

instance Core.AWSRequest UpdateRegexPatternSet where
  type
    AWSResponse UpdateRegexPatternSet =
      UpdateRegexPatternSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateRegexPatternSetResponse'
            Prelude.<$> (x Data..?> "NextLockToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateRegexPatternSet where
  hashWithSalt _salt UpdateRegexPatternSet' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` scope
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` regularExpressionList
      `Prelude.hashWithSalt` lockToken

instance Prelude.NFData UpdateRegexPatternSet where
  rnf UpdateRegexPatternSet' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf scope
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf regularExpressionList
      `Prelude.seq` Prelude.rnf lockToken

instance Data.ToHeaders UpdateRegexPatternSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_20190729.UpdateRegexPatternSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateRegexPatternSet where
  toJSON UpdateRegexPatternSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Scope" Data..= scope),
            Prelude.Just ("Id" Data..= id),
            Prelude.Just
              ( "RegularExpressionList"
                  Data..= regularExpressionList
              ),
            Prelude.Just ("LockToken" Data..= lockToken)
          ]
      )

instance Data.ToPath UpdateRegexPatternSet where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateRegexPatternSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRegexPatternSetResponse' smart constructor.
data UpdateRegexPatternSetResponse = UpdateRegexPatternSetResponse'
  { -- | A token used for optimistic locking. WAF returns this token to your
    -- @update@ requests. You use @NextLockToken@ in the same manner as you use
    -- @LockToken@.
    nextLockToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRegexPatternSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextLockToken', 'updateRegexPatternSetResponse_nextLockToken' - A token used for optimistic locking. WAF returns this token to your
-- @update@ requests. You use @NextLockToken@ in the same manner as you use
-- @LockToken@.
--
-- 'httpStatus', 'updateRegexPatternSetResponse_httpStatus' - The response's http status code.
newUpdateRegexPatternSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateRegexPatternSetResponse
newUpdateRegexPatternSetResponse pHttpStatus_ =
  UpdateRegexPatternSetResponse'
    { nextLockToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token used for optimistic locking. WAF returns this token to your
-- @update@ requests. You use @NextLockToken@ in the same manner as you use
-- @LockToken@.
updateRegexPatternSetResponse_nextLockToken :: Lens.Lens' UpdateRegexPatternSetResponse (Prelude.Maybe Prelude.Text)
updateRegexPatternSetResponse_nextLockToken = Lens.lens (\UpdateRegexPatternSetResponse' {nextLockToken} -> nextLockToken) (\s@UpdateRegexPatternSetResponse' {} a -> s {nextLockToken = a} :: UpdateRegexPatternSetResponse)

-- | The response's http status code.
updateRegexPatternSetResponse_httpStatus :: Lens.Lens' UpdateRegexPatternSetResponse Prelude.Int
updateRegexPatternSetResponse_httpStatus = Lens.lens (\UpdateRegexPatternSetResponse' {httpStatus} -> httpStatus) (\s@UpdateRegexPatternSetResponse' {} a -> s {httpStatus = a} :: UpdateRegexPatternSetResponse)

instance Prelude.NFData UpdateRegexPatternSetResponse where
  rnf UpdateRegexPatternSetResponse' {..} =
    Prelude.rnf nextLockToken
      `Prelude.seq` Prelude.rnf httpStatus
