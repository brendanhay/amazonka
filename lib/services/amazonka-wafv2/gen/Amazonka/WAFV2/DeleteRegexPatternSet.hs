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
-- Module      : Amazonka.WAFV2.DeleteRegexPatternSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified RegexPatternSet.
module Amazonka.WAFV2.DeleteRegexPatternSet
  ( -- * Creating a Request
    DeleteRegexPatternSet (..),
    newDeleteRegexPatternSet,

    -- * Request Lenses
    deleteRegexPatternSet_name,
    deleteRegexPatternSet_scope,
    deleteRegexPatternSet_id,
    deleteRegexPatternSet_lockToken,

    -- * Destructuring the Response
    DeleteRegexPatternSetResponse (..),
    newDeleteRegexPatternSetResponse,

    -- * Response Lenses
    deleteRegexPatternSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFV2.Types

-- | /See:/ 'newDeleteRegexPatternSet' smart constructor.
data DeleteRegexPatternSet = DeleteRegexPatternSet'
  { -- | The name of the set. You cannot change the name after you create the
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
-- Create a value of 'DeleteRegexPatternSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteRegexPatternSet_name' - The name of the set. You cannot change the name after you create the
-- set.
--
-- 'scope', 'deleteRegexPatternSet_scope' - Specifies whether this is for an Amazon CloudFront distribution or for a
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
-- 'id', 'deleteRegexPatternSet_id' - A unique identifier for the set. This ID is returned in the responses to
-- create and list commands. You provide it to operations like update and
-- delete.
--
-- 'lockToken', 'deleteRegexPatternSet_lockToken' - A token used for optimistic locking. WAF returns a token to your @get@
-- and @list@ requests, to mark the state of the entity at the time of the
-- request. To make changes to the entity associated with the token, you
-- provide the token to operations like @update@ and @delete@. WAF uses the
-- token to ensure that no changes have been made to the entity since you
-- last retrieved it. If a change has been made, the update fails with a
-- @WAFOptimisticLockException@. If this happens, perform another @get@,
-- and use the new token returned by that operation.
newDeleteRegexPatternSet ::
  -- | 'name'
  Prelude.Text ->
  -- | 'scope'
  Scope ->
  -- | 'id'
  Prelude.Text ->
  -- | 'lockToken'
  Prelude.Text ->
  DeleteRegexPatternSet
newDeleteRegexPatternSet
  pName_
  pScope_
  pId_
  pLockToken_ =
    DeleteRegexPatternSet'
      { name = pName_,
        scope = pScope_,
        id = pId_,
        lockToken = pLockToken_
      }

-- | The name of the set. You cannot change the name after you create the
-- set.
deleteRegexPatternSet_name :: Lens.Lens' DeleteRegexPatternSet Prelude.Text
deleteRegexPatternSet_name = Lens.lens (\DeleteRegexPatternSet' {name} -> name) (\s@DeleteRegexPatternSet' {} a -> s {name = a} :: DeleteRegexPatternSet)

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
deleteRegexPatternSet_scope :: Lens.Lens' DeleteRegexPatternSet Scope
deleteRegexPatternSet_scope = Lens.lens (\DeleteRegexPatternSet' {scope} -> scope) (\s@DeleteRegexPatternSet' {} a -> s {scope = a} :: DeleteRegexPatternSet)

-- | A unique identifier for the set. This ID is returned in the responses to
-- create and list commands. You provide it to operations like update and
-- delete.
deleteRegexPatternSet_id :: Lens.Lens' DeleteRegexPatternSet Prelude.Text
deleteRegexPatternSet_id = Lens.lens (\DeleteRegexPatternSet' {id} -> id) (\s@DeleteRegexPatternSet' {} a -> s {id = a} :: DeleteRegexPatternSet)

-- | A token used for optimistic locking. WAF returns a token to your @get@
-- and @list@ requests, to mark the state of the entity at the time of the
-- request. To make changes to the entity associated with the token, you
-- provide the token to operations like @update@ and @delete@. WAF uses the
-- token to ensure that no changes have been made to the entity since you
-- last retrieved it. If a change has been made, the update fails with a
-- @WAFOptimisticLockException@. If this happens, perform another @get@,
-- and use the new token returned by that operation.
deleteRegexPatternSet_lockToken :: Lens.Lens' DeleteRegexPatternSet Prelude.Text
deleteRegexPatternSet_lockToken = Lens.lens (\DeleteRegexPatternSet' {lockToken} -> lockToken) (\s@DeleteRegexPatternSet' {} a -> s {lockToken = a} :: DeleteRegexPatternSet)

instance Core.AWSRequest DeleteRegexPatternSet where
  type
    AWSResponse DeleteRegexPatternSet =
      DeleteRegexPatternSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteRegexPatternSetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteRegexPatternSet where
  hashWithSalt _salt DeleteRegexPatternSet' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` scope
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` lockToken

instance Prelude.NFData DeleteRegexPatternSet where
  rnf DeleteRegexPatternSet' {..} =
    Prelude.rnf name `Prelude.seq`
      Prelude.rnf scope `Prelude.seq`
        Prelude.rnf id `Prelude.seq`
          Prelude.rnf lockToken

instance Data.ToHeaders DeleteRegexPatternSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_20190729.DeleteRegexPatternSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteRegexPatternSet where
  toJSON DeleteRegexPatternSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Scope" Data..= scope),
            Prelude.Just ("Id" Data..= id),
            Prelude.Just ("LockToken" Data..= lockToken)
          ]
      )

instance Data.ToPath DeleteRegexPatternSet where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteRegexPatternSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRegexPatternSetResponse' smart constructor.
data DeleteRegexPatternSetResponse = DeleteRegexPatternSetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRegexPatternSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteRegexPatternSetResponse_httpStatus' - The response's http status code.
newDeleteRegexPatternSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteRegexPatternSetResponse
newDeleteRegexPatternSetResponse pHttpStatus_ =
  DeleteRegexPatternSetResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteRegexPatternSetResponse_httpStatus :: Lens.Lens' DeleteRegexPatternSetResponse Prelude.Int
deleteRegexPatternSetResponse_httpStatus = Lens.lens (\DeleteRegexPatternSetResponse' {httpStatus} -> httpStatus) (\s@DeleteRegexPatternSetResponse' {} a -> s {httpStatus = a} :: DeleteRegexPatternSetResponse)

instance Prelude.NFData DeleteRegexPatternSetResponse where
  rnf DeleteRegexPatternSetResponse' {..} =
    Prelude.rnf httpStatus
