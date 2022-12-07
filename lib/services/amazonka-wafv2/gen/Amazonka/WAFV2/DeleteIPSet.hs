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
-- Module      : Amazonka.WAFV2.DeleteIPSet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified IPSet.
module Amazonka.WAFV2.DeleteIPSet
  ( -- * Creating a Request
    DeleteIPSet (..),
    newDeleteIPSet,

    -- * Request Lenses
    deleteIPSet_name,
    deleteIPSet_scope,
    deleteIPSet_id,
    deleteIPSet_lockToken,

    -- * Destructuring the Response
    DeleteIPSetResponse (..),
    newDeleteIPSetResponse,

    -- * Response Lenses
    deleteIPSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFV2.Types

-- | /See:/ 'newDeleteIPSet' smart constructor.
data DeleteIPSet = DeleteIPSet'
  { -- | The name of the IP set. You cannot change the name of an @IPSet@ after
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
-- Create a value of 'DeleteIPSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteIPSet_name' - The name of the IP set. You cannot change the name of an @IPSet@ after
-- you create it.
--
-- 'scope', 'deleteIPSet_scope' - Specifies whether this is for an Amazon CloudFront distribution or for a
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
-- 'id', 'deleteIPSet_id' - A unique identifier for the set. This ID is returned in the responses to
-- create and list commands. You provide it to operations like update and
-- delete.
--
-- 'lockToken', 'deleteIPSet_lockToken' - A token used for optimistic locking. WAF returns a token to your @get@
-- and @list@ requests, to mark the state of the entity at the time of the
-- request. To make changes to the entity associated with the token, you
-- provide the token to operations like @update@ and @delete@. WAF uses the
-- token to ensure that no changes have been made to the entity since you
-- last retrieved it. If a change has been made, the update fails with a
-- @WAFOptimisticLockException@. If this happens, perform another @get@,
-- and use the new token returned by that operation.
newDeleteIPSet ::
  -- | 'name'
  Prelude.Text ->
  -- | 'scope'
  Scope ->
  -- | 'id'
  Prelude.Text ->
  -- | 'lockToken'
  Prelude.Text ->
  DeleteIPSet
newDeleteIPSet pName_ pScope_ pId_ pLockToken_ =
  DeleteIPSet'
    { name = pName_,
      scope = pScope_,
      id = pId_,
      lockToken = pLockToken_
    }

-- | The name of the IP set. You cannot change the name of an @IPSet@ after
-- you create it.
deleteIPSet_name :: Lens.Lens' DeleteIPSet Prelude.Text
deleteIPSet_name = Lens.lens (\DeleteIPSet' {name} -> name) (\s@DeleteIPSet' {} a -> s {name = a} :: DeleteIPSet)

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
deleteIPSet_scope :: Lens.Lens' DeleteIPSet Scope
deleteIPSet_scope = Lens.lens (\DeleteIPSet' {scope} -> scope) (\s@DeleteIPSet' {} a -> s {scope = a} :: DeleteIPSet)

-- | A unique identifier for the set. This ID is returned in the responses to
-- create and list commands. You provide it to operations like update and
-- delete.
deleteIPSet_id :: Lens.Lens' DeleteIPSet Prelude.Text
deleteIPSet_id = Lens.lens (\DeleteIPSet' {id} -> id) (\s@DeleteIPSet' {} a -> s {id = a} :: DeleteIPSet)

-- | A token used for optimistic locking. WAF returns a token to your @get@
-- and @list@ requests, to mark the state of the entity at the time of the
-- request. To make changes to the entity associated with the token, you
-- provide the token to operations like @update@ and @delete@. WAF uses the
-- token to ensure that no changes have been made to the entity since you
-- last retrieved it. If a change has been made, the update fails with a
-- @WAFOptimisticLockException@. If this happens, perform another @get@,
-- and use the new token returned by that operation.
deleteIPSet_lockToken :: Lens.Lens' DeleteIPSet Prelude.Text
deleteIPSet_lockToken = Lens.lens (\DeleteIPSet' {lockToken} -> lockToken) (\s@DeleteIPSet' {} a -> s {lockToken = a} :: DeleteIPSet)

instance Core.AWSRequest DeleteIPSet where
  type AWSResponse DeleteIPSet = DeleteIPSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteIPSetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteIPSet where
  hashWithSalt _salt DeleteIPSet' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` scope
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` lockToken

instance Prelude.NFData DeleteIPSet where
  rnf DeleteIPSet' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf scope
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lockToken

instance Data.ToHeaders DeleteIPSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_20190729.DeleteIPSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteIPSet where
  toJSON DeleteIPSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Scope" Data..= scope),
            Prelude.Just ("Id" Data..= id),
            Prelude.Just ("LockToken" Data..= lockToken)
          ]
      )

instance Data.ToPath DeleteIPSet where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteIPSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteIPSetResponse' smart constructor.
data DeleteIPSetResponse = DeleteIPSetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteIPSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteIPSetResponse_httpStatus' - The response's http status code.
newDeleteIPSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteIPSetResponse
newDeleteIPSetResponse pHttpStatus_ =
  DeleteIPSetResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteIPSetResponse_httpStatus :: Lens.Lens' DeleteIPSetResponse Prelude.Int
deleteIPSetResponse_httpStatus = Lens.lens (\DeleteIPSetResponse' {httpStatus} -> httpStatus) (\s@DeleteIPSetResponse' {} a -> s {httpStatus = a} :: DeleteIPSetResponse)

instance Prelude.NFData DeleteIPSetResponse where
  rnf DeleteIPSetResponse' {..} = Prelude.rnf httpStatus
