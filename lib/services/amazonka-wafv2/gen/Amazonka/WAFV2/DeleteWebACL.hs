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
-- Module      : Amazonka.WAFV2.DeleteWebACL
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified WebACL.
--
-- You can only use this if @ManagedByFirewallManager@ is false in the
-- specified WebACL.
--
-- Before deleting any web ACL, first disassociate it from all resources.
--
-- -   To retrieve a list of the resources that are associated with a web
--     ACL, use the following calls:
--
--     -   For regional resources, call ListResourcesForWebACL.
--
--     -   For Amazon CloudFront distributions, use the CloudFront call
--         @ListDistributionsByWebACLId@. For information, see
--         <https://docs.aws.amazon.com/cloudfront/latest/APIReference/API_ListDistributionsByWebACLId.html ListDistributionsByWebACLId>.
--
-- -   To disassociate a resource from a web ACL, use the following calls:
--
--     -   For regional resources, call DisassociateWebACL.
--
--     -   For Amazon CloudFront distributions, provide an empty web ACL ID
--         in the CloudFront call @UpdateDistribution@. For information,
--         see
--         <https://docs.aws.amazon.com/cloudfront/latest/APIReference/API_UpdateDistribution.html UpdateDistribution>.
module Amazonka.WAFV2.DeleteWebACL
  ( -- * Creating a Request
    DeleteWebACL (..),
    newDeleteWebACL,

    -- * Request Lenses
    deleteWebACL_name,
    deleteWebACL_scope,
    deleteWebACL_id,
    deleteWebACL_lockToken,

    -- * Destructuring the Response
    DeleteWebACLResponse (..),
    newDeleteWebACLResponse,

    -- * Response Lenses
    deleteWebACLResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFV2.Types

-- | /See:/ 'newDeleteWebACL' smart constructor.
data DeleteWebACL = DeleteWebACL'
  { -- | The name of the web ACL. You cannot change the name of a web ACL after
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
-- Create a value of 'DeleteWebACL' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteWebACL_name' - The name of the web ACL. You cannot change the name of a web ACL after
-- you create it.
--
-- 'scope', 'deleteWebACL_scope' - Specifies whether this is for an Amazon CloudFront distribution or for a
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
-- 'id', 'deleteWebACL_id' - The unique identifier for the web ACL. This ID is returned in the
-- responses to create and list commands. You provide it to operations like
-- update and delete.
--
-- 'lockToken', 'deleteWebACL_lockToken' - A token used for optimistic locking. WAF returns a token to your @get@
-- and @list@ requests, to mark the state of the entity at the time of the
-- request. To make changes to the entity associated with the token, you
-- provide the token to operations like @update@ and @delete@. WAF uses the
-- token to ensure that no changes have been made to the entity since you
-- last retrieved it. If a change has been made, the update fails with a
-- @WAFOptimisticLockException@. If this happens, perform another @get@,
-- and use the new token returned by that operation.
newDeleteWebACL ::
  -- | 'name'
  Prelude.Text ->
  -- | 'scope'
  Scope ->
  -- | 'id'
  Prelude.Text ->
  -- | 'lockToken'
  Prelude.Text ->
  DeleteWebACL
newDeleteWebACL pName_ pScope_ pId_ pLockToken_ =
  DeleteWebACL'
    { name = pName_,
      scope = pScope_,
      id = pId_,
      lockToken = pLockToken_
    }

-- | The name of the web ACL. You cannot change the name of a web ACL after
-- you create it.
deleteWebACL_name :: Lens.Lens' DeleteWebACL Prelude.Text
deleteWebACL_name = Lens.lens (\DeleteWebACL' {name} -> name) (\s@DeleteWebACL' {} a -> s {name = a} :: DeleteWebACL)

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
deleteWebACL_scope :: Lens.Lens' DeleteWebACL Scope
deleteWebACL_scope = Lens.lens (\DeleteWebACL' {scope} -> scope) (\s@DeleteWebACL' {} a -> s {scope = a} :: DeleteWebACL)

-- | The unique identifier for the web ACL. This ID is returned in the
-- responses to create and list commands. You provide it to operations like
-- update and delete.
deleteWebACL_id :: Lens.Lens' DeleteWebACL Prelude.Text
deleteWebACL_id = Lens.lens (\DeleteWebACL' {id} -> id) (\s@DeleteWebACL' {} a -> s {id = a} :: DeleteWebACL)

-- | A token used for optimistic locking. WAF returns a token to your @get@
-- and @list@ requests, to mark the state of the entity at the time of the
-- request. To make changes to the entity associated with the token, you
-- provide the token to operations like @update@ and @delete@. WAF uses the
-- token to ensure that no changes have been made to the entity since you
-- last retrieved it. If a change has been made, the update fails with a
-- @WAFOptimisticLockException@. If this happens, perform another @get@,
-- and use the new token returned by that operation.
deleteWebACL_lockToken :: Lens.Lens' DeleteWebACL Prelude.Text
deleteWebACL_lockToken = Lens.lens (\DeleteWebACL' {lockToken} -> lockToken) (\s@DeleteWebACL' {} a -> s {lockToken = a} :: DeleteWebACL)

instance Core.AWSRequest DeleteWebACL where
  type AWSResponse DeleteWebACL = DeleteWebACLResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteWebACLResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteWebACL where
  hashWithSalt _salt DeleteWebACL' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` scope
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` lockToken

instance Prelude.NFData DeleteWebACL where
  rnf DeleteWebACL' {..} =
    Prelude.rnf name `Prelude.seq`
      Prelude.rnf scope `Prelude.seq`
        Prelude.rnf id `Prelude.seq`
          Prelude.rnf lockToken

instance Data.ToHeaders DeleteWebACL where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_20190729.DeleteWebACL" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteWebACL where
  toJSON DeleteWebACL' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Scope" Data..= scope),
            Prelude.Just ("Id" Data..= id),
            Prelude.Just ("LockToken" Data..= lockToken)
          ]
      )

instance Data.ToPath DeleteWebACL where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteWebACL where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteWebACLResponse' smart constructor.
data DeleteWebACLResponse = DeleteWebACLResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteWebACLResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteWebACLResponse_httpStatus' - The response's http status code.
newDeleteWebACLResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteWebACLResponse
newDeleteWebACLResponse pHttpStatus_ =
  DeleteWebACLResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteWebACLResponse_httpStatus :: Lens.Lens' DeleteWebACLResponse Prelude.Int
deleteWebACLResponse_httpStatus = Lens.lens (\DeleteWebACLResponse' {httpStatus} -> httpStatus) (\s@DeleteWebACLResponse' {} a -> s {httpStatus = a} :: DeleteWebACLResponse)

instance Prelude.NFData DeleteWebACLResponse where
  rnf DeleteWebACLResponse' {..} =
    Prelude.rnf httpStatus
