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
-- Module      : Amazonka.WAFV2.GetIPSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the specified IPSet.
module Amazonka.WAFV2.GetIPSet
  ( -- * Creating a Request
    GetIPSet (..),
    newGetIPSet,

    -- * Request Lenses
    getIPSet_name,
    getIPSet_scope,
    getIPSet_id,

    -- * Destructuring the Response
    GetIPSetResponse (..),
    newGetIPSetResponse,

    -- * Response Lenses
    getIPSetResponse_iPSet,
    getIPSetResponse_lockToken,
    getIPSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFV2.Types

-- | /See:/ 'newGetIPSet' smart constructor.
data GetIPSet = GetIPSet'
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
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetIPSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getIPSet_name' - The name of the IP set. You cannot change the name of an @IPSet@ after
-- you create it.
--
-- 'scope', 'getIPSet_scope' - Specifies whether this is for an Amazon CloudFront distribution or for a
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
-- 'id', 'getIPSet_id' - A unique identifier for the set. This ID is returned in the responses to
-- create and list commands. You provide it to operations like update and
-- delete.
newGetIPSet ::
  -- | 'name'
  Prelude.Text ->
  -- | 'scope'
  Scope ->
  -- | 'id'
  Prelude.Text ->
  GetIPSet
newGetIPSet pName_ pScope_ pId_ =
  GetIPSet'
    { name = pName_,
      scope = pScope_,
      id = pId_
    }

-- | The name of the IP set. You cannot change the name of an @IPSet@ after
-- you create it.
getIPSet_name :: Lens.Lens' GetIPSet Prelude.Text
getIPSet_name = Lens.lens (\GetIPSet' {name} -> name) (\s@GetIPSet' {} a -> s {name = a} :: GetIPSet)

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
getIPSet_scope :: Lens.Lens' GetIPSet Scope
getIPSet_scope = Lens.lens (\GetIPSet' {scope} -> scope) (\s@GetIPSet' {} a -> s {scope = a} :: GetIPSet)

-- | A unique identifier for the set. This ID is returned in the responses to
-- create and list commands. You provide it to operations like update and
-- delete.
getIPSet_id :: Lens.Lens' GetIPSet Prelude.Text
getIPSet_id = Lens.lens (\GetIPSet' {id} -> id) (\s@GetIPSet' {} a -> s {id = a} :: GetIPSet)

instance Core.AWSRequest GetIPSet where
  type AWSResponse GetIPSet = GetIPSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetIPSetResponse'
            Prelude.<$> (x Data..?> "IPSet")
            Prelude.<*> (x Data..?> "LockToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetIPSet where
  hashWithSalt _salt GetIPSet' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` scope
      `Prelude.hashWithSalt` id

instance Prelude.NFData GetIPSet where
  rnf GetIPSet' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf scope
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders GetIPSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSWAF_20190729.GetIPSet" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetIPSet where
  toJSON GetIPSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Scope" Data..= scope),
            Prelude.Just ("Id" Data..= id)
          ]
      )

instance Data.ToPath GetIPSet where
  toPath = Prelude.const "/"

instance Data.ToQuery GetIPSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetIPSetResponse' smart constructor.
data GetIPSetResponse = GetIPSetResponse'
  { iPSet :: Prelude.Maybe IPSet,
    -- | A token used for optimistic locking. WAF returns a token to your @get@
    -- and @list@ requests, to mark the state of the entity at the time of the
    -- request. To make changes to the entity associated with the token, you
    -- provide the token to operations like @update@ and @delete@. WAF uses the
    -- token to ensure that no changes have been made to the entity since you
    -- last retrieved it. If a change has been made, the update fails with a
    -- @WAFOptimisticLockException@. If this happens, perform another @get@,
    -- and use the new token returned by that operation.
    lockToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetIPSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iPSet', 'getIPSetResponse_iPSet' -
--
-- 'lockToken', 'getIPSetResponse_lockToken' - A token used for optimistic locking. WAF returns a token to your @get@
-- and @list@ requests, to mark the state of the entity at the time of the
-- request. To make changes to the entity associated with the token, you
-- provide the token to operations like @update@ and @delete@. WAF uses the
-- token to ensure that no changes have been made to the entity since you
-- last retrieved it. If a change has been made, the update fails with a
-- @WAFOptimisticLockException@. If this happens, perform another @get@,
-- and use the new token returned by that operation.
--
-- 'httpStatus', 'getIPSetResponse_httpStatus' - The response's http status code.
newGetIPSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetIPSetResponse
newGetIPSetResponse pHttpStatus_ =
  GetIPSetResponse'
    { iPSet = Prelude.Nothing,
      lockToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

getIPSetResponse_iPSet :: Lens.Lens' GetIPSetResponse (Prelude.Maybe IPSet)
getIPSetResponse_iPSet = Lens.lens (\GetIPSetResponse' {iPSet} -> iPSet) (\s@GetIPSetResponse' {} a -> s {iPSet = a} :: GetIPSetResponse)

-- | A token used for optimistic locking. WAF returns a token to your @get@
-- and @list@ requests, to mark the state of the entity at the time of the
-- request. To make changes to the entity associated with the token, you
-- provide the token to operations like @update@ and @delete@. WAF uses the
-- token to ensure that no changes have been made to the entity since you
-- last retrieved it. If a change has been made, the update fails with a
-- @WAFOptimisticLockException@. If this happens, perform another @get@,
-- and use the new token returned by that operation.
getIPSetResponse_lockToken :: Lens.Lens' GetIPSetResponse (Prelude.Maybe Prelude.Text)
getIPSetResponse_lockToken = Lens.lens (\GetIPSetResponse' {lockToken} -> lockToken) (\s@GetIPSetResponse' {} a -> s {lockToken = a} :: GetIPSetResponse)

-- | The response's http status code.
getIPSetResponse_httpStatus :: Lens.Lens' GetIPSetResponse Prelude.Int
getIPSetResponse_httpStatus = Lens.lens (\GetIPSetResponse' {httpStatus} -> httpStatus) (\s@GetIPSetResponse' {} a -> s {httpStatus = a} :: GetIPSetResponse)

instance Prelude.NFData GetIPSetResponse where
  rnf GetIPSetResponse' {..} =
    Prelude.rnf iPSet
      `Prelude.seq` Prelude.rnf lockToken
      `Prelude.seq` Prelude.rnf httpStatus
