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
-- Module      : Amazonka.WAFV2.GetRegexPatternSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the specified RegexPatternSet.
module Amazonka.WAFV2.GetRegexPatternSet
  ( -- * Creating a Request
    GetRegexPatternSet (..),
    newGetRegexPatternSet,

    -- * Request Lenses
    getRegexPatternSet_name,
    getRegexPatternSet_scope,
    getRegexPatternSet_id,

    -- * Destructuring the Response
    GetRegexPatternSetResponse (..),
    newGetRegexPatternSetResponse,

    -- * Response Lenses
    getRegexPatternSetResponse_lockToken,
    getRegexPatternSetResponse_regexPatternSet,
    getRegexPatternSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFV2.Types

-- | /See:/ 'newGetRegexPatternSet' smart constructor.
data GetRegexPatternSet = GetRegexPatternSet'
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
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRegexPatternSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getRegexPatternSet_name' - The name of the set. You cannot change the name after you create the
-- set.
--
-- 'scope', 'getRegexPatternSet_scope' - Specifies whether this is for an Amazon CloudFront distribution or for a
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
-- 'id', 'getRegexPatternSet_id' - A unique identifier for the set. This ID is returned in the responses to
-- create and list commands. You provide it to operations like update and
-- delete.
newGetRegexPatternSet ::
  -- | 'name'
  Prelude.Text ->
  -- | 'scope'
  Scope ->
  -- | 'id'
  Prelude.Text ->
  GetRegexPatternSet
newGetRegexPatternSet pName_ pScope_ pId_ =
  GetRegexPatternSet'
    { name = pName_,
      scope = pScope_,
      id = pId_
    }

-- | The name of the set. You cannot change the name after you create the
-- set.
getRegexPatternSet_name :: Lens.Lens' GetRegexPatternSet Prelude.Text
getRegexPatternSet_name = Lens.lens (\GetRegexPatternSet' {name} -> name) (\s@GetRegexPatternSet' {} a -> s {name = a} :: GetRegexPatternSet)

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
getRegexPatternSet_scope :: Lens.Lens' GetRegexPatternSet Scope
getRegexPatternSet_scope = Lens.lens (\GetRegexPatternSet' {scope} -> scope) (\s@GetRegexPatternSet' {} a -> s {scope = a} :: GetRegexPatternSet)

-- | A unique identifier for the set. This ID is returned in the responses to
-- create and list commands. You provide it to operations like update and
-- delete.
getRegexPatternSet_id :: Lens.Lens' GetRegexPatternSet Prelude.Text
getRegexPatternSet_id = Lens.lens (\GetRegexPatternSet' {id} -> id) (\s@GetRegexPatternSet' {} a -> s {id = a} :: GetRegexPatternSet)

instance Core.AWSRequest GetRegexPatternSet where
  type
    AWSResponse GetRegexPatternSet =
      GetRegexPatternSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRegexPatternSetResponse'
            Prelude.<$> (x Data..?> "LockToken")
            Prelude.<*> (x Data..?> "RegexPatternSet")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRegexPatternSet where
  hashWithSalt _salt GetRegexPatternSet' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` scope
      `Prelude.hashWithSalt` id

instance Prelude.NFData GetRegexPatternSet where
  rnf GetRegexPatternSet' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf scope
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders GetRegexPatternSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_20190729.GetRegexPatternSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetRegexPatternSet where
  toJSON GetRegexPatternSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Scope" Data..= scope),
            Prelude.Just ("Id" Data..= id)
          ]
      )

instance Data.ToPath GetRegexPatternSet where
  toPath = Prelude.const "/"

instance Data.ToQuery GetRegexPatternSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRegexPatternSetResponse' smart constructor.
data GetRegexPatternSetResponse = GetRegexPatternSetResponse'
  { -- | A token used for optimistic locking. WAF returns a token to your @get@
    -- and @list@ requests, to mark the state of the entity at the time of the
    -- request. To make changes to the entity associated with the token, you
    -- provide the token to operations like @update@ and @delete@. WAF uses the
    -- token to ensure that no changes have been made to the entity since you
    -- last retrieved it. If a change has been made, the update fails with a
    -- @WAFOptimisticLockException@. If this happens, perform another @get@,
    -- and use the new token returned by that operation.
    lockToken :: Prelude.Maybe Prelude.Text,
    regexPatternSet :: Prelude.Maybe RegexPatternSet,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRegexPatternSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lockToken', 'getRegexPatternSetResponse_lockToken' - A token used for optimistic locking. WAF returns a token to your @get@
-- and @list@ requests, to mark the state of the entity at the time of the
-- request. To make changes to the entity associated with the token, you
-- provide the token to operations like @update@ and @delete@. WAF uses the
-- token to ensure that no changes have been made to the entity since you
-- last retrieved it. If a change has been made, the update fails with a
-- @WAFOptimisticLockException@. If this happens, perform another @get@,
-- and use the new token returned by that operation.
--
-- 'regexPatternSet', 'getRegexPatternSetResponse_regexPatternSet' -
--
-- 'httpStatus', 'getRegexPatternSetResponse_httpStatus' - The response's http status code.
newGetRegexPatternSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRegexPatternSetResponse
newGetRegexPatternSetResponse pHttpStatus_ =
  GetRegexPatternSetResponse'
    { lockToken =
        Prelude.Nothing,
      regexPatternSet = Prelude.Nothing,
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
getRegexPatternSetResponse_lockToken :: Lens.Lens' GetRegexPatternSetResponse (Prelude.Maybe Prelude.Text)
getRegexPatternSetResponse_lockToken = Lens.lens (\GetRegexPatternSetResponse' {lockToken} -> lockToken) (\s@GetRegexPatternSetResponse' {} a -> s {lockToken = a} :: GetRegexPatternSetResponse)

getRegexPatternSetResponse_regexPatternSet :: Lens.Lens' GetRegexPatternSetResponse (Prelude.Maybe RegexPatternSet)
getRegexPatternSetResponse_regexPatternSet = Lens.lens (\GetRegexPatternSetResponse' {regexPatternSet} -> regexPatternSet) (\s@GetRegexPatternSetResponse' {} a -> s {regexPatternSet = a} :: GetRegexPatternSetResponse)

-- | The response's http status code.
getRegexPatternSetResponse_httpStatus :: Lens.Lens' GetRegexPatternSetResponse Prelude.Int
getRegexPatternSetResponse_httpStatus = Lens.lens (\GetRegexPatternSetResponse' {httpStatus} -> httpStatus) (\s@GetRegexPatternSetResponse' {} a -> s {httpStatus = a} :: GetRegexPatternSetResponse)

instance Prelude.NFData GetRegexPatternSetResponse where
  rnf GetRegexPatternSetResponse' {..} =
    Prelude.rnf lockToken
      `Prelude.seq` Prelude.rnf regexPatternSet
      `Prelude.seq` Prelude.rnf httpStatus
