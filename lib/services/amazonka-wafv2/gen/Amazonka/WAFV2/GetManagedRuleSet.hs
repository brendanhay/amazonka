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
-- Module      : Amazonka.WAFV2.GetManagedRuleSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the specified managed rule set.
--
-- This is intended for use only by vendors of managed rule sets. Vendors
-- are Amazon Web Services and Amazon Web Services Marketplace sellers.
--
-- Vendors, you can use the managed rule set APIs to provide controlled
-- rollout of your versioned managed rule group offerings for your
-- customers. The APIs are @ListManagedRuleSets@, @GetManagedRuleSet@,
-- @PutManagedRuleSetVersions@, and
-- @UpdateManagedRuleSetVersionExpiryDate@.
module Amazonka.WAFV2.GetManagedRuleSet
  ( -- * Creating a Request
    GetManagedRuleSet (..),
    newGetManagedRuleSet,

    -- * Request Lenses
    getManagedRuleSet_name,
    getManagedRuleSet_scope,
    getManagedRuleSet_id,

    -- * Destructuring the Response
    GetManagedRuleSetResponse (..),
    newGetManagedRuleSetResponse,

    -- * Response Lenses
    getManagedRuleSetResponse_lockToken,
    getManagedRuleSetResponse_managedRuleSet,
    getManagedRuleSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFV2.Types

-- | /See:/ 'newGetManagedRuleSet' smart constructor.
data GetManagedRuleSet = GetManagedRuleSet'
  { -- | The name of the managed rule set. You use this, along with the rule set
    -- ID, to identify the rule set.
    --
    -- This name is assigned to the corresponding managed rule group, which
    -- your customers can access and use.
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
    -- | A unique identifier for the managed rule set. The ID is returned in the
    -- responses to commands like @list@. You provide it to operations like
    -- @get@ and @update@.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetManagedRuleSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getManagedRuleSet_name' - The name of the managed rule set. You use this, along with the rule set
-- ID, to identify the rule set.
--
-- This name is assigned to the corresponding managed rule group, which
-- your customers can access and use.
--
-- 'scope', 'getManagedRuleSet_scope' - Specifies whether this is for an Amazon CloudFront distribution or for a
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
-- 'id', 'getManagedRuleSet_id' - A unique identifier for the managed rule set. The ID is returned in the
-- responses to commands like @list@. You provide it to operations like
-- @get@ and @update@.
newGetManagedRuleSet ::
  -- | 'name'
  Prelude.Text ->
  -- | 'scope'
  Scope ->
  -- | 'id'
  Prelude.Text ->
  GetManagedRuleSet
newGetManagedRuleSet pName_ pScope_ pId_ =
  GetManagedRuleSet'
    { name = pName_,
      scope = pScope_,
      id = pId_
    }

-- | The name of the managed rule set. You use this, along with the rule set
-- ID, to identify the rule set.
--
-- This name is assigned to the corresponding managed rule group, which
-- your customers can access and use.
getManagedRuleSet_name :: Lens.Lens' GetManagedRuleSet Prelude.Text
getManagedRuleSet_name = Lens.lens (\GetManagedRuleSet' {name} -> name) (\s@GetManagedRuleSet' {} a -> s {name = a} :: GetManagedRuleSet)

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
getManagedRuleSet_scope :: Lens.Lens' GetManagedRuleSet Scope
getManagedRuleSet_scope = Lens.lens (\GetManagedRuleSet' {scope} -> scope) (\s@GetManagedRuleSet' {} a -> s {scope = a} :: GetManagedRuleSet)

-- | A unique identifier for the managed rule set. The ID is returned in the
-- responses to commands like @list@. You provide it to operations like
-- @get@ and @update@.
getManagedRuleSet_id :: Lens.Lens' GetManagedRuleSet Prelude.Text
getManagedRuleSet_id = Lens.lens (\GetManagedRuleSet' {id} -> id) (\s@GetManagedRuleSet' {} a -> s {id = a} :: GetManagedRuleSet)

instance Core.AWSRequest GetManagedRuleSet where
  type
    AWSResponse GetManagedRuleSet =
      GetManagedRuleSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetManagedRuleSetResponse'
            Prelude.<$> (x Data..?> "LockToken")
            Prelude.<*> (x Data..?> "ManagedRuleSet")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetManagedRuleSet where
  hashWithSalt _salt GetManagedRuleSet' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` scope
      `Prelude.hashWithSalt` id

instance Prelude.NFData GetManagedRuleSet where
  rnf GetManagedRuleSet' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf scope
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders GetManagedRuleSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_20190729.GetManagedRuleSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetManagedRuleSet where
  toJSON GetManagedRuleSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Scope" Data..= scope),
            Prelude.Just ("Id" Data..= id)
          ]
      )

instance Data.ToPath GetManagedRuleSet where
  toPath = Prelude.const "/"

instance Data.ToQuery GetManagedRuleSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetManagedRuleSetResponse' smart constructor.
data GetManagedRuleSetResponse = GetManagedRuleSetResponse'
  { -- | A token used for optimistic locking. WAF returns a token to your @get@
    -- and @list@ requests, to mark the state of the entity at the time of the
    -- request. To make changes to the entity associated with the token, you
    -- provide the token to operations like @update@ and @delete@. WAF uses the
    -- token to ensure that no changes have been made to the entity since you
    -- last retrieved it. If a change has been made, the update fails with a
    -- @WAFOptimisticLockException@. If this happens, perform another @get@,
    -- and use the new token returned by that operation.
    lockToken :: Prelude.Maybe Prelude.Text,
    -- | The managed rule set that you requested.
    managedRuleSet :: Prelude.Maybe ManagedRuleSet,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetManagedRuleSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lockToken', 'getManagedRuleSetResponse_lockToken' - A token used for optimistic locking. WAF returns a token to your @get@
-- and @list@ requests, to mark the state of the entity at the time of the
-- request. To make changes to the entity associated with the token, you
-- provide the token to operations like @update@ and @delete@. WAF uses the
-- token to ensure that no changes have been made to the entity since you
-- last retrieved it. If a change has been made, the update fails with a
-- @WAFOptimisticLockException@. If this happens, perform another @get@,
-- and use the new token returned by that operation.
--
-- 'managedRuleSet', 'getManagedRuleSetResponse_managedRuleSet' - The managed rule set that you requested.
--
-- 'httpStatus', 'getManagedRuleSetResponse_httpStatus' - The response's http status code.
newGetManagedRuleSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetManagedRuleSetResponse
newGetManagedRuleSetResponse pHttpStatus_ =
  GetManagedRuleSetResponse'
    { lockToken =
        Prelude.Nothing,
      managedRuleSet = Prelude.Nothing,
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
getManagedRuleSetResponse_lockToken :: Lens.Lens' GetManagedRuleSetResponse (Prelude.Maybe Prelude.Text)
getManagedRuleSetResponse_lockToken = Lens.lens (\GetManagedRuleSetResponse' {lockToken} -> lockToken) (\s@GetManagedRuleSetResponse' {} a -> s {lockToken = a} :: GetManagedRuleSetResponse)

-- | The managed rule set that you requested.
getManagedRuleSetResponse_managedRuleSet :: Lens.Lens' GetManagedRuleSetResponse (Prelude.Maybe ManagedRuleSet)
getManagedRuleSetResponse_managedRuleSet = Lens.lens (\GetManagedRuleSetResponse' {managedRuleSet} -> managedRuleSet) (\s@GetManagedRuleSetResponse' {} a -> s {managedRuleSet = a} :: GetManagedRuleSetResponse)

-- | The response's http status code.
getManagedRuleSetResponse_httpStatus :: Lens.Lens' GetManagedRuleSetResponse Prelude.Int
getManagedRuleSetResponse_httpStatus = Lens.lens (\GetManagedRuleSetResponse' {httpStatus} -> httpStatus) (\s@GetManagedRuleSetResponse' {} a -> s {httpStatus = a} :: GetManagedRuleSetResponse)

instance Prelude.NFData GetManagedRuleSetResponse where
  rnf GetManagedRuleSetResponse' {..} =
    Prelude.rnf lockToken
      `Prelude.seq` Prelude.rnf managedRuleSet
      `Prelude.seq` Prelude.rnf httpStatus
