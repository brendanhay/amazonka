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
-- Module      : Amazonka.WAFV2.PutManagedRuleSetVersions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Defines the versions of your managed rule set that you are offering to
-- the customers. Customers see your offerings as managed rule groups with
-- versioning.
--
-- This is intended for use only by vendors of managed rule sets. Vendors
-- are Amazon Web Services and Amazon Web Services Marketplace sellers.
--
-- Vendors, you can use the managed rule set APIs to provide controlled
-- rollout of your versioned managed rule group offerings for your
-- customers. The APIs are @ListManagedRuleSets@, @GetManagedRuleSet@,
-- @PutManagedRuleSetVersions@, and
-- @UpdateManagedRuleSetVersionExpiryDate@.
--
-- Customers retrieve their managed rule group list by calling
-- ListAvailableManagedRuleGroups. The name that you provide here for your
-- managed rule set is the name the customer sees for the corresponding
-- managed rule group. Customers can retrieve the available versions for a
-- managed rule group by calling ListAvailableManagedRuleGroupVersions. You
-- provide a rule group specification for each version. For each managed
-- rule set, you must specify a version that you recommend using.
--
-- To initiate the expiration of a managed rule group version, use
-- UpdateManagedRuleSetVersionExpiryDate.
module Amazonka.WAFV2.PutManagedRuleSetVersions
  ( -- * Creating a Request
    PutManagedRuleSetVersions (..),
    newPutManagedRuleSetVersions,

    -- * Request Lenses
    putManagedRuleSetVersions_recommendedVersion,
    putManagedRuleSetVersions_versionsToPublish,
    putManagedRuleSetVersions_name,
    putManagedRuleSetVersions_scope,
    putManagedRuleSetVersions_id,
    putManagedRuleSetVersions_lockToken,

    -- * Destructuring the Response
    PutManagedRuleSetVersionsResponse (..),
    newPutManagedRuleSetVersionsResponse,

    -- * Response Lenses
    putManagedRuleSetVersionsResponse_nextLockToken,
    putManagedRuleSetVersionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFV2.Types

-- | /See:/ 'newPutManagedRuleSetVersions' smart constructor.
data PutManagedRuleSetVersions = PutManagedRuleSetVersions'
  { -- | The version of the named managed rule group that you\'d like your
    -- customers to choose, from among your version offerings.
    recommendedVersion :: Prelude.Maybe Prelude.Text,
    -- | The versions of the named managed rule group that you want to offer to
    -- your customers.
    versionsToPublish :: Prelude.Maybe (Prelude.HashMap Prelude.Text VersionToPublish),
    -- | The name of the managed rule set. You use this, along with the rule set
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
-- Create a value of 'PutManagedRuleSetVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recommendedVersion', 'putManagedRuleSetVersions_recommendedVersion' - The version of the named managed rule group that you\'d like your
-- customers to choose, from among your version offerings.
--
-- 'versionsToPublish', 'putManagedRuleSetVersions_versionsToPublish' - The versions of the named managed rule group that you want to offer to
-- your customers.
--
-- 'name', 'putManagedRuleSetVersions_name' - The name of the managed rule set. You use this, along with the rule set
-- ID, to identify the rule set.
--
-- This name is assigned to the corresponding managed rule group, which
-- your customers can access and use.
--
-- 'scope', 'putManagedRuleSetVersions_scope' - Specifies whether this is for an Amazon CloudFront distribution or for a
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
-- 'id', 'putManagedRuleSetVersions_id' - A unique identifier for the managed rule set. The ID is returned in the
-- responses to commands like @list@. You provide it to operations like
-- @get@ and @update@.
--
-- 'lockToken', 'putManagedRuleSetVersions_lockToken' - A token used for optimistic locking. WAF returns a token to your @get@
-- and @list@ requests, to mark the state of the entity at the time of the
-- request. To make changes to the entity associated with the token, you
-- provide the token to operations like @update@ and @delete@. WAF uses the
-- token to ensure that no changes have been made to the entity since you
-- last retrieved it. If a change has been made, the update fails with a
-- @WAFOptimisticLockException@. If this happens, perform another @get@,
-- and use the new token returned by that operation.
newPutManagedRuleSetVersions ::
  -- | 'name'
  Prelude.Text ->
  -- | 'scope'
  Scope ->
  -- | 'id'
  Prelude.Text ->
  -- | 'lockToken'
  Prelude.Text ->
  PutManagedRuleSetVersions
newPutManagedRuleSetVersions
  pName_
  pScope_
  pId_
  pLockToken_ =
    PutManagedRuleSetVersions'
      { recommendedVersion =
          Prelude.Nothing,
        versionsToPublish = Prelude.Nothing,
        name = pName_,
        scope = pScope_,
        id = pId_,
        lockToken = pLockToken_
      }

-- | The version of the named managed rule group that you\'d like your
-- customers to choose, from among your version offerings.
putManagedRuleSetVersions_recommendedVersion :: Lens.Lens' PutManagedRuleSetVersions (Prelude.Maybe Prelude.Text)
putManagedRuleSetVersions_recommendedVersion = Lens.lens (\PutManagedRuleSetVersions' {recommendedVersion} -> recommendedVersion) (\s@PutManagedRuleSetVersions' {} a -> s {recommendedVersion = a} :: PutManagedRuleSetVersions)

-- | The versions of the named managed rule group that you want to offer to
-- your customers.
putManagedRuleSetVersions_versionsToPublish :: Lens.Lens' PutManagedRuleSetVersions (Prelude.Maybe (Prelude.HashMap Prelude.Text VersionToPublish))
putManagedRuleSetVersions_versionsToPublish = Lens.lens (\PutManagedRuleSetVersions' {versionsToPublish} -> versionsToPublish) (\s@PutManagedRuleSetVersions' {} a -> s {versionsToPublish = a} :: PutManagedRuleSetVersions) Prelude.. Lens.mapping Lens.coerced

-- | The name of the managed rule set. You use this, along with the rule set
-- ID, to identify the rule set.
--
-- This name is assigned to the corresponding managed rule group, which
-- your customers can access and use.
putManagedRuleSetVersions_name :: Lens.Lens' PutManagedRuleSetVersions Prelude.Text
putManagedRuleSetVersions_name = Lens.lens (\PutManagedRuleSetVersions' {name} -> name) (\s@PutManagedRuleSetVersions' {} a -> s {name = a} :: PutManagedRuleSetVersions)

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
putManagedRuleSetVersions_scope :: Lens.Lens' PutManagedRuleSetVersions Scope
putManagedRuleSetVersions_scope = Lens.lens (\PutManagedRuleSetVersions' {scope} -> scope) (\s@PutManagedRuleSetVersions' {} a -> s {scope = a} :: PutManagedRuleSetVersions)

-- | A unique identifier for the managed rule set. The ID is returned in the
-- responses to commands like @list@. You provide it to operations like
-- @get@ and @update@.
putManagedRuleSetVersions_id :: Lens.Lens' PutManagedRuleSetVersions Prelude.Text
putManagedRuleSetVersions_id = Lens.lens (\PutManagedRuleSetVersions' {id} -> id) (\s@PutManagedRuleSetVersions' {} a -> s {id = a} :: PutManagedRuleSetVersions)

-- | A token used for optimistic locking. WAF returns a token to your @get@
-- and @list@ requests, to mark the state of the entity at the time of the
-- request. To make changes to the entity associated with the token, you
-- provide the token to operations like @update@ and @delete@. WAF uses the
-- token to ensure that no changes have been made to the entity since you
-- last retrieved it. If a change has been made, the update fails with a
-- @WAFOptimisticLockException@. If this happens, perform another @get@,
-- and use the new token returned by that operation.
putManagedRuleSetVersions_lockToken :: Lens.Lens' PutManagedRuleSetVersions Prelude.Text
putManagedRuleSetVersions_lockToken = Lens.lens (\PutManagedRuleSetVersions' {lockToken} -> lockToken) (\s@PutManagedRuleSetVersions' {} a -> s {lockToken = a} :: PutManagedRuleSetVersions)

instance Core.AWSRequest PutManagedRuleSetVersions where
  type
    AWSResponse PutManagedRuleSetVersions =
      PutManagedRuleSetVersionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutManagedRuleSetVersionsResponse'
            Prelude.<$> (x Data..?> "NextLockToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutManagedRuleSetVersions where
  hashWithSalt _salt PutManagedRuleSetVersions' {..} =
    _salt
      `Prelude.hashWithSalt` recommendedVersion
      `Prelude.hashWithSalt` versionsToPublish
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` scope
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` lockToken

instance Prelude.NFData PutManagedRuleSetVersions where
  rnf PutManagedRuleSetVersions' {..} =
    Prelude.rnf recommendedVersion
      `Prelude.seq` Prelude.rnf versionsToPublish
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf scope
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lockToken

instance Data.ToHeaders PutManagedRuleSetVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_20190729.PutManagedRuleSetVersions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutManagedRuleSetVersions where
  toJSON PutManagedRuleSetVersions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RecommendedVersion" Data..=)
              Prelude.<$> recommendedVersion,
            ("VersionsToPublish" Data..=)
              Prelude.<$> versionsToPublish,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Scope" Data..= scope),
            Prelude.Just ("Id" Data..= id),
            Prelude.Just ("LockToken" Data..= lockToken)
          ]
      )

instance Data.ToPath PutManagedRuleSetVersions where
  toPath = Prelude.const "/"

instance Data.ToQuery PutManagedRuleSetVersions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutManagedRuleSetVersionsResponse' smart constructor.
data PutManagedRuleSetVersionsResponse = PutManagedRuleSetVersionsResponse'
  { -- | A token used for optimistic locking. WAF returns a token to your @get@
    -- and @list@ requests, to mark the state of the entity at the time of the
    -- request. To make changes to the entity associated with the token, you
    -- provide the token to operations like @update@ and @delete@. WAF uses the
    -- token to ensure that no changes have been made to the entity since you
    -- last retrieved it. If a change has been made, the update fails with a
    -- @WAFOptimisticLockException@. If this happens, perform another @get@,
    -- and use the new token returned by that operation.
    nextLockToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutManagedRuleSetVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextLockToken', 'putManagedRuleSetVersionsResponse_nextLockToken' - A token used for optimistic locking. WAF returns a token to your @get@
-- and @list@ requests, to mark the state of the entity at the time of the
-- request. To make changes to the entity associated with the token, you
-- provide the token to operations like @update@ and @delete@. WAF uses the
-- token to ensure that no changes have been made to the entity since you
-- last retrieved it. If a change has been made, the update fails with a
-- @WAFOptimisticLockException@. If this happens, perform another @get@,
-- and use the new token returned by that operation.
--
-- 'httpStatus', 'putManagedRuleSetVersionsResponse_httpStatus' - The response's http status code.
newPutManagedRuleSetVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutManagedRuleSetVersionsResponse
newPutManagedRuleSetVersionsResponse pHttpStatus_ =
  PutManagedRuleSetVersionsResponse'
    { nextLockToken =
        Prelude.Nothing,
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
putManagedRuleSetVersionsResponse_nextLockToken :: Lens.Lens' PutManagedRuleSetVersionsResponse (Prelude.Maybe Prelude.Text)
putManagedRuleSetVersionsResponse_nextLockToken = Lens.lens (\PutManagedRuleSetVersionsResponse' {nextLockToken} -> nextLockToken) (\s@PutManagedRuleSetVersionsResponse' {} a -> s {nextLockToken = a} :: PutManagedRuleSetVersionsResponse)

-- | The response's http status code.
putManagedRuleSetVersionsResponse_httpStatus :: Lens.Lens' PutManagedRuleSetVersionsResponse Prelude.Int
putManagedRuleSetVersionsResponse_httpStatus = Lens.lens (\PutManagedRuleSetVersionsResponse' {httpStatus} -> httpStatus) (\s@PutManagedRuleSetVersionsResponse' {} a -> s {httpStatus = a} :: PutManagedRuleSetVersionsResponse)

instance
  Prelude.NFData
    PutManagedRuleSetVersionsResponse
  where
  rnf PutManagedRuleSetVersionsResponse' {..} =
    Prelude.rnf nextLockToken
      `Prelude.seq` Prelude.rnf httpStatus
