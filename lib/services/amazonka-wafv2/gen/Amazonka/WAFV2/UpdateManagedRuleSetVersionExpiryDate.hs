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
-- Module      : Amazonka.WAFV2.UpdateManagedRuleSetVersionExpiryDate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the expiration information for your managed rule set. Use this
-- to initiate the expiration of a managed rule group version. After you
-- initiate expiration for a version, WAF excludes it from the response to
-- ListAvailableManagedRuleGroupVersions for the managed rule group.
--
-- This is intended for use only by vendors of managed rule sets. Vendors
-- are Amazon Web Services and Amazon Web Services Marketplace sellers.
--
-- Vendors, you can use the managed rule set APIs to provide controlled
-- rollout of your versioned managed rule group offerings for your
-- customers. The APIs are @ListManagedRuleSets@, @GetManagedRuleSet@,
-- @PutManagedRuleSetVersions@, and
-- @UpdateManagedRuleSetVersionExpiryDate@.
module Amazonka.WAFV2.UpdateManagedRuleSetVersionExpiryDate
  ( -- * Creating a Request
    UpdateManagedRuleSetVersionExpiryDate (..),
    newUpdateManagedRuleSetVersionExpiryDate,

    -- * Request Lenses
    updateManagedRuleSetVersionExpiryDate_name,
    updateManagedRuleSetVersionExpiryDate_scope,
    updateManagedRuleSetVersionExpiryDate_id,
    updateManagedRuleSetVersionExpiryDate_lockToken,
    updateManagedRuleSetVersionExpiryDate_versionToExpire,
    updateManagedRuleSetVersionExpiryDate_expiryTimestamp,

    -- * Destructuring the Response
    UpdateManagedRuleSetVersionExpiryDateResponse (..),
    newUpdateManagedRuleSetVersionExpiryDateResponse,

    -- * Response Lenses
    updateManagedRuleSetVersionExpiryDateResponse_expiringVersion,
    updateManagedRuleSetVersionExpiryDateResponse_expiryTimestamp,
    updateManagedRuleSetVersionExpiryDateResponse_nextLockToken,
    updateManagedRuleSetVersionExpiryDateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFV2.Types

-- | /See:/ 'newUpdateManagedRuleSetVersionExpiryDate' smart constructor.
data UpdateManagedRuleSetVersionExpiryDate = UpdateManagedRuleSetVersionExpiryDate'
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
    id :: Prelude.Text,
    -- | A token used for optimistic locking. WAF returns a token to your @get@
    -- and @list@ requests, to mark the state of the entity at the time of the
    -- request. To make changes to the entity associated with the token, you
    -- provide the token to operations like @update@ and @delete@. WAF uses the
    -- token to ensure that no changes have been made to the entity since you
    -- last retrieved it. If a change has been made, the update fails with a
    -- @WAFOptimisticLockException@. If this happens, perform another @get@,
    -- and use the new token returned by that operation.
    lockToken :: Prelude.Text,
    -- | The version that you want to remove from your list of offerings for the
    -- named managed rule group.
    versionToExpire :: Prelude.Text,
    -- | The time that you want the version to expire.
    --
    -- Times are in Coordinated Universal Time (UTC) format. UTC format
    -- includes the special designator, Z. For example, \"2016-09-27T14:50Z\".
    expiryTimestamp :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateManagedRuleSetVersionExpiryDate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateManagedRuleSetVersionExpiryDate_name' - The name of the managed rule set. You use this, along with the rule set
-- ID, to identify the rule set.
--
-- This name is assigned to the corresponding managed rule group, which
-- your customers can access and use.
--
-- 'scope', 'updateManagedRuleSetVersionExpiryDate_scope' - Specifies whether this is for an Amazon CloudFront distribution or for a
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
-- 'id', 'updateManagedRuleSetVersionExpiryDate_id' - A unique identifier for the managed rule set. The ID is returned in the
-- responses to commands like @list@. You provide it to operations like
-- @get@ and @update@.
--
-- 'lockToken', 'updateManagedRuleSetVersionExpiryDate_lockToken' - A token used for optimistic locking. WAF returns a token to your @get@
-- and @list@ requests, to mark the state of the entity at the time of the
-- request. To make changes to the entity associated with the token, you
-- provide the token to operations like @update@ and @delete@. WAF uses the
-- token to ensure that no changes have been made to the entity since you
-- last retrieved it. If a change has been made, the update fails with a
-- @WAFOptimisticLockException@. If this happens, perform another @get@,
-- and use the new token returned by that operation.
--
-- 'versionToExpire', 'updateManagedRuleSetVersionExpiryDate_versionToExpire' - The version that you want to remove from your list of offerings for the
-- named managed rule group.
--
-- 'expiryTimestamp', 'updateManagedRuleSetVersionExpiryDate_expiryTimestamp' - The time that you want the version to expire.
--
-- Times are in Coordinated Universal Time (UTC) format. UTC format
-- includes the special designator, Z. For example, \"2016-09-27T14:50Z\".
newUpdateManagedRuleSetVersionExpiryDate ::
  -- | 'name'
  Prelude.Text ->
  -- | 'scope'
  Scope ->
  -- | 'id'
  Prelude.Text ->
  -- | 'lockToken'
  Prelude.Text ->
  -- | 'versionToExpire'
  Prelude.Text ->
  -- | 'expiryTimestamp'
  Prelude.UTCTime ->
  UpdateManagedRuleSetVersionExpiryDate
newUpdateManagedRuleSetVersionExpiryDate
  pName_
  pScope_
  pId_
  pLockToken_
  pVersionToExpire_
  pExpiryTimestamp_ =
    UpdateManagedRuleSetVersionExpiryDate'
      { name =
          pName_,
        scope = pScope_,
        id = pId_,
        lockToken = pLockToken_,
        versionToExpire = pVersionToExpire_,
        expiryTimestamp =
          Data._Time
            Lens.# pExpiryTimestamp_
      }

-- | The name of the managed rule set. You use this, along with the rule set
-- ID, to identify the rule set.
--
-- This name is assigned to the corresponding managed rule group, which
-- your customers can access and use.
updateManagedRuleSetVersionExpiryDate_name :: Lens.Lens' UpdateManagedRuleSetVersionExpiryDate Prelude.Text
updateManagedRuleSetVersionExpiryDate_name = Lens.lens (\UpdateManagedRuleSetVersionExpiryDate' {name} -> name) (\s@UpdateManagedRuleSetVersionExpiryDate' {} a -> s {name = a} :: UpdateManagedRuleSetVersionExpiryDate)

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
updateManagedRuleSetVersionExpiryDate_scope :: Lens.Lens' UpdateManagedRuleSetVersionExpiryDate Scope
updateManagedRuleSetVersionExpiryDate_scope = Lens.lens (\UpdateManagedRuleSetVersionExpiryDate' {scope} -> scope) (\s@UpdateManagedRuleSetVersionExpiryDate' {} a -> s {scope = a} :: UpdateManagedRuleSetVersionExpiryDate)

-- | A unique identifier for the managed rule set. The ID is returned in the
-- responses to commands like @list@. You provide it to operations like
-- @get@ and @update@.
updateManagedRuleSetVersionExpiryDate_id :: Lens.Lens' UpdateManagedRuleSetVersionExpiryDate Prelude.Text
updateManagedRuleSetVersionExpiryDate_id = Lens.lens (\UpdateManagedRuleSetVersionExpiryDate' {id} -> id) (\s@UpdateManagedRuleSetVersionExpiryDate' {} a -> s {id = a} :: UpdateManagedRuleSetVersionExpiryDate)

-- | A token used for optimistic locking. WAF returns a token to your @get@
-- and @list@ requests, to mark the state of the entity at the time of the
-- request. To make changes to the entity associated with the token, you
-- provide the token to operations like @update@ and @delete@. WAF uses the
-- token to ensure that no changes have been made to the entity since you
-- last retrieved it. If a change has been made, the update fails with a
-- @WAFOptimisticLockException@. If this happens, perform another @get@,
-- and use the new token returned by that operation.
updateManagedRuleSetVersionExpiryDate_lockToken :: Lens.Lens' UpdateManagedRuleSetVersionExpiryDate Prelude.Text
updateManagedRuleSetVersionExpiryDate_lockToken = Lens.lens (\UpdateManagedRuleSetVersionExpiryDate' {lockToken} -> lockToken) (\s@UpdateManagedRuleSetVersionExpiryDate' {} a -> s {lockToken = a} :: UpdateManagedRuleSetVersionExpiryDate)

-- | The version that you want to remove from your list of offerings for the
-- named managed rule group.
updateManagedRuleSetVersionExpiryDate_versionToExpire :: Lens.Lens' UpdateManagedRuleSetVersionExpiryDate Prelude.Text
updateManagedRuleSetVersionExpiryDate_versionToExpire = Lens.lens (\UpdateManagedRuleSetVersionExpiryDate' {versionToExpire} -> versionToExpire) (\s@UpdateManagedRuleSetVersionExpiryDate' {} a -> s {versionToExpire = a} :: UpdateManagedRuleSetVersionExpiryDate)

-- | The time that you want the version to expire.
--
-- Times are in Coordinated Universal Time (UTC) format. UTC format
-- includes the special designator, Z. For example, \"2016-09-27T14:50Z\".
updateManagedRuleSetVersionExpiryDate_expiryTimestamp :: Lens.Lens' UpdateManagedRuleSetVersionExpiryDate Prelude.UTCTime
updateManagedRuleSetVersionExpiryDate_expiryTimestamp = Lens.lens (\UpdateManagedRuleSetVersionExpiryDate' {expiryTimestamp} -> expiryTimestamp) (\s@UpdateManagedRuleSetVersionExpiryDate' {} a -> s {expiryTimestamp = a} :: UpdateManagedRuleSetVersionExpiryDate) Prelude.. Data._Time

instance
  Core.AWSRequest
    UpdateManagedRuleSetVersionExpiryDate
  where
  type
    AWSResponse
      UpdateManagedRuleSetVersionExpiryDate =
      UpdateManagedRuleSetVersionExpiryDateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateManagedRuleSetVersionExpiryDateResponse'
            Prelude.<$> (x Data..?> "ExpiringVersion")
              Prelude.<*> (x Data..?> "ExpiryTimestamp")
              Prelude.<*> (x Data..?> "NextLockToken")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateManagedRuleSetVersionExpiryDate
  where
  hashWithSalt
    _salt
    UpdateManagedRuleSetVersionExpiryDate' {..} =
      _salt `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` scope
        `Prelude.hashWithSalt` id
        `Prelude.hashWithSalt` lockToken
        `Prelude.hashWithSalt` versionToExpire
        `Prelude.hashWithSalt` expiryTimestamp

instance
  Prelude.NFData
    UpdateManagedRuleSetVersionExpiryDate
  where
  rnf UpdateManagedRuleSetVersionExpiryDate' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf scope
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lockToken
      `Prelude.seq` Prelude.rnf versionToExpire
      `Prelude.seq` Prelude.rnf expiryTimestamp

instance
  Data.ToHeaders
    UpdateManagedRuleSetVersionExpiryDate
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_20190729.UpdateManagedRuleSetVersionExpiryDate" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    UpdateManagedRuleSetVersionExpiryDate
  where
  toJSON UpdateManagedRuleSetVersionExpiryDate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Scope" Data..= scope),
            Prelude.Just ("Id" Data..= id),
            Prelude.Just ("LockToken" Data..= lockToken),
            Prelude.Just
              ("VersionToExpire" Data..= versionToExpire),
            Prelude.Just
              ("ExpiryTimestamp" Data..= expiryTimestamp)
          ]
      )

instance
  Data.ToPath
    UpdateManagedRuleSetVersionExpiryDate
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    UpdateManagedRuleSetVersionExpiryDate
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateManagedRuleSetVersionExpiryDateResponse' smart constructor.
data UpdateManagedRuleSetVersionExpiryDateResponse = UpdateManagedRuleSetVersionExpiryDateResponse'
  { -- | The version that is set to expire.
    expiringVersion :: Prelude.Maybe Prelude.Text,
    -- | The time that the version will expire.
    --
    -- Times are in Coordinated Universal Time (UTC) format. UTC format
    -- includes the special designator, Z. For example, \"2016-09-27T14:50Z\".
    expiryTimestamp :: Prelude.Maybe Data.POSIX,
    -- | A token used for optimistic locking. WAF returns a token to your @get@
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
-- Create a value of 'UpdateManagedRuleSetVersionExpiryDateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expiringVersion', 'updateManagedRuleSetVersionExpiryDateResponse_expiringVersion' - The version that is set to expire.
--
-- 'expiryTimestamp', 'updateManagedRuleSetVersionExpiryDateResponse_expiryTimestamp' - The time that the version will expire.
--
-- Times are in Coordinated Universal Time (UTC) format. UTC format
-- includes the special designator, Z. For example, \"2016-09-27T14:50Z\".
--
-- 'nextLockToken', 'updateManagedRuleSetVersionExpiryDateResponse_nextLockToken' - A token used for optimistic locking. WAF returns a token to your @get@
-- and @list@ requests, to mark the state of the entity at the time of the
-- request. To make changes to the entity associated with the token, you
-- provide the token to operations like @update@ and @delete@. WAF uses the
-- token to ensure that no changes have been made to the entity since you
-- last retrieved it. If a change has been made, the update fails with a
-- @WAFOptimisticLockException@. If this happens, perform another @get@,
-- and use the new token returned by that operation.
--
-- 'httpStatus', 'updateManagedRuleSetVersionExpiryDateResponse_httpStatus' - The response's http status code.
newUpdateManagedRuleSetVersionExpiryDateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateManagedRuleSetVersionExpiryDateResponse
newUpdateManagedRuleSetVersionExpiryDateResponse
  pHttpStatus_ =
    UpdateManagedRuleSetVersionExpiryDateResponse'
      { expiringVersion =
          Prelude.Nothing,
        expiryTimestamp =
          Prelude.Nothing,
        nextLockToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The version that is set to expire.
updateManagedRuleSetVersionExpiryDateResponse_expiringVersion :: Lens.Lens' UpdateManagedRuleSetVersionExpiryDateResponse (Prelude.Maybe Prelude.Text)
updateManagedRuleSetVersionExpiryDateResponse_expiringVersion = Lens.lens (\UpdateManagedRuleSetVersionExpiryDateResponse' {expiringVersion} -> expiringVersion) (\s@UpdateManagedRuleSetVersionExpiryDateResponse' {} a -> s {expiringVersion = a} :: UpdateManagedRuleSetVersionExpiryDateResponse)

-- | The time that the version will expire.
--
-- Times are in Coordinated Universal Time (UTC) format. UTC format
-- includes the special designator, Z. For example, \"2016-09-27T14:50Z\".
updateManagedRuleSetVersionExpiryDateResponse_expiryTimestamp :: Lens.Lens' UpdateManagedRuleSetVersionExpiryDateResponse (Prelude.Maybe Prelude.UTCTime)
updateManagedRuleSetVersionExpiryDateResponse_expiryTimestamp = Lens.lens (\UpdateManagedRuleSetVersionExpiryDateResponse' {expiryTimestamp} -> expiryTimestamp) (\s@UpdateManagedRuleSetVersionExpiryDateResponse' {} a -> s {expiryTimestamp = a} :: UpdateManagedRuleSetVersionExpiryDateResponse) Prelude.. Lens.mapping Data._Time

-- | A token used for optimistic locking. WAF returns a token to your @get@
-- and @list@ requests, to mark the state of the entity at the time of the
-- request. To make changes to the entity associated with the token, you
-- provide the token to operations like @update@ and @delete@. WAF uses the
-- token to ensure that no changes have been made to the entity since you
-- last retrieved it. If a change has been made, the update fails with a
-- @WAFOptimisticLockException@. If this happens, perform another @get@,
-- and use the new token returned by that operation.
updateManagedRuleSetVersionExpiryDateResponse_nextLockToken :: Lens.Lens' UpdateManagedRuleSetVersionExpiryDateResponse (Prelude.Maybe Prelude.Text)
updateManagedRuleSetVersionExpiryDateResponse_nextLockToken = Lens.lens (\UpdateManagedRuleSetVersionExpiryDateResponse' {nextLockToken} -> nextLockToken) (\s@UpdateManagedRuleSetVersionExpiryDateResponse' {} a -> s {nextLockToken = a} :: UpdateManagedRuleSetVersionExpiryDateResponse)

-- | The response's http status code.
updateManagedRuleSetVersionExpiryDateResponse_httpStatus :: Lens.Lens' UpdateManagedRuleSetVersionExpiryDateResponse Prelude.Int
updateManagedRuleSetVersionExpiryDateResponse_httpStatus = Lens.lens (\UpdateManagedRuleSetVersionExpiryDateResponse' {httpStatus} -> httpStatus) (\s@UpdateManagedRuleSetVersionExpiryDateResponse' {} a -> s {httpStatus = a} :: UpdateManagedRuleSetVersionExpiryDateResponse)

instance
  Prelude.NFData
    UpdateManagedRuleSetVersionExpiryDateResponse
  where
  rnf
    UpdateManagedRuleSetVersionExpiryDateResponse' {..} =
      Prelude.rnf expiringVersion
        `Prelude.seq` Prelude.rnf expiryTimestamp
        `Prelude.seq` Prelude.rnf nextLockToken
        `Prelude.seq` Prelude.rnf httpStatus
