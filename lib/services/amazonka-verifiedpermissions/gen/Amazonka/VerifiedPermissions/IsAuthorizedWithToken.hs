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
-- Module      : Amazonka.VerifiedPermissions.IsAuthorizedWithToken
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Makes an authorization decision about a service request described in the
-- parameters. The principal in this request comes from an external
-- identity source. The information in the parameters can also define
-- additional context that Verified Permissions can include in the
-- evaluation. The request is evaluated against all matching policies in
-- the specified policy store. The result of the decision is either @Allow@
-- or @Deny@, along with a list of the policies that resulted in the
-- decision.
--
-- If you delete a Amazon Cognito user pool or user, tokens from that
-- deleted pool or that deleted user continue to be usable until they
-- expire.
module Amazonka.VerifiedPermissions.IsAuthorizedWithToken
  ( -- * Creating a Request
    IsAuthorizedWithToken (..),
    newIsAuthorizedWithToken,

    -- * Request Lenses
    isAuthorizedWithToken_accessToken,
    isAuthorizedWithToken_action,
    isAuthorizedWithToken_context,
    isAuthorizedWithToken_entities,
    isAuthorizedWithToken_identityToken,
    isAuthorizedWithToken_resource,
    isAuthorizedWithToken_policyStoreId,

    -- * Destructuring the Response
    IsAuthorizedWithTokenResponse (..),
    newIsAuthorizedWithTokenResponse,

    -- * Response Lenses
    isAuthorizedWithTokenResponse_httpStatus,
    isAuthorizedWithTokenResponse_decision,
    isAuthorizedWithTokenResponse_determiningPolicies,
    isAuthorizedWithTokenResponse_errors,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VerifiedPermissions.Types

-- | /See:/ 'newIsAuthorizedWithToken' smart constructor.
data IsAuthorizedWithToken = IsAuthorizedWithToken'
  { -- | Specifies an access token for the principal to be authorized. This token
    -- is provided to you by the identity provider (IdP) associated with the
    -- specified identity source. You must specify either an @AccessToken@ or
    -- an @IdentityToken@, but not both.
    accessToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies the requested action to be authorized. Is the specified
    -- principal authorized to perform this action on the specified resource.
    action :: Prelude.Maybe ActionIdentifier,
    -- | Specifies additional context that can be used to make more granular
    -- authorization decisions.
    context :: Prelude.Maybe ContextDefinition,
    -- | Specifies the list of resources and principals and their associated
    -- attributes that Verified Permissions can examine when evaluating the
    -- policies.
    --
    -- You can include only principal and resource entities in this parameter;
    -- you can\'t include actions. You must specify actions in the schema.
    entities :: Prelude.Maybe EntitiesDefinition,
    -- | Specifies an identity token for the principal to be authorized. This
    -- token is provided to you by the identity provider (IdP) associated with
    -- the specified identity source. You must specify either an @AccessToken@
    -- or an @IdentityToken@, but not both.
    identityToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies the resource for which the authorization decision is made. For
    -- example, is the principal allowed to perform the action on the resource?
    resource :: Prelude.Maybe EntityIdentifier,
    -- | Specifies the ID of the policy store. Policies in this policy store will
    -- be used to make an authorization decision for the input.
    policyStoreId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IsAuthorizedWithToken' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessToken', 'isAuthorizedWithToken_accessToken' - Specifies an access token for the principal to be authorized. This token
-- is provided to you by the identity provider (IdP) associated with the
-- specified identity source. You must specify either an @AccessToken@ or
-- an @IdentityToken@, but not both.
--
-- 'action', 'isAuthorizedWithToken_action' - Specifies the requested action to be authorized. Is the specified
-- principal authorized to perform this action on the specified resource.
--
-- 'context', 'isAuthorizedWithToken_context' - Specifies additional context that can be used to make more granular
-- authorization decisions.
--
-- 'entities', 'isAuthorizedWithToken_entities' - Specifies the list of resources and principals and their associated
-- attributes that Verified Permissions can examine when evaluating the
-- policies.
--
-- You can include only principal and resource entities in this parameter;
-- you can\'t include actions. You must specify actions in the schema.
--
-- 'identityToken', 'isAuthorizedWithToken_identityToken' - Specifies an identity token for the principal to be authorized. This
-- token is provided to you by the identity provider (IdP) associated with
-- the specified identity source. You must specify either an @AccessToken@
-- or an @IdentityToken@, but not both.
--
-- 'resource', 'isAuthorizedWithToken_resource' - Specifies the resource for which the authorization decision is made. For
-- example, is the principal allowed to perform the action on the resource?
--
-- 'policyStoreId', 'isAuthorizedWithToken_policyStoreId' - Specifies the ID of the policy store. Policies in this policy store will
-- be used to make an authorization decision for the input.
newIsAuthorizedWithToken ::
  -- | 'policyStoreId'
  Prelude.Text ->
  IsAuthorizedWithToken
newIsAuthorizedWithToken pPolicyStoreId_ =
  IsAuthorizedWithToken'
    { accessToken =
        Prelude.Nothing,
      action = Prelude.Nothing,
      context = Prelude.Nothing,
      entities = Prelude.Nothing,
      identityToken = Prelude.Nothing,
      resource = Prelude.Nothing,
      policyStoreId = pPolicyStoreId_
    }

-- | Specifies an access token for the principal to be authorized. This token
-- is provided to you by the identity provider (IdP) associated with the
-- specified identity source. You must specify either an @AccessToken@ or
-- an @IdentityToken@, but not both.
isAuthorizedWithToken_accessToken :: Lens.Lens' IsAuthorizedWithToken (Prelude.Maybe Prelude.Text)
isAuthorizedWithToken_accessToken = Lens.lens (\IsAuthorizedWithToken' {accessToken} -> accessToken) (\s@IsAuthorizedWithToken' {} a -> s {accessToken = a} :: IsAuthorizedWithToken)

-- | Specifies the requested action to be authorized. Is the specified
-- principal authorized to perform this action on the specified resource.
isAuthorizedWithToken_action :: Lens.Lens' IsAuthorizedWithToken (Prelude.Maybe ActionIdentifier)
isAuthorizedWithToken_action = Lens.lens (\IsAuthorizedWithToken' {action} -> action) (\s@IsAuthorizedWithToken' {} a -> s {action = a} :: IsAuthorizedWithToken)

-- | Specifies additional context that can be used to make more granular
-- authorization decisions.
isAuthorizedWithToken_context :: Lens.Lens' IsAuthorizedWithToken (Prelude.Maybe ContextDefinition)
isAuthorizedWithToken_context = Lens.lens (\IsAuthorizedWithToken' {context} -> context) (\s@IsAuthorizedWithToken' {} a -> s {context = a} :: IsAuthorizedWithToken)

-- | Specifies the list of resources and principals and their associated
-- attributes that Verified Permissions can examine when evaluating the
-- policies.
--
-- You can include only principal and resource entities in this parameter;
-- you can\'t include actions. You must specify actions in the schema.
isAuthorizedWithToken_entities :: Lens.Lens' IsAuthorizedWithToken (Prelude.Maybe EntitiesDefinition)
isAuthorizedWithToken_entities = Lens.lens (\IsAuthorizedWithToken' {entities} -> entities) (\s@IsAuthorizedWithToken' {} a -> s {entities = a} :: IsAuthorizedWithToken)

-- | Specifies an identity token for the principal to be authorized. This
-- token is provided to you by the identity provider (IdP) associated with
-- the specified identity source. You must specify either an @AccessToken@
-- or an @IdentityToken@, but not both.
isAuthorizedWithToken_identityToken :: Lens.Lens' IsAuthorizedWithToken (Prelude.Maybe Prelude.Text)
isAuthorizedWithToken_identityToken = Lens.lens (\IsAuthorizedWithToken' {identityToken} -> identityToken) (\s@IsAuthorizedWithToken' {} a -> s {identityToken = a} :: IsAuthorizedWithToken)

-- | Specifies the resource for which the authorization decision is made. For
-- example, is the principal allowed to perform the action on the resource?
isAuthorizedWithToken_resource :: Lens.Lens' IsAuthorizedWithToken (Prelude.Maybe EntityIdentifier)
isAuthorizedWithToken_resource = Lens.lens (\IsAuthorizedWithToken' {resource} -> resource) (\s@IsAuthorizedWithToken' {} a -> s {resource = a} :: IsAuthorizedWithToken)

-- | Specifies the ID of the policy store. Policies in this policy store will
-- be used to make an authorization decision for the input.
isAuthorizedWithToken_policyStoreId :: Lens.Lens' IsAuthorizedWithToken Prelude.Text
isAuthorizedWithToken_policyStoreId = Lens.lens (\IsAuthorizedWithToken' {policyStoreId} -> policyStoreId) (\s@IsAuthorizedWithToken' {} a -> s {policyStoreId = a} :: IsAuthorizedWithToken)

instance Core.AWSRequest IsAuthorizedWithToken where
  type
    AWSResponse IsAuthorizedWithToken =
      IsAuthorizedWithTokenResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          IsAuthorizedWithTokenResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "decision")
            Prelude.<*> ( x
                            Data..?> "determiningPolicies"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "errors" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable IsAuthorizedWithToken where
  hashWithSalt _salt IsAuthorizedWithToken' {..} =
    _salt
      `Prelude.hashWithSalt` accessToken
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` context
      `Prelude.hashWithSalt` entities
      `Prelude.hashWithSalt` identityToken
      `Prelude.hashWithSalt` resource
      `Prelude.hashWithSalt` policyStoreId

instance Prelude.NFData IsAuthorizedWithToken where
  rnf IsAuthorizedWithToken' {..} =
    Prelude.rnf accessToken
      `Prelude.seq` Prelude.rnf action
      `Prelude.seq` Prelude.rnf context
      `Prelude.seq` Prelude.rnf entities
      `Prelude.seq` Prelude.rnf identityToken
      `Prelude.seq` Prelude.rnf resource
      `Prelude.seq` Prelude.rnf policyStoreId

instance Data.ToHeaders IsAuthorizedWithToken where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "VerifiedPermissions.IsAuthorizedWithToken" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON IsAuthorizedWithToken where
  toJSON IsAuthorizedWithToken' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("accessToken" Data..=) Prelude.<$> accessToken,
            ("action" Data..=) Prelude.<$> action,
            ("context" Data..=) Prelude.<$> context,
            ("entities" Data..=) Prelude.<$> entities,
            ("identityToken" Data..=) Prelude.<$> identityToken,
            ("resource" Data..=) Prelude.<$> resource,
            Prelude.Just
              ("policyStoreId" Data..= policyStoreId)
          ]
      )

instance Data.ToPath IsAuthorizedWithToken where
  toPath = Prelude.const "/"

instance Data.ToQuery IsAuthorizedWithToken where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newIsAuthorizedWithTokenResponse' smart constructor.
data IsAuthorizedWithTokenResponse = IsAuthorizedWithTokenResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An authorization decision that indicates if the authorization request
    -- should be allowed or denied.
    decision :: Decision,
    -- | The list of determining policies used to make the authorization
    -- decision. For example, if there are multiple matching policies, where at
    -- least one is a forbid policy, then because forbid always overrides
    -- permit the forbid policies are the determining policies. If all matching
    -- policies are permit policies, then those policies are the determining
    -- policies. When no policies match and the response is the default DENY,
    -- there are no determining policies.
    determiningPolicies :: [DeterminingPolicyItem],
    -- | Errors that occurred while making an authorization decision. For
    -- example, a policy references an entity or entity attribute that does not
    -- exist in the slice.
    errors :: [EvaluationErrorItem]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IsAuthorizedWithTokenResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'isAuthorizedWithTokenResponse_httpStatus' - The response's http status code.
--
-- 'decision', 'isAuthorizedWithTokenResponse_decision' - An authorization decision that indicates if the authorization request
-- should be allowed or denied.
--
-- 'determiningPolicies', 'isAuthorizedWithTokenResponse_determiningPolicies' - The list of determining policies used to make the authorization
-- decision. For example, if there are multiple matching policies, where at
-- least one is a forbid policy, then because forbid always overrides
-- permit the forbid policies are the determining policies. If all matching
-- policies are permit policies, then those policies are the determining
-- policies. When no policies match and the response is the default DENY,
-- there are no determining policies.
--
-- 'errors', 'isAuthorizedWithTokenResponse_errors' - Errors that occurred while making an authorization decision. For
-- example, a policy references an entity or entity attribute that does not
-- exist in the slice.
newIsAuthorizedWithTokenResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'decision'
  Decision ->
  IsAuthorizedWithTokenResponse
newIsAuthorizedWithTokenResponse
  pHttpStatus_
  pDecision_ =
    IsAuthorizedWithTokenResponse'
      { httpStatus =
          pHttpStatus_,
        decision = pDecision_,
        determiningPolicies = Prelude.mempty,
        errors = Prelude.mempty
      }

-- | The response's http status code.
isAuthorizedWithTokenResponse_httpStatus :: Lens.Lens' IsAuthorizedWithTokenResponse Prelude.Int
isAuthorizedWithTokenResponse_httpStatus = Lens.lens (\IsAuthorizedWithTokenResponse' {httpStatus} -> httpStatus) (\s@IsAuthorizedWithTokenResponse' {} a -> s {httpStatus = a} :: IsAuthorizedWithTokenResponse)

-- | An authorization decision that indicates if the authorization request
-- should be allowed or denied.
isAuthorizedWithTokenResponse_decision :: Lens.Lens' IsAuthorizedWithTokenResponse Decision
isAuthorizedWithTokenResponse_decision = Lens.lens (\IsAuthorizedWithTokenResponse' {decision} -> decision) (\s@IsAuthorizedWithTokenResponse' {} a -> s {decision = a} :: IsAuthorizedWithTokenResponse)

-- | The list of determining policies used to make the authorization
-- decision. For example, if there are multiple matching policies, where at
-- least one is a forbid policy, then because forbid always overrides
-- permit the forbid policies are the determining policies. If all matching
-- policies are permit policies, then those policies are the determining
-- policies. When no policies match and the response is the default DENY,
-- there are no determining policies.
isAuthorizedWithTokenResponse_determiningPolicies :: Lens.Lens' IsAuthorizedWithTokenResponse [DeterminingPolicyItem]
isAuthorizedWithTokenResponse_determiningPolicies = Lens.lens (\IsAuthorizedWithTokenResponse' {determiningPolicies} -> determiningPolicies) (\s@IsAuthorizedWithTokenResponse' {} a -> s {determiningPolicies = a} :: IsAuthorizedWithTokenResponse) Prelude.. Lens.coerced

-- | Errors that occurred while making an authorization decision. For
-- example, a policy references an entity or entity attribute that does not
-- exist in the slice.
isAuthorizedWithTokenResponse_errors :: Lens.Lens' IsAuthorizedWithTokenResponse [EvaluationErrorItem]
isAuthorizedWithTokenResponse_errors = Lens.lens (\IsAuthorizedWithTokenResponse' {errors} -> errors) (\s@IsAuthorizedWithTokenResponse' {} a -> s {errors = a} :: IsAuthorizedWithTokenResponse) Prelude.. Lens.coerced

instance Prelude.NFData IsAuthorizedWithTokenResponse where
  rnf IsAuthorizedWithTokenResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf decision
      `Prelude.seq` Prelude.rnf determiningPolicies
      `Prelude.seq` Prelude.rnf errors
