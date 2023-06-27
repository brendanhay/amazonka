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
-- Module      : Amazonka.VerifiedPermissions.IsAuthorized
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Makes an authorization decision about a service request described in the
-- parameters. The information in the parameters can also define additional
-- context that Verified Permissions can include in the evaluation. The
-- request is evaluated against all matching policies in the specified
-- policy store. The result of the decision is either @Allow@ or @Deny@,
-- along with a list of the policies that resulted in the decision.
module Amazonka.VerifiedPermissions.IsAuthorized
  ( -- * Creating a Request
    IsAuthorized (..),
    newIsAuthorized,

    -- * Request Lenses
    isAuthorized_action,
    isAuthorized_context,
    isAuthorized_entities,
    isAuthorized_principal,
    isAuthorized_resource,
    isAuthorized_policyStoreId,

    -- * Destructuring the Response
    IsAuthorizedResponse (..),
    newIsAuthorizedResponse,

    -- * Response Lenses
    isAuthorizedResponse_httpStatus,
    isAuthorizedResponse_decision,
    isAuthorizedResponse_determiningPolicies,
    isAuthorizedResponse_errors,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VerifiedPermissions.Types

-- | /See:/ 'newIsAuthorized' smart constructor.
data IsAuthorized = IsAuthorized'
  { -- | Specifies the requested action to be authorized. For example, is the
    -- principal authorized to perform this action on the resource?
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
    -- | Specifies the principal for which the authorization decision is to be
    -- made.
    principal :: Prelude.Maybe EntityIdentifier,
    -- | Specifies the resource for which the authorization decision is to be
    -- made.
    resource :: Prelude.Maybe EntityIdentifier,
    -- | Specifies the ID of the policy store. Policies in this policy store will
    -- be used to make an authorization decision for the input.
    policyStoreId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IsAuthorized' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'isAuthorized_action' - Specifies the requested action to be authorized. For example, is the
-- principal authorized to perform this action on the resource?
--
-- 'context', 'isAuthorized_context' - Specifies additional context that can be used to make more granular
-- authorization decisions.
--
-- 'entities', 'isAuthorized_entities' - Specifies the list of resources and principals and their associated
-- attributes that Verified Permissions can examine when evaluating the
-- policies.
--
-- You can include only principal and resource entities in this parameter;
-- you can\'t include actions. You must specify actions in the schema.
--
-- 'principal', 'isAuthorized_principal' - Specifies the principal for which the authorization decision is to be
-- made.
--
-- 'resource', 'isAuthorized_resource' - Specifies the resource for which the authorization decision is to be
-- made.
--
-- 'policyStoreId', 'isAuthorized_policyStoreId' - Specifies the ID of the policy store. Policies in this policy store will
-- be used to make an authorization decision for the input.
newIsAuthorized ::
  -- | 'policyStoreId'
  Prelude.Text ->
  IsAuthorized
newIsAuthorized pPolicyStoreId_ =
  IsAuthorized'
    { action = Prelude.Nothing,
      context = Prelude.Nothing,
      entities = Prelude.Nothing,
      principal = Prelude.Nothing,
      resource = Prelude.Nothing,
      policyStoreId = pPolicyStoreId_
    }

-- | Specifies the requested action to be authorized. For example, is the
-- principal authorized to perform this action on the resource?
isAuthorized_action :: Lens.Lens' IsAuthorized (Prelude.Maybe ActionIdentifier)
isAuthorized_action = Lens.lens (\IsAuthorized' {action} -> action) (\s@IsAuthorized' {} a -> s {action = a} :: IsAuthorized)

-- | Specifies additional context that can be used to make more granular
-- authorization decisions.
isAuthorized_context :: Lens.Lens' IsAuthorized (Prelude.Maybe ContextDefinition)
isAuthorized_context = Lens.lens (\IsAuthorized' {context} -> context) (\s@IsAuthorized' {} a -> s {context = a} :: IsAuthorized)

-- | Specifies the list of resources and principals and their associated
-- attributes that Verified Permissions can examine when evaluating the
-- policies.
--
-- You can include only principal and resource entities in this parameter;
-- you can\'t include actions. You must specify actions in the schema.
isAuthorized_entities :: Lens.Lens' IsAuthorized (Prelude.Maybe EntitiesDefinition)
isAuthorized_entities = Lens.lens (\IsAuthorized' {entities} -> entities) (\s@IsAuthorized' {} a -> s {entities = a} :: IsAuthorized)

-- | Specifies the principal for which the authorization decision is to be
-- made.
isAuthorized_principal :: Lens.Lens' IsAuthorized (Prelude.Maybe EntityIdentifier)
isAuthorized_principal = Lens.lens (\IsAuthorized' {principal} -> principal) (\s@IsAuthorized' {} a -> s {principal = a} :: IsAuthorized)

-- | Specifies the resource for which the authorization decision is to be
-- made.
isAuthorized_resource :: Lens.Lens' IsAuthorized (Prelude.Maybe EntityIdentifier)
isAuthorized_resource = Lens.lens (\IsAuthorized' {resource} -> resource) (\s@IsAuthorized' {} a -> s {resource = a} :: IsAuthorized)

-- | Specifies the ID of the policy store. Policies in this policy store will
-- be used to make an authorization decision for the input.
isAuthorized_policyStoreId :: Lens.Lens' IsAuthorized Prelude.Text
isAuthorized_policyStoreId = Lens.lens (\IsAuthorized' {policyStoreId} -> policyStoreId) (\s@IsAuthorized' {} a -> s {policyStoreId = a} :: IsAuthorized)

instance Core.AWSRequest IsAuthorized where
  type AWSResponse IsAuthorized = IsAuthorizedResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          IsAuthorizedResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "decision")
            Prelude.<*> ( x
                            Data..?> "determiningPolicies"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "errors" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable IsAuthorized where
  hashWithSalt _salt IsAuthorized' {..} =
    _salt
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` context
      `Prelude.hashWithSalt` entities
      `Prelude.hashWithSalt` principal
      `Prelude.hashWithSalt` resource
      `Prelude.hashWithSalt` policyStoreId

instance Prelude.NFData IsAuthorized where
  rnf IsAuthorized' {..} =
    Prelude.rnf action
      `Prelude.seq` Prelude.rnf context
      `Prelude.seq` Prelude.rnf entities
      `Prelude.seq` Prelude.rnf principal
      `Prelude.seq` Prelude.rnf resource
      `Prelude.seq` Prelude.rnf policyStoreId

instance Data.ToHeaders IsAuthorized where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "VerifiedPermissions.IsAuthorized" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON IsAuthorized where
  toJSON IsAuthorized' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("action" Data..=) Prelude.<$> action,
            ("context" Data..=) Prelude.<$> context,
            ("entities" Data..=) Prelude.<$> entities,
            ("principal" Data..=) Prelude.<$> principal,
            ("resource" Data..=) Prelude.<$> resource,
            Prelude.Just
              ("policyStoreId" Data..= policyStoreId)
          ]
      )

instance Data.ToPath IsAuthorized where
  toPath = Prelude.const "/"

instance Data.ToQuery IsAuthorized where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newIsAuthorizedResponse' smart constructor.
data IsAuthorizedResponse = IsAuthorizedResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An authorization decision that indicates if the authorization request
    -- should be allowed or denied.
    decision :: Decision,
    -- | The list of determining policies used to make the authorization
    -- decision. For example, if there are two matching policies, where one is
    -- a forbid and the other is a permit, then the forbid policy will be the
    -- determining policy. In the case of multiple matching permit policies
    -- then there would be multiple determining policies. In the case that no
    -- policies match, and hence the response is DENY, there would be no
    -- determining policies.
    determiningPolicies :: [DeterminingPolicyItem],
    -- | Errors that occurred while making an authorization decision, for
    -- example, a policy references an Entity or entity Attribute that does not
    -- exist in the slice.
    errors :: [EvaluationErrorItem]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IsAuthorizedResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'isAuthorizedResponse_httpStatus' - The response's http status code.
--
-- 'decision', 'isAuthorizedResponse_decision' - An authorization decision that indicates if the authorization request
-- should be allowed or denied.
--
-- 'determiningPolicies', 'isAuthorizedResponse_determiningPolicies' - The list of determining policies used to make the authorization
-- decision. For example, if there are two matching policies, where one is
-- a forbid and the other is a permit, then the forbid policy will be the
-- determining policy. In the case of multiple matching permit policies
-- then there would be multiple determining policies. In the case that no
-- policies match, and hence the response is DENY, there would be no
-- determining policies.
--
-- 'errors', 'isAuthorizedResponse_errors' - Errors that occurred while making an authorization decision, for
-- example, a policy references an Entity or entity Attribute that does not
-- exist in the slice.
newIsAuthorizedResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'decision'
  Decision ->
  IsAuthorizedResponse
newIsAuthorizedResponse pHttpStatus_ pDecision_ =
  IsAuthorizedResponse'
    { httpStatus = pHttpStatus_,
      decision = pDecision_,
      determiningPolicies = Prelude.mempty,
      errors = Prelude.mempty
    }

-- | The response's http status code.
isAuthorizedResponse_httpStatus :: Lens.Lens' IsAuthorizedResponse Prelude.Int
isAuthorizedResponse_httpStatus = Lens.lens (\IsAuthorizedResponse' {httpStatus} -> httpStatus) (\s@IsAuthorizedResponse' {} a -> s {httpStatus = a} :: IsAuthorizedResponse)

-- | An authorization decision that indicates if the authorization request
-- should be allowed or denied.
isAuthorizedResponse_decision :: Lens.Lens' IsAuthorizedResponse Decision
isAuthorizedResponse_decision = Lens.lens (\IsAuthorizedResponse' {decision} -> decision) (\s@IsAuthorizedResponse' {} a -> s {decision = a} :: IsAuthorizedResponse)

-- | The list of determining policies used to make the authorization
-- decision. For example, if there are two matching policies, where one is
-- a forbid and the other is a permit, then the forbid policy will be the
-- determining policy. In the case of multiple matching permit policies
-- then there would be multiple determining policies. In the case that no
-- policies match, and hence the response is DENY, there would be no
-- determining policies.
isAuthorizedResponse_determiningPolicies :: Lens.Lens' IsAuthorizedResponse [DeterminingPolicyItem]
isAuthorizedResponse_determiningPolicies = Lens.lens (\IsAuthorizedResponse' {determiningPolicies} -> determiningPolicies) (\s@IsAuthorizedResponse' {} a -> s {determiningPolicies = a} :: IsAuthorizedResponse) Prelude.. Lens.coerced

-- | Errors that occurred while making an authorization decision, for
-- example, a policy references an Entity or entity Attribute that does not
-- exist in the slice.
isAuthorizedResponse_errors :: Lens.Lens' IsAuthorizedResponse [EvaluationErrorItem]
isAuthorizedResponse_errors = Lens.lens (\IsAuthorizedResponse' {errors} -> errors) (\s@IsAuthorizedResponse' {} a -> s {errors = a} :: IsAuthorizedResponse) Prelude.. Lens.coerced

instance Prelude.NFData IsAuthorizedResponse where
  rnf IsAuthorizedResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf decision
      `Prelude.seq` Prelude.rnf determiningPolicies
      `Prelude.seq` Prelude.rnf errors
