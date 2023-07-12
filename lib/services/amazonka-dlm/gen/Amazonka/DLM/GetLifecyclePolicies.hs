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
-- Module      : Amazonka.DLM.GetLifecyclePolicies
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets summary information about all or the specified data lifecycle
-- policies.
--
-- To get complete information about a policy, use GetLifecyclePolicy.
module Amazonka.DLM.GetLifecyclePolicies
  ( -- * Creating a Request
    GetLifecyclePolicies (..),
    newGetLifecyclePolicies,

    -- * Request Lenses
    getLifecyclePolicies_policyIds,
    getLifecyclePolicies_resourceTypes,
    getLifecyclePolicies_state,
    getLifecyclePolicies_tagsToAdd,
    getLifecyclePolicies_targetTags,

    -- * Destructuring the Response
    GetLifecyclePoliciesResponse (..),
    newGetLifecyclePoliciesResponse,

    -- * Response Lenses
    getLifecyclePoliciesResponse_policies,
    getLifecyclePoliciesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DLM.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetLifecyclePolicies' smart constructor.
data GetLifecyclePolicies = GetLifecyclePolicies'
  { -- | The identifiers of the data lifecycle policies.
    policyIds :: Prelude.Maybe [Prelude.Text],
    -- | The resource type.
    resourceTypes :: Prelude.Maybe (Prelude.NonEmpty ResourceTypeValues),
    -- | The activation state.
    state :: Prelude.Maybe GettablePolicyStateValues,
    -- | The tags to add to objects created by the policy.
    --
    -- Tags are strings in the format @key=value@.
    --
    -- These user-defined tags are added in addition to the Amazon Web
    -- Services-added lifecycle tags.
    tagsToAdd :: Prelude.Maybe [Prelude.Text],
    -- | The target tag for a policy.
    --
    -- Tags are strings in the format @key=value@.
    targetTags :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLifecyclePolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyIds', 'getLifecyclePolicies_policyIds' - The identifiers of the data lifecycle policies.
--
-- 'resourceTypes', 'getLifecyclePolicies_resourceTypes' - The resource type.
--
-- 'state', 'getLifecyclePolicies_state' - The activation state.
--
-- 'tagsToAdd', 'getLifecyclePolicies_tagsToAdd' - The tags to add to objects created by the policy.
--
-- Tags are strings in the format @key=value@.
--
-- These user-defined tags are added in addition to the Amazon Web
-- Services-added lifecycle tags.
--
-- 'targetTags', 'getLifecyclePolicies_targetTags' - The target tag for a policy.
--
-- Tags are strings in the format @key=value@.
newGetLifecyclePolicies ::
  GetLifecyclePolicies
newGetLifecyclePolicies =
  GetLifecyclePolicies'
    { policyIds = Prelude.Nothing,
      resourceTypes = Prelude.Nothing,
      state = Prelude.Nothing,
      tagsToAdd = Prelude.Nothing,
      targetTags = Prelude.Nothing
    }

-- | The identifiers of the data lifecycle policies.
getLifecyclePolicies_policyIds :: Lens.Lens' GetLifecyclePolicies (Prelude.Maybe [Prelude.Text])
getLifecyclePolicies_policyIds = Lens.lens (\GetLifecyclePolicies' {policyIds} -> policyIds) (\s@GetLifecyclePolicies' {} a -> s {policyIds = a} :: GetLifecyclePolicies) Prelude.. Lens.mapping Lens.coerced

-- | The resource type.
getLifecyclePolicies_resourceTypes :: Lens.Lens' GetLifecyclePolicies (Prelude.Maybe (Prelude.NonEmpty ResourceTypeValues))
getLifecyclePolicies_resourceTypes = Lens.lens (\GetLifecyclePolicies' {resourceTypes} -> resourceTypes) (\s@GetLifecyclePolicies' {} a -> s {resourceTypes = a} :: GetLifecyclePolicies) Prelude.. Lens.mapping Lens.coerced

-- | The activation state.
getLifecyclePolicies_state :: Lens.Lens' GetLifecyclePolicies (Prelude.Maybe GettablePolicyStateValues)
getLifecyclePolicies_state = Lens.lens (\GetLifecyclePolicies' {state} -> state) (\s@GetLifecyclePolicies' {} a -> s {state = a} :: GetLifecyclePolicies)

-- | The tags to add to objects created by the policy.
--
-- Tags are strings in the format @key=value@.
--
-- These user-defined tags are added in addition to the Amazon Web
-- Services-added lifecycle tags.
getLifecyclePolicies_tagsToAdd :: Lens.Lens' GetLifecyclePolicies (Prelude.Maybe [Prelude.Text])
getLifecyclePolicies_tagsToAdd = Lens.lens (\GetLifecyclePolicies' {tagsToAdd} -> tagsToAdd) (\s@GetLifecyclePolicies' {} a -> s {tagsToAdd = a} :: GetLifecyclePolicies) Prelude.. Lens.mapping Lens.coerced

-- | The target tag for a policy.
--
-- Tags are strings in the format @key=value@.
getLifecyclePolicies_targetTags :: Lens.Lens' GetLifecyclePolicies (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
getLifecyclePolicies_targetTags = Lens.lens (\GetLifecyclePolicies' {targetTags} -> targetTags) (\s@GetLifecyclePolicies' {} a -> s {targetTags = a} :: GetLifecyclePolicies) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest GetLifecyclePolicies where
  type
    AWSResponse GetLifecyclePolicies =
      GetLifecyclePoliciesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLifecyclePoliciesResponse'
            Prelude.<$> (x Data..?> "Policies" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLifecyclePolicies where
  hashWithSalt _salt GetLifecyclePolicies' {..} =
    _salt
      `Prelude.hashWithSalt` policyIds
      `Prelude.hashWithSalt` resourceTypes
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` tagsToAdd
      `Prelude.hashWithSalt` targetTags

instance Prelude.NFData GetLifecyclePolicies where
  rnf GetLifecyclePolicies' {..} =
    Prelude.rnf policyIds
      `Prelude.seq` Prelude.rnf resourceTypes
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf tagsToAdd
      `Prelude.seq` Prelude.rnf targetTags

instance Data.ToHeaders GetLifecyclePolicies where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetLifecyclePolicies where
  toPath = Prelude.const "/policies"

instance Data.ToQuery GetLifecyclePolicies where
  toQuery GetLifecyclePolicies' {..} =
    Prelude.mconcat
      [ "policyIds"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> policyIds),
        "resourceTypes"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> resourceTypes
            ),
        "state" Data.=: state,
        "tagsToAdd"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> tagsToAdd),
        "targetTags"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> targetTags)
      ]

-- | /See:/ 'newGetLifecyclePoliciesResponse' smart constructor.
data GetLifecyclePoliciesResponse = GetLifecyclePoliciesResponse'
  { -- | Summary information about the lifecycle policies.
    policies :: Prelude.Maybe [LifecyclePolicySummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLifecyclePoliciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policies', 'getLifecyclePoliciesResponse_policies' - Summary information about the lifecycle policies.
--
-- 'httpStatus', 'getLifecyclePoliciesResponse_httpStatus' - The response's http status code.
newGetLifecyclePoliciesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetLifecyclePoliciesResponse
newGetLifecyclePoliciesResponse pHttpStatus_ =
  GetLifecyclePoliciesResponse'
    { policies =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Summary information about the lifecycle policies.
getLifecyclePoliciesResponse_policies :: Lens.Lens' GetLifecyclePoliciesResponse (Prelude.Maybe [LifecyclePolicySummary])
getLifecyclePoliciesResponse_policies = Lens.lens (\GetLifecyclePoliciesResponse' {policies} -> policies) (\s@GetLifecyclePoliciesResponse' {} a -> s {policies = a} :: GetLifecyclePoliciesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getLifecyclePoliciesResponse_httpStatus :: Lens.Lens' GetLifecyclePoliciesResponse Prelude.Int
getLifecyclePoliciesResponse_httpStatus = Lens.lens (\GetLifecyclePoliciesResponse' {httpStatus} -> httpStatus) (\s@GetLifecyclePoliciesResponse' {} a -> s {httpStatus = a} :: GetLifecyclePoliciesResponse)

instance Prelude.NFData GetLifecyclePoliciesResponse where
  rnf GetLifecyclePoliciesResponse' {..} =
    Prelude.rnf policies
      `Prelude.seq` Prelude.rnf httpStatus
