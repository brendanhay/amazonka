{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.VerifiedPermissions.Types.TemplateLinkedPolicyDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VerifiedPermissions.Types.TemplateLinkedPolicyDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.VerifiedPermissions.Types.EntityIdentifier

-- | Contains information about a policy created by instantiating a policy
-- template.
--
-- /See:/ 'newTemplateLinkedPolicyDefinition' smart constructor.
data TemplateLinkedPolicyDefinition = TemplateLinkedPolicyDefinition'
  { -- | The principal associated with this template-linked policy. Verified
    -- Permissions substitutes this principal for the @?principal@ placeholder
    -- in the policy template when it evaluates an authorization request.
    principal :: Prelude.Maybe EntityIdentifier,
    -- | The resource associated with this template-linked policy. Verified
    -- Permissions substitutes this resource for the @?resource@ placeholder in
    -- the policy template when it evaluates an authorization request.
    resource :: Prelude.Maybe EntityIdentifier,
    -- | The unique identifier of the policy template used to create this policy.
    policyTemplateId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TemplateLinkedPolicyDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'principal', 'templateLinkedPolicyDefinition_principal' - The principal associated with this template-linked policy. Verified
-- Permissions substitutes this principal for the @?principal@ placeholder
-- in the policy template when it evaluates an authorization request.
--
-- 'resource', 'templateLinkedPolicyDefinition_resource' - The resource associated with this template-linked policy. Verified
-- Permissions substitutes this resource for the @?resource@ placeholder in
-- the policy template when it evaluates an authorization request.
--
-- 'policyTemplateId', 'templateLinkedPolicyDefinition_policyTemplateId' - The unique identifier of the policy template used to create this policy.
newTemplateLinkedPolicyDefinition ::
  -- | 'policyTemplateId'
  Prelude.Text ->
  TemplateLinkedPolicyDefinition
newTemplateLinkedPolicyDefinition pPolicyTemplateId_ =
  TemplateLinkedPolicyDefinition'
    { principal =
        Prelude.Nothing,
      resource = Prelude.Nothing,
      policyTemplateId = pPolicyTemplateId_
    }

-- | The principal associated with this template-linked policy. Verified
-- Permissions substitutes this principal for the @?principal@ placeholder
-- in the policy template when it evaluates an authorization request.
templateLinkedPolicyDefinition_principal :: Lens.Lens' TemplateLinkedPolicyDefinition (Prelude.Maybe EntityIdentifier)
templateLinkedPolicyDefinition_principal = Lens.lens (\TemplateLinkedPolicyDefinition' {principal} -> principal) (\s@TemplateLinkedPolicyDefinition' {} a -> s {principal = a} :: TemplateLinkedPolicyDefinition)

-- | The resource associated with this template-linked policy. Verified
-- Permissions substitutes this resource for the @?resource@ placeholder in
-- the policy template when it evaluates an authorization request.
templateLinkedPolicyDefinition_resource :: Lens.Lens' TemplateLinkedPolicyDefinition (Prelude.Maybe EntityIdentifier)
templateLinkedPolicyDefinition_resource = Lens.lens (\TemplateLinkedPolicyDefinition' {resource} -> resource) (\s@TemplateLinkedPolicyDefinition' {} a -> s {resource = a} :: TemplateLinkedPolicyDefinition)

-- | The unique identifier of the policy template used to create this policy.
templateLinkedPolicyDefinition_policyTemplateId :: Lens.Lens' TemplateLinkedPolicyDefinition Prelude.Text
templateLinkedPolicyDefinition_policyTemplateId = Lens.lens (\TemplateLinkedPolicyDefinition' {policyTemplateId} -> policyTemplateId) (\s@TemplateLinkedPolicyDefinition' {} a -> s {policyTemplateId = a} :: TemplateLinkedPolicyDefinition)

instance
  Prelude.Hashable
    TemplateLinkedPolicyDefinition
  where
  hashWithSalt
    _salt
    TemplateLinkedPolicyDefinition' {..} =
      _salt
        `Prelude.hashWithSalt` principal
        `Prelude.hashWithSalt` resource
        `Prelude.hashWithSalt` policyTemplateId

instance
  Prelude.NFData
    TemplateLinkedPolicyDefinition
  where
  rnf TemplateLinkedPolicyDefinition' {..} =
    Prelude.rnf principal
      `Prelude.seq` Prelude.rnf resource
      `Prelude.seq` Prelude.rnf policyTemplateId

instance Data.ToJSON TemplateLinkedPolicyDefinition where
  toJSON TemplateLinkedPolicyDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("principal" Data..=) Prelude.<$> principal,
            ("resource" Data..=) Prelude.<$> resource,
            Prelude.Just
              ("policyTemplateId" Data..= policyTemplateId)
          ]
      )
