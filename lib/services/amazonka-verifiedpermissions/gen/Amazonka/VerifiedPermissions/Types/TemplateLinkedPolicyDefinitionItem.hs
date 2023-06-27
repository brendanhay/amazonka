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
-- Module      : Amazonka.VerifiedPermissions.Types.TemplateLinkedPolicyDefinitionItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VerifiedPermissions.Types.TemplateLinkedPolicyDefinitionItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.VerifiedPermissions.Types.EntityIdentifier

-- | Contains information about a policy created by instantiating a policy
-- template.
--
-- This
--
-- /See:/ 'newTemplateLinkedPolicyDefinitionItem' smart constructor.
data TemplateLinkedPolicyDefinitionItem = TemplateLinkedPolicyDefinitionItem'
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
-- Create a value of 'TemplateLinkedPolicyDefinitionItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'principal', 'templateLinkedPolicyDefinitionItem_principal' - The principal associated with this template-linked policy. Verified
-- Permissions substitutes this principal for the @?principal@ placeholder
-- in the policy template when it evaluates an authorization request.
--
-- 'resource', 'templateLinkedPolicyDefinitionItem_resource' - The resource associated with this template-linked policy. Verified
-- Permissions substitutes this resource for the @?resource@ placeholder in
-- the policy template when it evaluates an authorization request.
--
-- 'policyTemplateId', 'templateLinkedPolicyDefinitionItem_policyTemplateId' - The unique identifier of the policy template used to create this policy.
newTemplateLinkedPolicyDefinitionItem ::
  -- | 'policyTemplateId'
  Prelude.Text ->
  TemplateLinkedPolicyDefinitionItem
newTemplateLinkedPolicyDefinitionItem
  pPolicyTemplateId_ =
    TemplateLinkedPolicyDefinitionItem'
      { principal =
          Prelude.Nothing,
        resource = Prelude.Nothing,
        policyTemplateId = pPolicyTemplateId_
      }

-- | The principal associated with this template-linked policy. Verified
-- Permissions substitutes this principal for the @?principal@ placeholder
-- in the policy template when it evaluates an authorization request.
templateLinkedPolicyDefinitionItem_principal :: Lens.Lens' TemplateLinkedPolicyDefinitionItem (Prelude.Maybe EntityIdentifier)
templateLinkedPolicyDefinitionItem_principal = Lens.lens (\TemplateLinkedPolicyDefinitionItem' {principal} -> principal) (\s@TemplateLinkedPolicyDefinitionItem' {} a -> s {principal = a} :: TemplateLinkedPolicyDefinitionItem)

-- | The resource associated with this template-linked policy. Verified
-- Permissions substitutes this resource for the @?resource@ placeholder in
-- the policy template when it evaluates an authorization request.
templateLinkedPolicyDefinitionItem_resource :: Lens.Lens' TemplateLinkedPolicyDefinitionItem (Prelude.Maybe EntityIdentifier)
templateLinkedPolicyDefinitionItem_resource = Lens.lens (\TemplateLinkedPolicyDefinitionItem' {resource} -> resource) (\s@TemplateLinkedPolicyDefinitionItem' {} a -> s {resource = a} :: TemplateLinkedPolicyDefinitionItem)

-- | The unique identifier of the policy template used to create this policy.
templateLinkedPolicyDefinitionItem_policyTemplateId :: Lens.Lens' TemplateLinkedPolicyDefinitionItem Prelude.Text
templateLinkedPolicyDefinitionItem_policyTemplateId = Lens.lens (\TemplateLinkedPolicyDefinitionItem' {policyTemplateId} -> policyTemplateId) (\s@TemplateLinkedPolicyDefinitionItem' {} a -> s {policyTemplateId = a} :: TemplateLinkedPolicyDefinitionItem)

instance
  Data.FromJSON
    TemplateLinkedPolicyDefinitionItem
  where
  parseJSON =
    Data.withObject
      "TemplateLinkedPolicyDefinitionItem"
      ( \x ->
          TemplateLinkedPolicyDefinitionItem'
            Prelude.<$> (x Data..:? "principal")
            Prelude.<*> (x Data..:? "resource")
            Prelude.<*> (x Data..: "policyTemplateId")
      )

instance
  Prelude.Hashable
    TemplateLinkedPolicyDefinitionItem
  where
  hashWithSalt
    _salt
    TemplateLinkedPolicyDefinitionItem' {..} =
      _salt
        `Prelude.hashWithSalt` principal
        `Prelude.hashWithSalt` resource
        `Prelude.hashWithSalt` policyTemplateId

instance
  Prelude.NFData
    TemplateLinkedPolicyDefinitionItem
  where
  rnf TemplateLinkedPolicyDefinitionItem' {..} =
    Prelude.rnf principal
      `Prelude.seq` Prelude.rnf resource
      `Prelude.seq` Prelude.rnf policyTemplateId
