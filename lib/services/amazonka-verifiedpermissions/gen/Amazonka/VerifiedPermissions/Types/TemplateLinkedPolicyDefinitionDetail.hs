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
-- Module      : Amazonka.VerifiedPermissions.Types.TemplateLinkedPolicyDefinitionDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VerifiedPermissions.Types.TemplateLinkedPolicyDefinitionDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.VerifiedPermissions.Types.EntityIdentifier

-- | Contains information about a policy that was
--
-- created by instantiating a policy template.
--
-- This
--
-- /See:/ 'newTemplateLinkedPolicyDefinitionDetail' smart constructor.
data TemplateLinkedPolicyDefinitionDetail = TemplateLinkedPolicyDefinitionDetail'
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
-- Create a value of 'TemplateLinkedPolicyDefinitionDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'principal', 'templateLinkedPolicyDefinitionDetail_principal' - The principal associated with this template-linked policy. Verified
-- Permissions substitutes this principal for the @?principal@ placeholder
-- in the policy template when it evaluates an authorization request.
--
-- 'resource', 'templateLinkedPolicyDefinitionDetail_resource' - The resource associated with this template-linked policy. Verified
-- Permissions substitutes this resource for the @?resource@ placeholder in
-- the policy template when it evaluates an authorization request.
--
-- 'policyTemplateId', 'templateLinkedPolicyDefinitionDetail_policyTemplateId' - The unique identifier of the policy template used to create this policy.
newTemplateLinkedPolicyDefinitionDetail ::
  -- | 'policyTemplateId'
  Prelude.Text ->
  TemplateLinkedPolicyDefinitionDetail
newTemplateLinkedPolicyDefinitionDetail
  pPolicyTemplateId_ =
    TemplateLinkedPolicyDefinitionDetail'
      { principal =
          Prelude.Nothing,
        resource = Prelude.Nothing,
        policyTemplateId = pPolicyTemplateId_
      }

-- | The principal associated with this template-linked policy. Verified
-- Permissions substitutes this principal for the @?principal@ placeholder
-- in the policy template when it evaluates an authorization request.
templateLinkedPolicyDefinitionDetail_principal :: Lens.Lens' TemplateLinkedPolicyDefinitionDetail (Prelude.Maybe EntityIdentifier)
templateLinkedPolicyDefinitionDetail_principal = Lens.lens (\TemplateLinkedPolicyDefinitionDetail' {principal} -> principal) (\s@TemplateLinkedPolicyDefinitionDetail' {} a -> s {principal = a} :: TemplateLinkedPolicyDefinitionDetail)

-- | The resource associated with this template-linked policy. Verified
-- Permissions substitutes this resource for the @?resource@ placeholder in
-- the policy template when it evaluates an authorization request.
templateLinkedPolicyDefinitionDetail_resource :: Lens.Lens' TemplateLinkedPolicyDefinitionDetail (Prelude.Maybe EntityIdentifier)
templateLinkedPolicyDefinitionDetail_resource = Lens.lens (\TemplateLinkedPolicyDefinitionDetail' {resource} -> resource) (\s@TemplateLinkedPolicyDefinitionDetail' {} a -> s {resource = a} :: TemplateLinkedPolicyDefinitionDetail)

-- | The unique identifier of the policy template used to create this policy.
templateLinkedPolicyDefinitionDetail_policyTemplateId :: Lens.Lens' TemplateLinkedPolicyDefinitionDetail Prelude.Text
templateLinkedPolicyDefinitionDetail_policyTemplateId = Lens.lens (\TemplateLinkedPolicyDefinitionDetail' {policyTemplateId} -> policyTemplateId) (\s@TemplateLinkedPolicyDefinitionDetail' {} a -> s {policyTemplateId = a} :: TemplateLinkedPolicyDefinitionDetail)

instance
  Data.FromJSON
    TemplateLinkedPolicyDefinitionDetail
  where
  parseJSON =
    Data.withObject
      "TemplateLinkedPolicyDefinitionDetail"
      ( \x ->
          TemplateLinkedPolicyDefinitionDetail'
            Prelude.<$> (x Data..:? "principal")
            Prelude.<*> (x Data..:? "resource")
            Prelude.<*> (x Data..: "policyTemplateId")
      )

instance
  Prelude.Hashable
    TemplateLinkedPolicyDefinitionDetail
  where
  hashWithSalt
    _salt
    TemplateLinkedPolicyDefinitionDetail' {..} =
      _salt
        `Prelude.hashWithSalt` principal
        `Prelude.hashWithSalt` resource
        `Prelude.hashWithSalt` policyTemplateId

instance
  Prelude.NFData
    TemplateLinkedPolicyDefinitionDetail
  where
  rnf TemplateLinkedPolicyDefinitionDetail' {..} =
    Prelude.rnf principal
      `Prelude.seq` Prelude.rnf resource
      `Prelude.seq` Prelude.rnf policyTemplateId
