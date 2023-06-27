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
-- Module      : Amazonka.VerifiedPermissions.Types.PolicyFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VerifiedPermissions.Types.PolicyFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.VerifiedPermissions.Types.EntityReference
import Amazonka.VerifiedPermissions.Types.PolicyType

-- | Contains information about a filter to refine policies returned in a
-- query.
--
-- This data type is used as a response parameter for the
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_ListPolicies.html ListPolicies>
-- operation.
--
-- /See:/ 'newPolicyFilter' smart constructor.
data PolicyFilter = PolicyFilter'
  { -- | Filters the output to only template-linked policies that were
    -- instantiated from the specified policy template.
    policyTemplateId :: Prelude.Maybe Prelude.Text,
    -- | Filters the output to only policies of the specified type.
    policyType :: Prelude.Maybe PolicyType,
    -- | Filters the output to only policies that reference the specified
    -- principal.
    principal :: Prelude.Maybe EntityReference,
    -- | Filters the output to only policies that reference the specified
    -- resource.
    resource :: Prelude.Maybe EntityReference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PolicyFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyTemplateId', 'policyFilter_policyTemplateId' - Filters the output to only template-linked policies that were
-- instantiated from the specified policy template.
--
-- 'policyType', 'policyFilter_policyType' - Filters the output to only policies of the specified type.
--
-- 'principal', 'policyFilter_principal' - Filters the output to only policies that reference the specified
-- principal.
--
-- 'resource', 'policyFilter_resource' - Filters the output to only policies that reference the specified
-- resource.
newPolicyFilter ::
  PolicyFilter
newPolicyFilter =
  PolicyFilter'
    { policyTemplateId = Prelude.Nothing,
      policyType = Prelude.Nothing,
      principal = Prelude.Nothing,
      resource = Prelude.Nothing
    }

-- | Filters the output to only template-linked policies that were
-- instantiated from the specified policy template.
policyFilter_policyTemplateId :: Lens.Lens' PolicyFilter (Prelude.Maybe Prelude.Text)
policyFilter_policyTemplateId = Lens.lens (\PolicyFilter' {policyTemplateId} -> policyTemplateId) (\s@PolicyFilter' {} a -> s {policyTemplateId = a} :: PolicyFilter)

-- | Filters the output to only policies of the specified type.
policyFilter_policyType :: Lens.Lens' PolicyFilter (Prelude.Maybe PolicyType)
policyFilter_policyType = Lens.lens (\PolicyFilter' {policyType} -> policyType) (\s@PolicyFilter' {} a -> s {policyType = a} :: PolicyFilter)

-- | Filters the output to only policies that reference the specified
-- principal.
policyFilter_principal :: Lens.Lens' PolicyFilter (Prelude.Maybe EntityReference)
policyFilter_principal = Lens.lens (\PolicyFilter' {principal} -> principal) (\s@PolicyFilter' {} a -> s {principal = a} :: PolicyFilter)

-- | Filters the output to only policies that reference the specified
-- resource.
policyFilter_resource :: Lens.Lens' PolicyFilter (Prelude.Maybe EntityReference)
policyFilter_resource = Lens.lens (\PolicyFilter' {resource} -> resource) (\s@PolicyFilter' {} a -> s {resource = a} :: PolicyFilter)

instance Prelude.Hashable PolicyFilter where
  hashWithSalt _salt PolicyFilter' {..} =
    _salt
      `Prelude.hashWithSalt` policyTemplateId
      `Prelude.hashWithSalt` policyType
      `Prelude.hashWithSalt` principal
      `Prelude.hashWithSalt` resource

instance Prelude.NFData PolicyFilter where
  rnf PolicyFilter' {..} =
    Prelude.rnf policyTemplateId
      `Prelude.seq` Prelude.rnf policyType
      `Prelude.seq` Prelude.rnf principal
      `Prelude.seq` Prelude.rnf resource

instance Data.ToJSON PolicyFilter where
  toJSON PolicyFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("policyTemplateId" Data..=)
              Prelude.<$> policyTemplateId,
            ("policyType" Data..=) Prelude.<$> policyType,
            ("principal" Data..=) Prelude.<$> principal,
            ("resource" Data..=) Prelude.<$> resource
          ]
      )
