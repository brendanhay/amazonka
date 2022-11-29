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
-- Module      : Amazonka.FMS.Types.ViolationDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.ViolationDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FMS.Types.ResourceViolation
import Amazonka.FMS.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Violations for a resource based on the specified Firewall Manager policy
-- and Amazon Web Services account.
--
-- /See:/ 'newViolationDetail' smart constructor.
data ViolationDetail = ViolationDetail'
  { -- | The @ResourceTag@ objects associated with the resource.
    resourceTags :: Prelude.Maybe [Tag],
    -- | Brief description for the requested resource.
    resourceDescription :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Firewall Manager policy that the violation details were
    -- requested for.
    policyId :: Prelude.Text,
    -- | The Amazon Web Services account that the violation details were
    -- requested for.
    memberAccount :: Prelude.Text,
    -- | The resource ID that the violation details were requested for.
    resourceId :: Prelude.Text,
    -- | The resource type that the violation details were requested for.
    resourceType :: Prelude.Text,
    -- | List of violations for the requested resource.
    resourceViolations :: [ResourceViolation]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ViolationDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceTags', 'violationDetail_resourceTags' - The @ResourceTag@ objects associated with the resource.
--
-- 'resourceDescription', 'violationDetail_resourceDescription' - Brief description for the requested resource.
--
-- 'policyId', 'violationDetail_policyId' - The ID of the Firewall Manager policy that the violation details were
-- requested for.
--
-- 'memberAccount', 'violationDetail_memberAccount' - The Amazon Web Services account that the violation details were
-- requested for.
--
-- 'resourceId', 'violationDetail_resourceId' - The resource ID that the violation details were requested for.
--
-- 'resourceType', 'violationDetail_resourceType' - The resource type that the violation details were requested for.
--
-- 'resourceViolations', 'violationDetail_resourceViolations' - List of violations for the requested resource.
newViolationDetail ::
  -- | 'policyId'
  Prelude.Text ->
  -- | 'memberAccount'
  Prelude.Text ->
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'resourceType'
  Prelude.Text ->
  ViolationDetail
newViolationDetail
  pPolicyId_
  pMemberAccount_
  pResourceId_
  pResourceType_ =
    ViolationDetail'
      { resourceTags = Prelude.Nothing,
        resourceDescription = Prelude.Nothing,
        policyId = pPolicyId_,
        memberAccount = pMemberAccount_,
        resourceId = pResourceId_,
        resourceType = pResourceType_,
        resourceViolations = Prelude.mempty
      }

-- | The @ResourceTag@ objects associated with the resource.
violationDetail_resourceTags :: Lens.Lens' ViolationDetail (Prelude.Maybe [Tag])
violationDetail_resourceTags = Lens.lens (\ViolationDetail' {resourceTags} -> resourceTags) (\s@ViolationDetail' {} a -> s {resourceTags = a} :: ViolationDetail) Prelude.. Lens.mapping Lens.coerced

-- | Brief description for the requested resource.
violationDetail_resourceDescription :: Lens.Lens' ViolationDetail (Prelude.Maybe Prelude.Text)
violationDetail_resourceDescription = Lens.lens (\ViolationDetail' {resourceDescription} -> resourceDescription) (\s@ViolationDetail' {} a -> s {resourceDescription = a} :: ViolationDetail)

-- | The ID of the Firewall Manager policy that the violation details were
-- requested for.
violationDetail_policyId :: Lens.Lens' ViolationDetail Prelude.Text
violationDetail_policyId = Lens.lens (\ViolationDetail' {policyId} -> policyId) (\s@ViolationDetail' {} a -> s {policyId = a} :: ViolationDetail)

-- | The Amazon Web Services account that the violation details were
-- requested for.
violationDetail_memberAccount :: Lens.Lens' ViolationDetail Prelude.Text
violationDetail_memberAccount = Lens.lens (\ViolationDetail' {memberAccount} -> memberAccount) (\s@ViolationDetail' {} a -> s {memberAccount = a} :: ViolationDetail)

-- | The resource ID that the violation details were requested for.
violationDetail_resourceId :: Lens.Lens' ViolationDetail Prelude.Text
violationDetail_resourceId = Lens.lens (\ViolationDetail' {resourceId} -> resourceId) (\s@ViolationDetail' {} a -> s {resourceId = a} :: ViolationDetail)

-- | The resource type that the violation details were requested for.
violationDetail_resourceType :: Lens.Lens' ViolationDetail Prelude.Text
violationDetail_resourceType = Lens.lens (\ViolationDetail' {resourceType} -> resourceType) (\s@ViolationDetail' {} a -> s {resourceType = a} :: ViolationDetail)

-- | List of violations for the requested resource.
violationDetail_resourceViolations :: Lens.Lens' ViolationDetail [ResourceViolation]
violationDetail_resourceViolations = Lens.lens (\ViolationDetail' {resourceViolations} -> resourceViolations) (\s@ViolationDetail' {} a -> s {resourceViolations = a} :: ViolationDetail) Prelude.. Lens.coerced

instance Core.FromJSON ViolationDetail where
  parseJSON =
    Core.withObject
      "ViolationDetail"
      ( \x ->
          ViolationDetail'
            Prelude.<$> (x Core..:? "ResourceTags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ResourceDescription")
            Prelude.<*> (x Core..: "PolicyId")
            Prelude.<*> (x Core..: "MemberAccount")
            Prelude.<*> (x Core..: "ResourceId")
            Prelude.<*> (x Core..: "ResourceType")
            Prelude.<*> ( x Core..:? "ResourceViolations"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ViolationDetail where
  hashWithSalt _salt ViolationDetail' {..} =
    _salt `Prelude.hashWithSalt` resourceTags
      `Prelude.hashWithSalt` resourceDescription
      `Prelude.hashWithSalt` policyId
      `Prelude.hashWithSalt` memberAccount
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` resourceViolations

instance Prelude.NFData ViolationDetail where
  rnf ViolationDetail' {..} =
    Prelude.rnf resourceTags
      `Prelude.seq` Prelude.rnf resourceDescription
      `Prelude.seq` Prelude.rnf policyId
      `Prelude.seq` Prelude.rnf memberAccount
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf resourceViolations
