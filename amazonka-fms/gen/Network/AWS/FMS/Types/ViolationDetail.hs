{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.FMS.Types.ViolationDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.ViolationDetail where

import Network.AWS.FMS.Types.ResourceViolation
import Network.AWS.FMS.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Violations for a resource based on the specified AWS Firewall Manager
-- policy and AWS account.
--
-- /See:/ 'newViolationDetail' smart constructor.
data ViolationDetail = ViolationDetail'
  { -- | Brief description for the requested resource.
    resourceDescription :: Prelude.Maybe Prelude.Text,
    -- | The @ResourceTag@ objects associated with the resource.
    resourceTags :: Prelude.Maybe [Tag],
    -- | The ID of the AWS Firewall Manager policy that the violation details
    -- were requested for.
    policyId :: Prelude.Text,
    -- | The AWS account that the violation details were requested for.
    memberAccount :: Prelude.Text,
    -- | The resource ID that the violation details were requested for.
    resourceId :: Prelude.Text,
    -- | The resource type that the violation details were requested for.
    resourceType :: Prelude.Text,
    -- | List of violations for the requested resource.
    resourceViolations :: [ResourceViolation]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ViolationDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceDescription', 'violationDetail_resourceDescription' - Brief description for the requested resource.
--
-- 'resourceTags', 'violationDetail_resourceTags' - The @ResourceTag@ objects associated with the resource.
--
-- 'policyId', 'violationDetail_policyId' - The ID of the AWS Firewall Manager policy that the violation details
-- were requested for.
--
-- 'memberAccount', 'violationDetail_memberAccount' - The AWS account that the violation details were requested for.
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
      { resourceDescription =
          Prelude.Nothing,
        resourceTags = Prelude.Nothing,
        policyId = pPolicyId_,
        memberAccount = pMemberAccount_,
        resourceId = pResourceId_,
        resourceType = pResourceType_,
        resourceViolations = Prelude.mempty
      }

-- | Brief description for the requested resource.
violationDetail_resourceDescription :: Lens.Lens' ViolationDetail (Prelude.Maybe Prelude.Text)
violationDetail_resourceDescription = Lens.lens (\ViolationDetail' {resourceDescription} -> resourceDescription) (\s@ViolationDetail' {} a -> s {resourceDescription = a} :: ViolationDetail)

-- | The @ResourceTag@ objects associated with the resource.
violationDetail_resourceTags :: Lens.Lens' ViolationDetail (Prelude.Maybe [Tag])
violationDetail_resourceTags = Lens.lens (\ViolationDetail' {resourceTags} -> resourceTags) (\s@ViolationDetail' {} a -> s {resourceTags = a} :: ViolationDetail) Prelude.. Lens.mapping Prelude._Coerce

-- | The ID of the AWS Firewall Manager policy that the violation details
-- were requested for.
violationDetail_policyId :: Lens.Lens' ViolationDetail Prelude.Text
violationDetail_policyId = Lens.lens (\ViolationDetail' {policyId} -> policyId) (\s@ViolationDetail' {} a -> s {policyId = a} :: ViolationDetail)

-- | The AWS account that the violation details were requested for.
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
violationDetail_resourceViolations = Lens.lens (\ViolationDetail' {resourceViolations} -> resourceViolations) (\s@ViolationDetail' {} a -> s {resourceViolations = a} :: ViolationDetail) Prelude.. Prelude._Coerce

instance Prelude.FromJSON ViolationDetail where
  parseJSON =
    Prelude.withObject
      "ViolationDetail"
      ( \x ->
          ViolationDetail'
            Prelude.<$> (x Prelude..:? "ResourceDescription")
            Prelude.<*> ( x Prelude..:? "ResourceTags"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..: "PolicyId")
            Prelude.<*> (x Prelude..: "MemberAccount")
            Prelude.<*> (x Prelude..: "ResourceId")
            Prelude.<*> (x Prelude..: "ResourceType")
            Prelude.<*> ( x Prelude..:? "ResourceViolations"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ViolationDetail

instance Prelude.NFData ViolationDetail
