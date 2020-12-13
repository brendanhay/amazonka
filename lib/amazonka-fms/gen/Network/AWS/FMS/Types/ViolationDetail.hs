{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.ViolationDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.ViolationDetail
  ( ViolationDetail (..),

    -- * Smart constructor
    mkViolationDetail,

    -- * Lenses
    vdResourceId,
    vdResourceType,
    vdPolicyId,
    vdResourceTags,
    vdResourceDescription,
    vdResourceViolations,
    vdMemberAccount,
  )
where

import Network.AWS.FMS.Types.ResourceViolation
import Network.AWS.FMS.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Violations for a resource based on the specified AWS Firewall Manager policy and AWS account.
--
-- /See:/ 'mkViolationDetail' smart constructor.
data ViolationDetail = ViolationDetail'
  { -- | The resource ID that the violation details were requested for.
    resourceId :: Lude.Text,
    -- | The resource type that the violation details were requested for.
    resourceType :: Lude.Text,
    -- | The ID of the AWS Firewall Manager policy that the violation details were requested for.
    policyId :: Lude.Text,
    -- | The @ResourceTag@ objects associated with the resource.
    resourceTags :: Lude.Maybe [Tag],
    -- | Brief description for the requested resource.
    resourceDescription :: Lude.Maybe Lude.Text,
    -- | List of violations for the requested resource.
    resourceViolations :: [ResourceViolation],
    -- | The AWS account that the violation details were requested for.
    memberAccount :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ViolationDetail' with the minimum fields required to make a request.
--
-- * 'resourceId' - The resource ID that the violation details were requested for.
-- * 'resourceType' - The resource type that the violation details were requested for.
-- * 'policyId' - The ID of the AWS Firewall Manager policy that the violation details were requested for.
-- * 'resourceTags' - The @ResourceTag@ objects associated with the resource.
-- * 'resourceDescription' - Brief description for the requested resource.
-- * 'resourceViolations' - List of violations for the requested resource.
-- * 'memberAccount' - The AWS account that the violation details were requested for.
mkViolationDetail ::
  -- | 'resourceId'
  Lude.Text ->
  -- | 'resourceType'
  Lude.Text ->
  -- | 'policyId'
  Lude.Text ->
  -- | 'memberAccount'
  Lude.Text ->
  ViolationDetail
mkViolationDetail
  pResourceId_
  pResourceType_
  pPolicyId_
  pMemberAccount_ =
    ViolationDetail'
      { resourceId = pResourceId_,
        resourceType = pResourceType_,
        policyId = pPolicyId_,
        resourceTags = Lude.Nothing,
        resourceDescription = Lude.Nothing,
        resourceViolations = Lude.mempty,
        memberAccount = pMemberAccount_
      }

-- | The resource ID that the violation details were requested for.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdResourceId :: Lens.Lens' ViolationDetail Lude.Text
vdResourceId = Lens.lens (resourceId :: ViolationDetail -> Lude.Text) (\s a -> s {resourceId = a} :: ViolationDetail)
{-# DEPRECATED vdResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The resource type that the violation details were requested for.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdResourceType :: Lens.Lens' ViolationDetail Lude.Text
vdResourceType = Lens.lens (resourceType :: ViolationDetail -> Lude.Text) (\s a -> s {resourceType = a} :: ViolationDetail)
{-# DEPRECATED vdResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The ID of the AWS Firewall Manager policy that the violation details were requested for.
--
-- /Note:/ Consider using 'policyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdPolicyId :: Lens.Lens' ViolationDetail Lude.Text
vdPolicyId = Lens.lens (policyId :: ViolationDetail -> Lude.Text) (\s a -> s {policyId = a} :: ViolationDetail)
{-# DEPRECATED vdPolicyId "Use generic-lens or generic-optics with 'policyId' instead." #-}

-- | The @ResourceTag@ objects associated with the resource.
--
-- /Note:/ Consider using 'resourceTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdResourceTags :: Lens.Lens' ViolationDetail (Lude.Maybe [Tag])
vdResourceTags = Lens.lens (resourceTags :: ViolationDetail -> Lude.Maybe [Tag]) (\s a -> s {resourceTags = a} :: ViolationDetail)
{-# DEPRECATED vdResourceTags "Use generic-lens or generic-optics with 'resourceTags' instead." #-}

-- | Brief description for the requested resource.
--
-- /Note:/ Consider using 'resourceDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdResourceDescription :: Lens.Lens' ViolationDetail (Lude.Maybe Lude.Text)
vdResourceDescription = Lens.lens (resourceDescription :: ViolationDetail -> Lude.Maybe Lude.Text) (\s a -> s {resourceDescription = a} :: ViolationDetail)
{-# DEPRECATED vdResourceDescription "Use generic-lens or generic-optics with 'resourceDescription' instead." #-}

-- | List of violations for the requested resource.
--
-- /Note:/ Consider using 'resourceViolations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdResourceViolations :: Lens.Lens' ViolationDetail [ResourceViolation]
vdResourceViolations = Lens.lens (resourceViolations :: ViolationDetail -> [ResourceViolation]) (\s a -> s {resourceViolations = a} :: ViolationDetail)
{-# DEPRECATED vdResourceViolations "Use generic-lens or generic-optics with 'resourceViolations' instead." #-}

-- | The AWS account that the violation details were requested for.
--
-- /Note:/ Consider using 'memberAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdMemberAccount :: Lens.Lens' ViolationDetail Lude.Text
vdMemberAccount = Lens.lens (memberAccount :: ViolationDetail -> Lude.Text) (\s a -> s {memberAccount = a} :: ViolationDetail)
{-# DEPRECATED vdMemberAccount "Use generic-lens or generic-optics with 'memberAccount' instead." #-}

instance Lude.FromJSON ViolationDetail where
  parseJSON =
    Lude.withObject
      "ViolationDetail"
      ( \x ->
          ViolationDetail'
            Lude.<$> (x Lude..: "ResourceId")
            Lude.<*> (x Lude..: "ResourceType")
            Lude.<*> (x Lude..: "PolicyId")
            Lude.<*> (x Lude..:? "ResourceTags" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ResourceDescription")
            Lude.<*> (x Lude..:? "ResourceViolations" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "MemberAccount")
      )
