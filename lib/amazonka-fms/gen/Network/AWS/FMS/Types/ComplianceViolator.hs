-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.ComplianceViolator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.ComplianceViolator
  ( ComplianceViolator (..),

    -- * Smart constructor
    mkComplianceViolator,

    -- * Lenses
    cvResourceId,
    cvResourceType,
    cvViolationReason,
  )
where

import Network.AWS.FMS.Types.ViolationReason
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details of the resource that is not protected by the policy.
--
-- /See:/ 'mkComplianceViolator' smart constructor.
data ComplianceViolator = ComplianceViolator'
  { resourceId ::
      Lude.Maybe Lude.Text,
    resourceType :: Lude.Maybe Lude.Text,
    violationReason :: Lude.Maybe ViolationReason
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ComplianceViolator' with the minimum fields required to make a request.
--
-- * 'resourceId' - The resource ID.
-- * 'resourceType' - The resource type. This is in the format shown in the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> . For example: @AWS::ElasticLoadBalancingV2::LoadBalancer@ , @AWS::CloudFront::Distribution@ , or @AWS::NetworkFirewall::FirewallPolicy@ .
-- * 'violationReason' - The reason that the resource is not protected by the policy.
mkComplianceViolator ::
  ComplianceViolator
mkComplianceViolator =
  ComplianceViolator'
    { resourceId = Lude.Nothing,
      resourceType = Lude.Nothing,
      violationReason = Lude.Nothing
    }

-- | The resource ID.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvResourceId :: Lens.Lens' ComplianceViolator (Lude.Maybe Lude.Text)
cvResourceId = Lens.lens (resourceId :: ComplianceViolator -> Lude.Maybe Lude.Text) (\s a -> s {resourceId = a} :: ComplianceViolator)
{-# DEPRECATED cvResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The resource type. This is in the format shown in the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> . For example: @AWS::ElasticLoadBalancingV2::LoadBalancer@ , @AWS::CloudFront::Distribution@ , or @AWS::NetworkFirewall::FirewallPolicy@ .
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvResourceType :: Lens.Lens' ComplianceViolator (Lude.Maybe Lude.Text)
cvResourceType = Lens.lens (resourceType :: ComplianceViolator -> Lude.Maybe Lude.Text) (\s a -> s {resourceType = a} :: ComplianceViolator)
{-# DEPRECATED cvResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The reason that the resource is not protected by the policy.
--
-- /Note:/ Consider using 'violationReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvViolationReason :: Lens.Lens' ComplianceViolator (Lude.Maybe ViolationReason)
cvViolationReason = Lens.lens (violationReason :: ComplianceViolator -> Lude.Maybe ViolationReason) (\s a -> s {violationReason = a} :: ComplianceViolator)
{-# DEPRECATED cvViolationReason "Use generic-lens or generic-optics with 'violationReason' instead." #-}

instance Lude.FromJSON ComplianceViolator where
  parseJSON =
    Lude.withObject
      "ComplianceViolator"
      ( \x ->
          ComplianceViolator'
            Lude.<$> (x Lude..:? "ResourceId")
            Lude.<*> (x Lude..:? "ResourceType")
            Lude.<*> (x Lude..:? "ViolationReason")
      )
