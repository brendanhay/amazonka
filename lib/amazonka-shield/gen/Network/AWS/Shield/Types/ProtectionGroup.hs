-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.ProtectionGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.ProtectionGroup
  ( ProtectionGroup (..),

    -- * Smart constructor
    mkProtectionGroup,

    -- * Lenses
    pgResourceType,
    pgProtectionGroupId,
    pgAggregation,
    pgPattern,
    pgMembers,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Shield.Types.ProtectedResourceType
import Network.AWS.Shield.Types.ProtectionGroupAggregation
import Network.AWS.Shield.Types.ProtectionGroupPattern

-- | A grouping of protected resources that you and AWS Shield Advanced can monitor as a collective. This resource grouping improves the accuracy of detection and reduces false positives.
--
-- /See:/ 'mkProtectionGroup' smart constructor.
data ProtectionGroup = ProtectionGroup'
  { resourceType ::
      Lude.Maybe ProtectedResourceType,
    protectionGroupId :: Lude.Text,
    aggregation :: ProtectionGroupAggregation,
    pattern' :: ProtectionGroupPattern,
    members :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProtectionGroup' with the minimum fields required to make a request.
--
-- * 'aggregation' - Defines how AWS Shield combines resource data for the group in order to detect, mitigate, and report events.
--
--
--     * Sum - Use the total traffic across the group. This is a good choice for most cases. Examples include Elastic IP addresses for EC2 instances that scale manually or automatically.
--
--
--     * Mean - Use the average of the traffic across the group. This is a good choice for resources that share traffic uniformly. Examples include accelerators and load balancers.
--
--
--     * Max - Use the highest traffic from each resource. This is useful for resources that don't share traffic and for resources that share that traffic in a non-uniform way. Examples include CloudFront distributions and origin resources for CloudFront distributions.
--
--
-- * 'members' - The Amazon Resource Names (ARNs) of the resources to include in the protection group. You must set this when you set @Pattern@ to @ARBITRARY@ and you must not set it for any other @Pattern@ setting.
-- * 'pattern'' - The criteria to use to choose the protected resources for inclusion in the group. You can include all resources that have protections, provide a list of resource Amazon Resource Names (ARNs), or include all resources of a specified resource type.
-- * 'protectionGroupId' - The name of the protection group. You use this to identify the protection group in lists and to manage the protection group, for example to update, delete, or describe it.
-- * 'resourceType' - The resource type to include in the protection group. All protected resources of this type are included in the protection group. You must set this when you set @Pattern@ to @BY_RESOURCE_TYPE@ and you must not set it for any other @Pattern@ setting.
mkProtectionGroup ::
  -- | 'protectionGroupId'
  Lude.Text ->
  -- | 'aggregation'
  ProtectionGroupAggregation ->
  -- | 'pattern''
  ProtectionGroupPattern ->
  ProtectionGroup
mkProtectionGroup pProtectionGroupId_ pAggregation_ pPattern_ =
  ProtectionGroup'
    { resourceType = Lude.Nothing,
      protectionGroupId = pProtectionGroupId_,
      aggregation = pAggregation_,
      pattern' = pPattern_,
      members = Lude.mempty
    }

-- | The resource type to include in the protection group. All protected resources of this type are included in the protection group. You must set this when you set @Pattern@ to @BY_RESOURCE_TYPE@ and you must not set it for any other @Pattern@ setting.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgResourceType :: Lens.Lens' ProtectionGroup (Lude.Maybe ProtectedResourceType)
pgResourceType = Lens.lens (resourceType :: ProtectionGroup -> Lude.Maybe ProtectedResourceType) (\s a -> s {resourceType = a} :: ProtectionGroup)
{-# DEPRECATED pgResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The name of the protection group. You use this to identify the protection group in lists and to manage the protection group, for example to update, delete, or describe it.
--
-- /Note:/ Consider using 'protectionGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgProtectionGroupId :: Lens.Lens' ProtectionGroup Lude.Text
pgProtectionGroupId = Lens.lens (protectionGroupId :: ProtectionGroup -> Lude.Text) (\s a -> s {protectionGroupId = a} :: ProtectionGroup)
{-# DEPRECATED pgProtectionGroupId "Use generic-lens or generic-optics with 'protectionGroupId' instead." #-}

-- | Defines how AWS Shield combines resource data for the group in order to detect, mitigate, and report events.
--
--
--     * Sum - Use the total traffic across the group. This is a good choice for most cases. Examples include Elastic IP addresses for EC2 instances that scale manually or automatically.
--
--
--     * Mean - Use the average of the traffic across the group. This is a good choice for resources that share traffic uniformly. Examples include accelerators and load balancers.
--
--
--     * Max - Use the highest traffic from each resource. This is useful for resources that don't share traffic and for resources that share that traffic in a non-uniform way. Examples include CloudFront distributions and origin resources for CloudFront distributions.
--
--
--
-- /Note:/ Consider using 'aggregation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgAggregation :: Lens.Lens' ProtectionGroup ProtectionGroupAggregation
pgAggregation = Lens.lens (aggregation :: ProtectionGroup -> ProtectionGroupAggregation) (\s a -> s {aggregation = a} :: ProtectionGroup)
{-# DEPRECATED pgAggregation "Use generic-lens or generic-optics with 'aggregation' instead." #-}

-- | The criteria to use to choose the protected resources for inclusion in the group. You can include all resources that have protections, provide a list of resource Amazon Resource Names (ARNs), or include all resources of a specified resource type.
--
-- /Note:/ Consider using 'pattern'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgPattern :: Lens.Lens' ProtectionGroup ProtectionGroupPattern
pgPattern = Lens.lens (pattern' :: ProtectionGroup -> ProtectionGroupPattern) (\s a -> s {pattern' = a} :: ProtectionGroup)
{-# DEPRECATED pgPattern "Use generic-lens or generic-optics with 'pattern'' instead." #-}

-- | The Amazon Resource Names (ARNs) of the resources to include in the protection group. You must set this when you set @Pattern@ to @ARBITRARY@ and you must not set it for any other @Pattern@ setting.
--
-- /Note:/ Consider using 'members' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgMembers :: Lens.Lens' ProtectionGroup [Lude.Text]
pgMembers = Lens.lens (members :: ProtectionGroup -> [Lude.Text]) (\s a -> s {members = a} :: ProtectionGroup)
{-# DEPRECATED pgMembers "Use generic-lens or generic-optics with 'members' instead." #-}

instance Lude.FromJSON ProtectionGroup where
  parseJSON =
    Lude.withObject
      "ProtectionGroup"
      ( \x ->
          ProtectionGroup'
            Lude.<$> (x Lude..:? "ResourceType")
            Lude.<*> (x Lude..: "ProtectionGroupId")
            Lude.<*> (x Lude..: "Aggregation")
            Lude.<*> (x Lude..: "Pattern")
            Lude.<*> (x Lude..:? "Members" Lude..!= Lude.mempty)
      )
