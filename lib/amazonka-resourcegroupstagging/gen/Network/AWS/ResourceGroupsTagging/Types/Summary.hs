-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroupsTagging.Types.Summary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroupsTagging.Types.Summary
  ( Summary (..),

    -- * Smart constructor
    mkSummary,

    -- * Lenses
    sTargetId,
    sLastUpdated,
    sResourceType,
    sNonCompliantResources,
    sTargetIdType,
    sRegion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.ResourceGroupsTagging.Types.TargetIdType

-- | A count of noncompliant resources.
--
-- /See:/ 'mkSummary' smart constructor.
data Summary = Summary'
  { targetId :: Lude.Maybe Lude.Text,
    lastUpdated :: Lude.Maybe Lude.Text,
    resourceType :: Lude.Maybe Lude.Text,
    nonCompliantResources :: Lude.Maybe Lude.Integer,
    targetIdType :: Lude.Maybe TargetIdType,
    region :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Summary' with the minimum fields required to make a request.
--
-- * 'lastUpdated' - The timestamp that shows when this summary was generated in this Region.
-- * 'nonCompliantResources' - The count of noncompliant resources.
-- * 'region' - The AWS Region that the summary applies to.
-- * 'resourceType' - The AWS resource type.
-- * 'targetId' - The account identifier or the root identifier of the organization. If you don't know the root ID, you can call the AWS Organizations <http://docs.aws.amazon.com/organizations/latest/APIReference/API_ListRoots.html ListRoots> API.
-- * 'targetIdType' - Whether the target is an account, an OU, or the organization root.
mkSummary ::
  Summary
mkSummary =
  Summary'
    { targetId = Lude.Nothing,
      lastUpdated = Lude.Nothing,
      resourceType = Lude.Nothing,
      nonCompliantResources = Lude.Nothing,
      targetIdType = Lude.Nothing,
      region = Lude.Nothing
    }

-- | The account identifier or the root identifier of the organization. If you don't know the root ID, you can call the AWS Organizations <http://docs.aws.amazon.com/organizations/latest/APIReference/API_ListRoots.html ListRoots> API.
--
-- /Note:/ Consider using 'targetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTargetId :: Lens.Lens' Summary (Lude.Maybe Lude.Text)
sTargetId = Lens.lens (targetId :: Summary -> Lude.Maybe Lude.Text) (\s a -> s {targetId = a} :: Summary)
{-# DEPRECATED sTargetId "Use generic-lens or generic-optics with 'targetId' instead." #-}

-- | The timestamp that shows when this summary was generated in this Region.
--
-- /Note:/ Consider using 'lastUpdated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sLastUpdated :: Lens.Lens' Summary (Lude.Maybe Lude.Text)
sLastUpdated = Lens.lens (lastUpdated :: Summary -> Lude.Maybe Lude.Text) (\s a -> s {lastUpdated = a} :: Summary)
{-# DEPRECATED sLastUpdated "Use generic-lens or generic-optics with 'lastUpdated' instead." #-}

-- | The AWS resource type.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sResourceType :: Lens.Lens' Summary (Lude.Maybe Lude.Text)
sResourceType = Lens.lens (resourceType :: Summary -> Lude.Maybe Lude.Text) (\s a -> s {resourceType = a} :: Summary)
{-# DEPRECATED sResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The count of noncompliant resources.
--
-- /Note:/ Consider using 'nonCompliantResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sNonCompliantResources :: Lens.Lens' Summary (Lude.Maybe Lude.Integer)
sNonCompliantResources = Lens.lens (nonCompliantResources :: Summary -> Lude.Maybe Lude.Integer) (\s a -> s {nonCompliantResources = a} :: Summary)
{-# DEPRECATED sNonCompliantResources "Use generic-lens or generic-optics with 'nonCompliantResources' instead." #-}

-- | Whether the target is an account, an OU, or the organization root.
--
-- /Note:/ Consider using 'targetIdType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTargetIdType :: Lens.Lens' Summary (Lude.Maybe TargetIdType)
sTargetIdType = Lens.lens (targetIdType :: Summary -> Lude.Maybe TargetIdType) (\s a -> s {targetIdType = a} :: Summary)
{-# DEPRECATED sTargetIdType "Use generic-lens or generic-optics with 'targetIdType' instead." #-}

-- | The AWS Region that the summary applies to.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sRegion :: Lens.Lens' Summary (Lude.Maybe Lude.Text)
sRegion = Lens.lens (region :: Summary -> Lude.Maybe Lude.Text) (\s a -> s {region = a} :: Summary)
{-# DEPRECATED sRegion "Use generic-lens or generic-optics with 'region' instead." #-}

instance Lude.FromJSON Summary where
  parseJSON =
    Lude.withObject
      "Summary"
      ( \x ->
          Summary'
            Lude.<$> (x Lude..:? "TargetId")
            Lude.<*> (x Lude..:? "LastUpdated")
            Lude.<*> (x Lude..:? "ResourceType")
            Lude.<*> (x Lude..:? "NonCompliantResources")
            Lude.<*> (x Lude..:? "TargetIdType")
            Lude.<*> (x Lude..:? "Region")
      )
