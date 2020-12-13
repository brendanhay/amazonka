{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroupsTagging.Types.ResourceTagMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroupsTagging.Types.ResourceTagMapping
  ( ResourceTagMapping (..),

    -- * Smart constructor
    mkResourceTagMapping,

    -- * Lenses
    rtmComplianceDetails,
    rtmResourceARN,
    rtmTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.ResourceGroupsTagging.Types.ComplianceDetails
import Network.AWS.ResourceGroupsTagging.Types.Tag

-- | A list of resource ARNs and the tags (keys and values) that are associated with each.
--
-- /See:/ 'mkResourceTagMapping' smart constructor.
data ResourceTagMapping = ResourceTagMapping'
  { -- | Information that shows whether a resource is compliant with the effective tag policy, including details on any noncompliant tag keys.
    complianceDetails :: Lude.Maybe ComplianceDetails,
    -- | The ARN of the resource.
    resourceARN :: Lude.Maybe Lude.Text,
    -- | The tags that have been applied to one or more AWS resources.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceTagMapping' with the minimum fields required to make a request.
--
-- * 'complianceDetails' - Information that shows whether a resource is compliant with the effective tag policy, including details on any noncompliant tag keys.
-- * 'resourceARN' - The ARN of the resource.
-- * 'tags' - The tags that have been applied to one or more AWS resources.
mkResourceTagMapping ::
  ResourceTagMapping
mkResourceTagMapping =
  ResourceTagMapping'
    { complianceDetails = Lude.Nothing,
      resourceARN = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | Information that shows whether a resource is compliant with the effective tag policy, including details on any noncompliant tag keys.
--
-- /Note:/ Consider using 'complianceDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtmComplianceDetails :: Lens.Lens' ResourceTagMapping (Lude.Maybe ComplianceDetails)
rtmComplianceDetails = Lens.lens (complianceDetails :: ResourceTagMapping -> Lude.Maybe ComplianceDetails) (\s a -> s {complianceDetails = a} :: ResourceTagMapping)
{-# DEPRECATED rtmComplianceDetails "Use generic-lens or generic-optics with 'complianceDetails' instead." #-}

-- | The ARN of the resource.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtmResourceARN :: Lens.Lens' ResourceTagMapping (Lude.Maybe Lude.Text)
rtmResourceARN = Lens.lens (resourceARN :: ResourceTagMapping -> Lude.Maybe Lude.Text) (\s a -> s {resourceARN = a} :: ResourceTagMapping)
{-# DEPRECATED rtmResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | The tags that have been applied to one or more AWS resources.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtmTags :: Lens.Lens' ResourceTagMapping (Lude.Maybe [Tag])
rtmTags = Lens.lens (tags :: ResourceTagMapping -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: ResourceTagMapping)
{-# DEPRECATED rtmTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON ResourceTagMapping where
  parseJSON =
    Lude.withObject
      "ResourceTagMapping"
      ( \x ->
          ResourceTagMapping'
            Lude.<$> (x Lude..:? "ComplianceDetails")
            Lude.<*> (x Lude..:? "ResourceARN")
            Lude.<*> (x Lude..:? "Tags" Lude..!= Lude.mempty)
      )
