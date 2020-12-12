{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.TagDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.TagDescription
  ( TagDescription (..),

    -- * Smart constructor
    mkTagDescription,

    -- * Lenses
    tdResourceId,
    tdResourceType,
    tdKey,
    tdPropagateAtLaunch,
    tdValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a tag for an Auto Scaling group.
--
-- /See:/ 'mkTagDescription' smart constructor.
data TagDescription = TagDescription'
  { resourceId :: Lude.Text,
    resourceType :: Lude.Text,
    key :: Lude.Text,
    propagateAtLaunch :: Lude.Bool,
    value :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TagDescription' with the minimum fields required to make a request.
--
-- * 'key' - The tag key.
-- * 'propagateAtLaunch' - Determines whether the tag is added to new instances as they are launched in the group.
-- * 'resourceId' - The name of the group.
-- * 'resourceType' - The type of resource. The only supported value is @auto-scaling-group@ .
-- * 'value' - The tag value.
mkTagDescription ::
  -- | 'resourceId'
  Lude.Text ->
  -- | 'resourceType'
  Lude.Text ->
  -- | 'key'
  Lude.Text ->
  -- | 'propagateAtLaunch'
  Lude.Bool ->
  -- | 'value'
  Lude.Text ->
  TagDescription
mkTagDescription
  pResourceId_
  pResourceType_
  pKey_
  pPropagateAtLaunch_
  pValue_ =
    TagDescription'
      { resourceId = pResourceId_,
        resourceType = pResourceType_,
        key = pKey_,
        propagateAtLaunch = pPropagateAtLaunch_,
        value = pValue_
      }

-- | The name of the group.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdResourceId :: Lens.Lens' TagDescription Lude.Text
tdResourceId = Lens.lens (resourceId :: TagDescription -> Lude.Text) (\s a -> s {resourceId = a} :: TagDescription)
{-# DEPRECATED tdResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The type of resource. The only supported value is @auto-scaling-group@ .
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdResourceType :: Lens.Lens' TagDescription Lude.Text
tdResourceType = Lens.lens (resourceType :: TagDescription -> Lude.Text) (\s a -> s {resourceType = a} :: TagDescription)
{-# DEPRECATED tdResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The tag key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdKey :: Lens.Lens' TagDescription Lude.Text
tdKey = Lens.lens (key :: TagDescription -> Lude.Text) (\s a -> s {key = a} :: TagDescription)
{-# DEPRECATED tdKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | Determines whether the tag is added to new instances as they are launched in the group.
--
-- /Note:/ Consider using 'propagateAtLaunch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdPropagateAtLaunch :: Lens.Lens' TagDescription Lude.Bool
tdPropagateAtLaunch = Lens.lens (propagateAtLaunch :: TagDescription -> Lude.Bool) (\s a -> s {propagateAtLaunch = a} :: TagDescription)
{-# DEPRECATED tdPropagateAtLaunch "Use generic-lens or generic-optics with 'propagateAtLaunch' instead." #-}

-- | The tag value.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdValue :: Lens.Lens' TagDescription Lude.Text
tdValue = Lens.lens (value :: TagDescription -> Lude.Text) (\s a -> s {value = a} :: TagDescription)
{-# DEPRECATED tdValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Lude.FromXML TagDescription where
  parseXML x =
    TagDescription'
      Lude.<$> (x Lude..@ "ResourceId")
      Lude.<*> (x Lude..@ "ResourceType")
      Lude.<*> (x Lude..@ "Key")
      Lude.<*> (x Lude..@ "PropagateAtLaunch")
      Lude.<*> (x Lude..@ "Value")
