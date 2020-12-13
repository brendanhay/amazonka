{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TagDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TagDescription
  ( TagDescription (..),

    -- * Smart constructor
    mkTagDescription,

    -- * Lenses
    tdResourceId,
    tdResourceType,
    tdValue,
    tdKey,
  )
where

import Network.AWS.EC2.Types.ResourceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a tag.
--
-- /See:/ 'mkTagDescription' smart constructor.
data TagDescription = TagDescription'
  { -- | The ID of the resource.
    resourceId :: Lude.Text,
    -- | The resource type.
    resourceType :: ResourceType,
    -- | The tag value.
    value :: Lude.Text,
    -- | The tag key.
    key :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TagDescription' with the minimum fields required to make a request.
--
-- * 'resourceId' - The ID of the resource.
-- * 'resourceType' - The resource type.
-- * 'value' - The tag value.
-- * 'key' - The tag key.
mkTagDescription ::
  -- | 'resourceId'
  Lude.Text ->
  -- | 'resourceType'
  ResourceType ->
  -- | 'value'
  Lude.Text ->
  -- | 'key'
  Lude.Text ->
  TagDescription
mkTagDescription pResourceId_ pResourceType_ pValue_ pKey_ =
  TagDescription'
    { resourceId = pResourceId_,
      resourceType = pResourceType_,
      value = pValue_,
      key = pKey_
    }

-- | The ID of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdResourceId :: Lens.Lens' TagDescription Lude.Text
tdResourceId = Lens.lens (resourceId :: TagDescription -> Lude.Text) (\s a -> s {resourceId = a} :: TagDescription)
{-# DEPRECATED tdResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The resource type.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdResourceType :: Lens.Lens' TagDescription ResourceType
tdResourceType = Lens.lens (resourceType :: TagDescription -> ResourceType) (\s a -> s {resourceType = a} :: TagDescription)
{-# DEPRECATED tdResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The tag value.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdValue :: Lens.Lens' TagDescription Lude.Text
tdValue = Lens.lens (value :: TagDescription -> Lude.Text) (\s a -> s {value = a} :: TagDescription)
{-# DEPRECATED tdValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The tag key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdKey :: Lens.Lens' TagDescription Lude.Text
tdKey = Lens.lens (key :: TagDescription -> Lude.Text) (\s a -> s {key = a} :: TagDescription)
{-# DEPRECATED tdKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.FromXML TagDescription where
  parseXML x =
    TagDescription'
      Lude.<$> (x Lude..@ "resourceId")
      Lude.<*> (x Lude..@ "resourceType")
      Lude.<*> (x Lude..@ "value")
      Lude.<*> (x Lude..@ "key")
