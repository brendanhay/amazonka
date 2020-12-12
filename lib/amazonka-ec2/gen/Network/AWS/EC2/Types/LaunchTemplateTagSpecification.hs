{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateTagSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateTagSpecification
  ( LaunchTemplateTagSpecification (..),

    -- * Smart constructor
    mkLaunchTemplateTagSpecification,

    -- * Lenses
    lttsResourceType,
    lttsTags,
  )
where

import Network.AWS.EC2.Types.ResourceType
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The tag specification for the launch template.
--
-- /See:/ 'mkLaunchTemplateTagSpecification' smart constructor.
data LaunchTemplateTagSpecification = LaunchTemplateTagSpecification'
  { resourceType ::
      Lude.Maybe ResourceType,
    tags :: Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LaunchTemplateTagSpecification' with the minimum fields required to make a request.
--
-- * 'resourceType' - The type of resource.
-- * 'tags' - The tags for the resource.
mkLaunchTemplateTagSpecification ::
  LaunchTemplateTagSpecification
mkLaunchTemplateTagSpecification =
  LaunchTemplateTagSpecification'
    { resourceType = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The type of resource.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lttsResourceType :: Lens.Lens' LaunchTemplateTagSpecification (Lude.Maybe ResourceType)
lttsResourceType = Lens.lens (resourceType :: LaunchTemplateTagSpecification -> Lude.Maybe ResourceType) (\s a -> s {resourceType = a} :: LaunchTemplateTagSpecification)
{-# DEPRECATED lttsResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The tags for the resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lttsTags :: Lens.Lens' LaunchTemplateTagSpecification (Lude.Maybe [Tag])
lttsTags = Lens.lens (tags :: LaunchTemplateTagSpecification -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: LaunchTemplateTagSpecification)
{-# DEPRECATED lttsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML LaunchTemplateTagSpecification where
  parseXML x =
    LaunchTemplateTagSpecification'
      Lude.<$> (x Lude..@? "resourceType")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
