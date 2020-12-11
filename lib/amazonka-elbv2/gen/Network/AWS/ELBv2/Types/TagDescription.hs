-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.TagDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.TagDescription
  ( TagDescription (..),

    -- * Smart constructor
    mkTagDescription,

    -- * Lenses
    tdResourceARN,
    tdTags,
  )
where

import Network.AWS.ELBv2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The tags associated with a resource.
--
-- /See:/ 'mkTagDescription' smart constructor.
data TagDescription = TagDescription'
  { resourceARN ::
      Lude.Maybe Lude.Text,
    tags :: Lude.Maybe (Lude.NonEmpty Tag)
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
-- * 'resourceARN' - The Amazon Resource Name (ARN) of the resource.
-- * 'tags' - Information about the tags.
mkTagDescription ::
  TagDescription
mkTagDescription =
  TagDescription' {resourceARN = Lude.Nothing, tags = Lude.Nothing}

-- | The Amazon Resource Name (ARN) of the resource.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdResourceARN :: Lens.Lens' TagDescription (Lude.Maybe Lude.Text)
tdResourceARN = Lens.lens (resourceARN :: TagDescription -> Lude.Maybe Lude.Text) (\s a -> s {resourceARN = a} :: TagDescription)
{-# DEPRECATED tdResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | Information about the tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdTags :: Lens.Lens' TagDescription (Lude.Maybe (Lude.NonEmpty Tag))
tdTags = Lens.lens (tags :: TagDescription -> Lude.Maybe (Lude.NonEmpty Tag)) (\s a -> s {tags = a} :: TagDescription)
{-# DEPRECATED tdTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML TagDescription where
  parseXML x =
    TagDescription'
      Lude.<$> (x Lude..@? "ResourceArn")
      Lude.<*> ( x Lude..@? "Tags" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLNonEmpty "member")
               )
