-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.ResourceTag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.ResourceTag
  ( ResourceTag (..),

    -- * Smart constructor
    mkResourceTag,

    -- * Lenses
    rtResourceARN,
    rtTags,
  )
where

import Network.AWS.DirectConnect.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a tag associated with an AWS Direct Connect resource.
--
-- /See:/ 'mkResourceTag' smart constructor.
data ResourceTag = ResourceTag'
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

-- | Creates a value of 'ResourceTag' with the minimum fields required to make a request.
--
-- * 'resourceARN' - The Amazon Resource Name (ARN) of the resource.
-- * 'tags' - The tags.
mkResourceTag ::
  ResourceTag
mkResourceTag =
  ResourceTag' {resourceARN = Lude.Nothing, tags = Lude.Nothing}

-- | The Amazon Resource Name (ARN) of the resource.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtResourceARN :: Lens.Lens' ResourceTag (Lude.Maybe Lude.Text)
rtResourceARN = Lens.lens (resourceARN :: ResourceTag -> Lude.Maybe Lude.Text) (\s a -> s {resourceARN = a} :: ResourceTag)
{-# DEPRECATED rtResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | The tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtTags :: Lens.Lens' ResourceTag (Lude.Maybe (Lude.NonEmpty Tag))
rtTags = Lens.lens (tags :: ResourceTag -> Lude.Maybe (Lude.NonEmpty Tag)) (\s a -> s {tags = a} :: ResourceTag)
{-# DEPRECATED rtTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON ResourceTag where
  parseJSON =
    Lude.withObject
      "ResourceTag"
      ( \x ->
          ResourceTag'
            Lude.<$> (x Lude..:? "resourceArn") Lude.<*> (x Lude..:? "tags")
      )
