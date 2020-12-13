{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types.ResourceTag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudTrail.Types.ResourceTag
  ( ResourceTag (..),

    -- * Smart constructor
    mkResourceTag,

    -- * Lenses
    rtResourceId,
    rtTagsList,
  )
where

import Network.AWS.CloudTrail.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A resource tag.
--
-- /See:/ 'mkResourceTag' smart constructor.
data ResourceTag = ResourceTag'
  { -- | Specifies the ARN of the resource.
    resourceId :: Lude.Maybe Lude.Text,
    -- | A list of tags.
    tagsList :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceTag' with the minimum fields required to make a request.
--
-- * 'resourceId' - Specifies the ARN of the resource.
-- * 'tagsList' - A list of tags.
mkResourceTag ::
  ResourceTag
mkResourceTag =
  ResourceTag' {resourceId = Lude.Nothing, tagsList = Lude.Nothing}

-- | Specifies the ARN of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtResourceId :: Lens.Lens' ResourceTag (Lude.Maybe Lude.Text)
rtResourceId = Lens.lens (resourceId :: ResourceTag -> Lude.Maybe Lude.Text) (\s a -> s {resourceId = a} :: ResourceTag)
{-# DEPRECATED rtResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | A list of tags.
--
-- /Note:/ Consider using 'tagsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtTagsList :: Lens.Lens' ResourceTag (Lude.Maybe [Tag])
rtTagsList = Lens.lens (tagsList :: ResourceTag -> Lude.Maybe [Tag]) (\s a -> s {tagsList = a} :: ResourceTag)
{-# DEPRECATED rtTagsList "Use generic-lens or generic-optics with 'tagsList' instead." #-}

instance Lude.FromJSON ResourceTag where
  parseJSON =
    Lude.withObject
      "ResourceTag"
      ( \x ->
          ResourceTag'
            Lude.<$> (x Lude..:? "ResourceId")
            Lude.<*> (x Lude..:? "TagsList" Lude..!= Lude.mempty)
      )
