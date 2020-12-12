{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.TagsModel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.TagsModel
  ( TagsModel (..),

    -- * Smart constructor
    mkTagsModel,

    -- * Lenses
    tmTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the tags (keys and values) for an application, campaign, message template, or segment.
--
-- /See:/ 'mkTagsModel' smart constructor.
newtype TagsModel = TagsModel'
  { tags ::
      Lude.HashMap Lude.Text (Lude.Text)
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TagsModel' with the minimum fields required to make a request.
--
-- * 'tags' - A string-to-string map of key-value pairs that defines the tags for an application, campaign, message template, or segment. Each of these resources can have a maximum of 50 tags.
--
-- Each tag consists of a required tag key and an associated tag value. The maximum length of a tag key is 128 characters. The maximum length of a tag value is 256 characters.
mkTagsModel ::
  TagsModel
mkTagsModel = TagsModel' {tags = Lude.mempty}

-- | A string-to-string map of key-value pairs that defines the tags for an application, campaign, message template, or segment. Each of these resources can have a maximum of 50 tags.
--
-- Each tag consists of a required tag key and an associated tag value. The maximum length of a tag key is 128 characters. The maximum length of a tag value is 256 characters.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmTags :: Lens.Lens' TagsModel (Lude.HashMap Lude.Text (Lude.Text))
tmTags = Lens.lens (tags :: TagsModel -> Lude.HashMap Lude.Text (Lude.Text)) (\s a -> s {tags = a} :: TagsModel)
{-# DEPRECATED tmTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON TagsModel where
  parseJSON =
    Lude.withObject
      "TagsModel"
      ( \x ->
          TagsModel' Lude.<$> (x Lude..:? "tags" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON TagsModel where
  toJSON TagsModel' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("tags" Lude..= tags)])
