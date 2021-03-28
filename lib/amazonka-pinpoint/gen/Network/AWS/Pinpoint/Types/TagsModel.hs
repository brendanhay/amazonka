{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.TagsModel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.TagsModel
  ( TagsModel (..)
  -- * Smart constructor
  , mkTagsModel
  -- * Lenses
  , tmTags
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the tags (keys and values) for an application, campaign, message template, or segment.
--
-- /See:/ 'mkTagsModel' smart constructor.
newtype TagsModel = TagsModel'
  { tags :: Core.HashMap Core.Text Core.Text
    -- ^ A string-to-string map of key-value pairs that defines the tags for an application, campaign, message template, or segment. Each of these resources can have a maximum of 50 tags.
--
-- Each tag consists of a required tag key and an associated tag value. The maximum length of a tag key is 128 characters. The maximum length of a tag value is 256 characters.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'TagsModel' value with any optional fields omitted.
mkTagsModel
    :: TagsModel
mkTagsModel = TagsModel'{tags = Core.mempty}

-- | A string-to-string map of key-value pairs that defines the tags for an application, campaign, message template, or segment. Each of these resources can have a maximum of 50 tags.
--
-- Each tag consists of a required tag key and an associated tag value. The maximum length of a tag key is 128 characters. The maximum length of a tag value is 256 characters.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmTags :: Lens.Lens' TagsModel (Core.HashMap Core.Text Core.Text)
tmTags = Lens.field @"tags"
{-# INLINEABLE tmTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromJSON TagsModel where
        toJSON TagsModel{..}
          = Core.object (Core.catMaybes [Core.Just ("tags" Core..= tags)])

instance Core.FromJSON TagsModel where
        parseJSON
          = Core.withObject "TagsModel" Core.$
              \ x -> TagsModel' Core.<$> (x Core..:? "tags" Core..!= Core.mempty)
