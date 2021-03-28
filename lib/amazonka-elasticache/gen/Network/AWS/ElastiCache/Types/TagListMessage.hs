{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.TagListMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElastiCache.Types.TagListMessage
  ( TagListMessage (..)
  -- * Smart constructor
  , mkTagListMessage
  -- * Lenses
  , tlmTagList
  ) where

import qualified Network.AWS.ElastiCache.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the output from the @AddTagsToResource@ , @ListTagsForResource@ , and @RemoveTagsFromResource@ operations.
--
-- /See:/ 'mkTagListMessage' smart constructor.
newtype TagListMessage = TagListMessage'
  { tagList :: Core.Maybe [Types.Tag]
    -- ^ A list of cost allocation tags as key-value pairs.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'TagListMessage' value with any optional fields omitted.
mkTagListMessage
    :: TagListMessage
mkTagListMessage = TagListMessage'{tagList = Core.Nothing}

-- | A list of cost allocation tags as key-value pairs.
--
-- /Note:/ Consider using 'tagList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tlmTagList :: Lens.Lens' TagListMessage (Core.Maybe [Types.Tag])
tlmTagList = Lens.field @"tagList"
{-# INLINEABLE tlmTagList #-}
{-# DEPRECATED tagList "Use generic-lens or generic-optics with 'tagList' instead"  #-}

instance Core.FromXML TagListMessage where
        parseXML x
          = TagListMessage' Core.<$>
              (x Core..@? "TagList" Core..<@> Core.parseXMLList "Tag")
