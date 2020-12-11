-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.TagListMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.TagListMessage
  ( TagListMessage (..),

    -- * Smart constructor
    mkTagListMessage,

    -- * Lenses
    tlmTagList,
  )
where

import Network.AWS.ElastiCache.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the output from the @AddTagsToResource@ , @ListTagsForResource@ , and @RemoveTagsFromResource@ operations.
--
-- /See:/ 'mkTagListMessage' smart constructor.
newtype TagListMessage = TagListMessage'
  { tagList ::
      Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TagListMessage' with the minimum fields required to make a request.
--
-- * 'tagList' - A list of cost allocation tags as key-value pairs.
mkTagListMessage ::
  TagListMessage
mkTagListMessage = TagListMessage' {tagList = Lude.Nothing}

-- | A list of cost allocation tags as key-value pairs.
--
-- /Note:/ Consider using 'tagList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tlmTagList :: Lens.Lens' TagListMessage (Lude.Maybe [Tag])
tlmTagList = Lens.lens (tagList :: TagListMessage -> Lude.Maybe [Tag]) (\s a -> s {tagList = a} :: TagListMessage)
{-# DEPRECATED tlmTagList "Use generic-lens or generic-optics with 'tagList' instead." #-}

instance Lude.FromXML TagListMessage where
  parseXML x =
    TagListMessage'
      Lude.<$> ( x Lude..@? "TagList" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "Tag")
               )
