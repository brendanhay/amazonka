{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Tagging
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Tagging
  ( Tagging (..),

    -- * Smart constructor
    mkTagging,

    -- * Lenses
    tTagSet,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Tag

-- | Container for @TagSet@ elements.
--
-- /See:/ 'mkTagging' smart constructor.
newtype Tagging = Tagging' {tagSet :: [Tag]}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Tagging' with the minimum fields required to make a request.
--
-- * 'tagSet' - A collection for a set of tags
mkTagging ::
  Tagging
mkTagging = Tagging' {tagSet = Lude.mempty}

-- | A collection for a set of tags
--
-- /Note:/ Consider using 'tagSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTagSet :: Lens.Lens' Tagging [Tag]
tTagSet = Lens.lens (tagSet :: Tagging -> [Tag]) (\s a -> s {tagSet = a} :: Tagging)
{-# DEPRECATED tTagSet "Use generic-lens or generic-optics with 'tagSet' instead." #-}

instance Lude.ToXML Tagging where
  toXML Tagging' {..} =
    Lude.mconcat ["TagSet" Lude.@= Lude.toXMLList "Tag" tagSet]
