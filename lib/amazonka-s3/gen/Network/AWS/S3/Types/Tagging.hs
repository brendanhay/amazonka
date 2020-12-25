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
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.Tag as Types

-- | Container for @TagSet@ elements.
--
-- /See:/ 'mkTagging' smart constructor.
newtype Tagging = Tagging'
  { -- | A collection for a set of tags
    tagSet :: [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Tagging' value with any optional fields omitted.
mkTagging ::
  Tagging
mkTagging = Tagging' {tagSet = Core.mempty}

-- | A collection for a set of tags
--
-- /Note:/ Consider using 'tagSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTagSet :: Lens.Lens' Tagging [Types.Tag]
tTagSet = Lens.field @"tagSet"
{-# DEPRECATED tTagSet "Use generic-lens or generic-optics with 'tagSet' instead." #-}

instance Core.ToXML Tagging where
  toXML Tagging {..} =
    Core.toXMLNode "TagSet" (Core.toXMLList "Tag" tagSet)
