{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.TagDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELBv2.Types.TagDescription
  ( TagDescription (..)
  -- * Smart constructor
  , mkTagDescription
  -- * Lenses
  , tdResourceArn
  , tdTags
  ) where

import qualified Network.AWS.ELBv2.Types.ResourceArn as Types
import qualified Network.AWS.ELBv2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The tags associated with a resource.
--
-- /See:/ 'mkTagDescription' smart constructor.
data TagDescription = TagDescription'
  { resourceArn :: Core.Maybe Types.ResourceArn
    -- ^ The Amazon Resource Name (ARN) of the resource.
  , tags :: Core.Maybe (Core.NonEmpty Types.Tag)
    -- ^ Information about the tags.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TagDescription' value with any optional fields omitted.
mkTagDescription
    :: TagDescription
mkTagDescription
  = TagDescription'{resourceArn = Core.Nothing, tags = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the resource.
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdResourceArn :: Lens.Lens' TagDescription (Core.Maybe Types.ResourceArn)
tdResourceArn = Lens.field @"resourceArn"
{-# INLINEABLE tdResourceArn #-}
{-# DEPRECATED resourceArn "Use generic-lens or generic-optics with 'resourceArn' instead"  #-}

-- | Information about the tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdTags :: Lens.Lens' TagDescription (Core.Maybe (Core.NonEmpty Types.Tag))
tdTags = Lens.field @"tags"
{-# INLINEABLE tdTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromXML TagDescription where
        parseXML x
          = TagDescription' Core.<$>
              (x Core..@? "ResourceArn") Core.<*>
                x Core..@? "Tags" Core..<@> Core.parseXMLNonEmpty "member"
