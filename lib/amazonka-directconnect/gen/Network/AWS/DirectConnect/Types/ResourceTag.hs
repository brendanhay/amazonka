{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.ResourceTag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectConnect.Types.ResourceTag
  ( ResourceTag (..)
  -- * Smart constructor
  , mkResourceTag
  -- * Lenses
  , rtResourceArn
  , rtTags
  ) where

import qualified Network.AWS.DirectConnect.Types.ResourceArn as Types
import qualified Network.AWS.DirectConnect.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a tag associated with an AWS Direct Connect resource.
--
-- /See:/ 'mkResourceTag' smart constructor.
data ResourceTag = ResourceTag'
  { resourceArn :: Core.Maybe Types.ResourceArn
    -- ^ The Amazon Resource Name (ARN) of the resource.
  , tags :: Core.Maybe (Core.NonEmpty Types.Tag)
    -- ^ The tags.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceTag' value with any optional fields omitted.
mkResourceTag
    :: ResourceTag
mkResourceTag
  = ResourceTag'{resourceArn = Core.Nothing, tags = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the resource.
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtResourceArn :: Lens.Lens' ResourceTag (Core.Maybe Types.ResourceArn)
rtResourceArn = Lens.field @"resourceArn"
{-# INLINEABLE rtResourceArn #-}
{-# DEPRECATED resourceArn "Use generic-lens or generic-optics with 'resourceArn' instead"  #-}

-- | The tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtTags :: Lens.Lens' ResourceTag (Core.Maybe (Core.NonEmpty Types.Tag))
rtTags = Lens.field @"tags"
{-# INLINEABLE rtTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromJSON ResourceTag where
        parseJSON
          = Core.withObject "ResourceTag" Core.$
              \ x ->
                ResourceTag' Core.<$>
                  (x Core..:? "resourceArn") Core.<*> x Core..:? "tags"
