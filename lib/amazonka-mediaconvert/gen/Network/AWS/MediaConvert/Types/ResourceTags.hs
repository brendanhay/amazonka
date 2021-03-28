{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ResourceTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.ResourceTags
  ( ResourceTags (..)
  -- * Smart constructor
  , mkResourceTags
  -- * Lenses
  , rtArn
  , rtTags
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The Amazon Resource Name (ARN) and tags for an AWS Elemental MediaConvert resource.
--
-- /See:/ 'mkResourceTags' smart constructor.
data ResourceTags = ResourceTags'
  { arn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the resource.
  , tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ The tags for the resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceTags' value with any optional fields omitted.
mkResourceTags
    :: ResourceTags
mkResourceTags
  = ResourceTags'{arn = Core.Nothing, tags = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the resource.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtArn :: Lens.Lens' ResourceTags (Core.Maybe Core.Text)
rtArn = Lens.field @"arn"
{-# INLINEABLE rtArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The tags for the resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtTags :: Lens.Lens' ResourceTags (Core.Maybe (Core.HashMap Core.Text Core.Text))
rtTags = Lens.field @"tags"
{-# INLINEABLE rtTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromJSON ResourceTags where
        parseJSON
          = Core.withObject "ResourceTags" Core.$
              \ x ->
                ResourceTags' Core.<$>
                  (x Core..:? "arn") Core.<*> x Core..:? "tags"
