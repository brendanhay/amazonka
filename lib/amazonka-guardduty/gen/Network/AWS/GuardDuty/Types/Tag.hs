{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.Tag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.Tag
  ( Tag (..)
  -- * Smart constructor
  , mkTag
  -- * Lenses
  , tKey
  , tValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about a tag associated with the EC2 instance.
--
-- /See:/ 'mkTag' smart constructor.
data Tag = Tag'
  { key :: Core.Maybe Core.Text
    -- ^ The EC2 instance tag key.
  , value :: Core.Maybe Core.Text
    -- ^ The EC2 instance tag value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Tag' value with any optional fields omitted.
mkTag
    :: Tag
mkTag = Tag'{key = Core.Nothing, value = Core.Nothing}

-- | The EC2 instance tag key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tKey :: Lens.Lens' Tag (Core.Maybe Core.Text)
tKey = Lens.field @"key"
{-# INLINEABLE tKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | The EC2 instance tag value.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tValue :: Lens.Lens' Tag (Core.Maybe Core.Text)
tValue = Lens.field @"value"
{-# INLINEABLE tValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromJSON Tag where
        parseJSON
          = Core.withObject "Tag" Core.$
              \ x -> Tag' Core.<$> (x Core..:? "key") Core.<*> x Core..:? "value"
