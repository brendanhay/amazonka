{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.TagKeyOnly
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.TagKeyOnly
  ( TagKeyOnly (..),

    -- * Smart constructor
    mkTagKeyOnly,

    -- * Lenses
    tkoKey,
  )
where

import qualified Network.AWS.ELB.Internal as Types
import qualified Network.AWS.ELB.Types.TagKey as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The key of a tag.
--
-- /See:/ 'mkTagKeyOnly' smart constructor.
newtype TagKeyOnly = TagKeyOnly'
  { -- | The name of the key.
    key :: Core.Maybe Types.TagKey
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'TagKeyOnly' value with any optional fields omitted.
mkTagKeyOnly ::
  TagKeyOnly
mkTagKeyOnly = TagKeyOnly' {key = Core.Nothing}

-- | The name of the key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tkoKey :: Lens.Lens' TagKeyOnly (Core.Maybe Types.TagKey)
tkoKey = Lens.field @"key"
{-# DEPRECATED tkoKey "Use generic-lens or generic-optics with 'key' instead." #-}
