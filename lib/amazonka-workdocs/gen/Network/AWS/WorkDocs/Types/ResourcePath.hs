{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.ResourcePath
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkDocs.Types.ResourcePath
  ( ResourcePath (..)
  -- * Smart constructor
  , mkResourcePath
  -- * Lenses
  , rpComponents
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkDocs.Types.ResourcePathComponent as Types

-- | Describes the path information of a resource.
--
-- /See:/ 'mkResourcePath' smart constructor.
newtype ResourcePath = ResourcePath'
  { components :: Core.Maybe [Types.ResourcePathComponent]
    -- ^ The components of the resource path.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ResourcePath' value with any optional fields omitted.
mkResourcePath
    :: ResourcePath
mkResourcePath = ResourcePath'{components = Core.Nothing}

-- | The components of the resource path.
--
-- /Note:/ Consider using 'components' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpComponents :: Lens.Lens' ResourcePath (Core.Maybe [Types.ResourcePathComponent])
rpComponents = Lens.field @"components"
{-# INLINEABLE rpComponents #-}
{-# DEPRECATED components "Use generic-lens or generic-optics with 'components' instead"  #-}

instance Core.FromJSON ResourcePath where
        parseJSON
          = Core.withObject "ResourcePath" Core.$
              \ x -> ResourcePath' Core.<$> (x Core..:? "Components")
