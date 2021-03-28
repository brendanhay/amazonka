{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ResizeInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.ResizeInfo
  ( ResizeInfo (..)
  -- * Smart constructor
  , mkResizeInfo
  -- * Lenses
  , riAllowCancelResize
  , riResizeType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types

-- | Describes a resize operation.
--
-- /See:/ 'mkResizeInfo' smart constructor.
data ResizeInfo = ResizeInfo'
  { allowCancelResize :: Core.Maybe Core.Bool
    -- ^ A boolean value indicating if the resize operation can be cancelled.
  , resizeType :: Core.Maybe Core.Text
    -- ^ Returns the value @ClassicResize@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResizeInfo' value with any optional fields omitted.
mkResizeInfo
    :: ResizeInfo
mkResizeInfo
  = ResizeInfo'{allowCancelResize = Core.Nothing,
                resizeType = Core.Nothing}

-- | A boolean value indicating if the resize operation can be cancelled.
--
-- /Note:/ Consider using 'allowCancelResize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riAllowCancelResize :: Lens.Lens' ResizeInfo (Core.Maybe Core.Bool)
riAllowCancelResize = Lens.field @"allowCancelResize"
{-# INLINEABLE riAllowCancelResize #-}
{-# DEPRECATED allowCancelResize "Use generic-lens or generic-optics with 'allowCancelResize' instead"  #-}

-- | Returns the value @ClassicResize@ .
--
-- /Note:/ Consider using 'resizeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riResizeType :: Lens.Lens' ResizeInfo (Core.Maybe Core.Text)
riResizeType = Lens.field @"resizeType"
{-# INLINEABLE riResizeType #-}
{-# DEPRECATED resizeType "Use generic-lens or generic-optics with 'resizeType' instead"  #-}

instance Core.FromXML ResizeInfo where
        parseXML x
          = ResizeInfo' Core.<$>
              (x Core..@? "AllowCancelResize") Core.<*> x Core..@? "ResizeType"
