{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ObjectLockRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ObjectLockRule
  ( ObjectLockRule (..),

    -- * Smart constructor
    mkObjectLockRule,

    -- * Lenses
    olrDefaultRetention,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.DefaultRetention as Types

-- | The container element for an Object Lock rule.
--
-- /See:/ 'mkObjectLockRule' smart constructor.
newtype ObjectLockRule = ObjectLockRule'
  { -- | The default retention period that you want to apply to new objects placed in the specified bucket.
    defaultRetention :: Core.Maybe Types.DefaultRetention
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ObjectLockRule' value with any optional fields omitted.
mkObjectLockRule ::
  ObjectLockRule
mkObjectLockRule = ObjectLockRule' {defaultRetention = Core.Nothing}

-- | The default retention period that you want to apply to new objects placed in the specified bucket.
--
-- /Note:/ Consider using 'defaultRetention' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
olrDefaultRetention :: Lens.Lens' ObjectLockRule (Core.Maybe Types.DefaultRetention)
olrDefaultRetention = Lens.field @"defaultRetention"
{-# DEPRECATED olrDefaultRetention "Use generic-lens or generic-optics with 'defaultRetention' instead." #-}

instance Core.ToXML ObjectLockRule where
  toXML ObjectLockRule {..} =
    Core.toXMLNode "DefaultRetention" Core.<$> defaultRetention

instance Core.FromXML ObjectLockRule where
  parseXML x =
    ObjectLockRule' Core.<$> (x Core..@? "DefaultRetention")
