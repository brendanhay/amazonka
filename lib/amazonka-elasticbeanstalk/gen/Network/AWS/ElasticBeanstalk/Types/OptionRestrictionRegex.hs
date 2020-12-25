{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.OptionRestrictionRegex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.OptionRestrictionRegex
  ( OptionRestrictionRegex (..),

    -- * Smart constructor
    mkOptionRestrictionRegex,

    -- * Lenses
    orrLabel,
    orrPattern,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types.Label as Types
import qualified Network.AWS.ElasticBeanstalk.Types.Pattern as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A regular expression representing a restriction on a string configuration option value.
--
-- /See:/ 'mkOptionRestrictionRegex' smart constructor.
data OptionRestrictionRegex = OptionRestrictionRegex'
  { -- | A unique name representing this regular expression.
    label :: Core.Maybe Types.Label,
    -- | The regular expression pattern that a string configuration option value with this restriction must match.
    pattern' :: Core.Maybe Types.Pattern
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OptionRestrictionRegex' value with any optional fields omitted.
mkOptionRestrictionRegex ::
  OptionRestrictionRegex
mkOptionRestrictionRegex =
  OptionRestrictionRegex'
    { label = Core.Nothing,
      pattern' = Core.Nothing
    }

-- | A unique name representing this regular expression.
--
-- /Note:/ Consider using 'label' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
orrLabel :: Lens.Lens' OptionRestrictionRegex (Core.Maybe Types.Label)
orrLabel = Lens.field @"label"
{-# DEPRECATED orrLabel "Use generic-lens or generic-optics with 'label' instead." #-}

-- | The regular expression pattern that a string configuration option value with this restriction must match.
--
-- /Note:/ Consider using 'pattern'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
orrPattern :: Lens.Lens' OptionRestrictionRegex (Core.Maybe Types.Pattern)
orrPattern = Lens.field @"pattern'"
{-# DEPRECATED orrPattern "Use generic-lens or generic-optics with 'pattern'' instead." #-}

instance Core.FromXML OptionRestrictionRegex where
  parseXML x =
    OptionRestrictionRegex'
      Core.<$> (x Core..@? "Label") Core.<*> (x Core..@? "Pattern")
