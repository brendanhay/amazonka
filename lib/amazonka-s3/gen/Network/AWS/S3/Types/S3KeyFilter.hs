{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.S3KeyFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.S3KeyFilter
  ( S3KeyFilter (..)
  -- * Smart constructor
  , mkS3KeyFilter
  -- * Lenses
  , skfFilterRules
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.FilterRule as Types

-- | A container for object key name prefix and suffix filtering rules.
--
-- /See:/ 'mkS3KeyFilter' smart constructor.
newtype S3KeyFilter = S3KeyFilter'
  { filterRules :: Core.Maybe [Types.FilterRule]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'S3KeyFilter' value with any optional fields omitted.
mkS3KeyFilter
    :: S3KeyFilter
mkS3KeyFilter = S3KeyFilter'{filterRules = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'filterRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skfFilterRules :: Lens.Lens' S3KeyFilter (Core.Maybe [Types.FilterRule])
skfFilterRules = Lens.field @"filterRules"
{-# INLINEABLE skfFilterRules #-}
{-# DEPRECATED filterRules "Use generic-lens or generic-optics with 'filterRules' instead"  #-}

instance Core.ToXML S3KeyFilter where
        toXML S3KeyFilter{..}
          = Core.maybe Core.mempty (Core.toXMLList "FilterRule") filterRules

instance Core.FromXML S3KeyFilter where
        parseXML x = S3KeyFilter' Core.<$> (x Core..@? "FilterRule")
