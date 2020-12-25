{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.IntelligentTieringAndOperator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.IntelligentTieringAndOperator
  ( IntelligentTieringAndOperator (..),

    -- * Smart constructor
    mkIntelligentTieringAndOperator,

    -- * Lenses
    itaoPrefix,
    itaoTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.Prefix as Types
import qualified Network.AWS.S3.Types.Tag as Types

-- | A container for specifying S3 Intelligent-Tiering filters. The filters determine the subset of objects to which the rule applies.
--
-- /See:/ 'mkIntelligentTieringAndOperator' smart constructor.
data IntelligentTieringAndOperator = IntelligentTieringAndOperator'
  { -- | An object key name prefix that identifies the subset of objects to which the configuration applies.
    prefix :: Core.Maybe Types.Prefix,
    -- | All of these tags must exist in the object's tag set in order for the configuration to apply.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IntelligentTieringAndOperator' value with any optional fields omitted.
mkIntelligentTieringAndOperator ::
  IntelligentTieringAndOperator
mkIntelligentTieringAndOperator =
  IntelligentTieringAndOperator'
    { prefix = Core.Nothing,
      tags = Core.Nothing
    }

-- | An object key name prefix that identifies the subset of objects to which the configuration applies.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itaoPrefix :: Lens.Lens' IntelligentTieringAndOperator (Core.Maybe Types.Prefix)
itaoPrefix = Lens.field @"prefix"
{-# DEPRECATED itaoPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | All of these tags must exist in the object's tag set in order for the configuration to apply.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itaoTags :: Lens.Lens' IntelligentTieringAndOperator (Core.Maybe [Types.Tag])
itaoTags = Lens.field @"tags"
{-# DEPRECATED itaoTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.ToXML IntelligentTieringAndOperator where
  toXML IntelligentTieringAndOperator {..} =
    Core.toXMLNode "Prefix" Core.<$> prefix
      Core.<> Core.toXMLNode "Tag" (Core.toXMLList "Tag" Core.<$> tags)

instance Core.FromXML IntelligentTieringAndOperator where
  parseXML x =
    IntelligentTieringAndOperator'
      Core.<$> (x Core..@? "Prefix")
      Core.<*> (x Core..@? "Tag" Core..<@> Core.parseXMLList "Tag")
