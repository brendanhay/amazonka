{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.OwnershipControls
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.OwnershipControls
  ( OwnershipControls (..)
  -- * Smart constructor
  , mkOwnershipControls
  -- * Lenses
  , ocRules
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.OwnershipControlsRule as Types

-- | The container element for a bucket's ownership controls.
--
-- /See:/ 'mkOwnershipControls' smart constructor.
newtype OwnershipControls = OwnershipControls'
  { rules :: [Types.OwnershipControlsRule]
    -- ^ The container element for an ownership control rule.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'OwnershipControls' value with any optional fields omitted.
mkOwnershipControls
    :: OwnershipControls
mkOwnershipControls = OwnershipControls'{rules = Core.mempty}

-- | The container element for an ownership control rule.
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocRules :: Lens.Lens' OwnershipControls [Types.OwnershipControlsRule]
ocRules = Lens.field @"rules"
{-# INLINEABLE ocRules #-}
{-# DEPRECATED rules "Use generic-lens or generic-optics with 'rules' instead"  #-}

instance Core.ToXML OwnershipControls where
        toXML OwnershipControls{..} = Core.toXMLList "Rule" rules

instance Core.FromXML OwnershipControls where
        parseXML x
          = OwnershipControls' Core.<$>
              (x Core..@ "Rule" Core..@! Core.mempty)
