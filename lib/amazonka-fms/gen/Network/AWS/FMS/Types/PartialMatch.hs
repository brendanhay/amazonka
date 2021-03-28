{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.PartialMatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.FMS.Types.PartialMatch
  ( PartialMatch (..)
  -- * Smart constructor
  , mkPartialMatch
  -- * Lenses
  , pmReference
  , pmTargetViolationReasons
  ) where

import qualified Network.AWS.FMS.Types.ReferenceRule as Types
import qualified Network.AWS.FMS.Types.TargetViolationReason as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The reference rule that partially matches the @ViolationTarget@ rule and violation reason.
--
-- /See:/ 'mkPartialMatch' smart constructor.
data PartialMatch = PartialMatch'
  { reference :: Core.Maybe Types.ReferenceRule
    -- ^ The reference rule from the master security group of the AWS Firewall Manager policy.
  , targetViolationReasons :: Core.Maybe [Types.TargetViolationReason]
    -- ^ The violation reason.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PartialMatch' value with any optional fields omitted.
mkPartialMatch
    :: PartialMatch
mkPartialMatch
  = PartialMatch'{reference = Core.Nothing,
                  targetViolationReasons = Core.Nothing}

-- | The reference rule from the master security group of the AWS Firewall Manager policy.
--
-- /Note:/ Consider using 'reference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmReference :: Lens.Lens' PartialMatch (Core.Maybe Types.ReferenceRule)
pmReference = Lens.field @"reference"
{-# INLINEABLE pmReference #-}
{-# DEPRECATED reference "Use generic-lens or generic-optics with 'reference' instead"  #-}

-- | The violation reason.
--
-- /Note:/ Consider using 'targetViolationReasons' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmTargetViolationReasons :: Lens.Lens' PartialMatch (Core.Maybe [Types.TargetViolationReason])
pmTargetViolationReasons = Lens.field @"targetViolationReasons"
{-# INLINEABLE pmTargetViolationReasons #-}
{-# DEPRECATED targetViolationReasons "Use generic-lens or generic-optics with 'targetViolationReasons' instead"  #-}

instance Core.FromJSON PartialMatch where
        parseJSON
          = Core.withObject "PartialMatch" Core.$
              \ x ->
                PartialMatch' Core.<$>
                  (x Core..:? "Reference") Core.<*>
                    x Core..:? "TargetViolationReasons"
