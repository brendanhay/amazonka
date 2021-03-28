{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.ProblemDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.ProblemDetail
  ( ProblemDetail (..)
  -- * Smart constructor
  , mkProblemDetail
  -- * Lenses
  , pdArn
  , pdName
  ) where

import qualified Network.AWS.DeviceFarm.Types.Arn as Types
import qualified Network.AWS.DeviceFarm.Types.Name as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a problem detail.
--
-- /See:/ 'mkProblemDetail' smart constructor.
data ProblemDetail = ProblemDetail'
  { arn :: Core.Maybe Types.Arn
    -- ^ The problem detail's ARN.
  , name :: Core.Maybe Types.Name
    -- ^ The problem detail's name.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProblemDetail' value with any optional fields omitted.
mkProblemDetail
    :: ProblemDetail
mkProblemDetail
  = ProblemDetail'{arn = Core.Nothing, name = Core.Nothing}

-- | The problem detail's ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdArn :: Lens.Lens' ProblemDetail (Core.Maybe Types.Arn)
pdArn = Lens.field @"arn"
{-# INLINEABLE pdArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The problem detail's name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdName :: Lens.Lens' ProblemDetail (Core.Maybe Types.Name)
pdName = Lens.field @"name"
{-# INLINEABLE pdName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON ProblemDetail where
        parseJSON
          = Core.withObject "ProblemDetail" Core.$
              \ x ->
                ProblemDetail' Core.<$>
                  (x Core..:? "arn") Core.<*> x Core..:? "name"
