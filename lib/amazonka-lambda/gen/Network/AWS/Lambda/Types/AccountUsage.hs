{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.AccountUsage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lambda.Types.AccountUsage
  ( AccountUsage (..)
  -- * Smart constructor
  , mkAccountUsage
  -- * Lenses
  , auFunctionCount
  , auTotalCodeSize
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The number of functions and amount of storage in use.
--
-- /See:/ 'mkAccountUsage' smart constructor.
data AccountUsage = AccountUsage'
  { functionCount :: Core.Maybe Core.Integer
    -- ^ The number of Lambda functions.
  , totalCodeSize :: Core.Maybe Core.Integer
    -- ^ The amount of storage space, in bytes, that's being used by deployment packages and layer archives.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AccountUsage' value with any optional fields omitted.
mkAccountUsage
    :: AccountUsage
mkAccountUsage
  = AccountUsage'{functionCount = Core.Nothing,
                  totalCodeSize = Core.Nothing}

-- | The number of Lambda functions.
--
-- /Note:/ Consider using 'functionCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
auFunctionCount :: Lens.Lens' AccountUsage (Core.Maybe Core.Integer)
auFunctionCount = Lens.field @"functionCount"
{-# INLINEABLE auFunctionCount #-}
{-# DEPRECATED functionCount "Use generic-lens or generic-optics with 'functionCount' instead"  #-}

-- | The amount of storage space, in bytes, that's being used by deployment packages and layer archives.
--
-- /Note:/ Consider using 'totalCodeSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
auTotalCodeSize :: Lens.Lens' AccountUsage (Core.Maybe Core.Integer)
auTotalCodeSize = Lens.field @"totalCodeSize"
{-# INLINEABLE auTotalCodeSize #-}
{-# DEPRECATED totalCodeSize "Use generic-lens or generic-optics with 'totalCodeSize' instead"  #-}

instance Core.FromJSON AccountUsage where
        parseJSON
          = Core.withObject "AccountUsage" Core.$
              \ x ->
                AccountUsage' Core.<$>
                  (x Core..:? "FunctionCount") Core.<*> x Core..:? "TotalCodeSize"
