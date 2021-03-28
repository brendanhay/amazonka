{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.UsageAccountResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.UsageAccountResult
  ( UsageAccountResult (..)
  -- * Smart constructor
  , mkUsageAccountResult
  -- * Lenses
  , uarAccountId
  , uarTotal
  ) where

import qualified Network.AWS.GuardDuty.Types.AccountId as Types
import qualified Network.AWS.GuardDuty.Types.Total as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information on the total of usage based on account IDs.
--
-- /See:/ 'mkUsageAccountResult' smart constructor.
data UsageAccountResult = UsageAccountResult'
  { accountId :: Core.Maybe Types.AccountId
    -- ^ The Account ID that generated usage.
  , total :: Core.Maybe Types.Total
    -- ^ Represents the total of usage for the Account ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UsageAccountResult' value with any optional fields omitted.
mkUsageAccountResult
    :: UsageAccountResult
mkUsageAccountResult
  = UsageAccountResult'{accountId = Core.Nothing,
                        total = Core.Nothing}

-- | The Account ID that generated usage.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarAccountId :: Lens.Lens' UsageAccountResult (Core.Maybe Types.AccountId)
uarAccountId = Lens.field @"accountId"
{-# INLINEABLE uarAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | Represents the total of usage for the Account ID.
--
-- /Note:/ Consider using 'total' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarTotal :: Lens.Lens' UsageAccountResult (Core.Maybe Types.Total)
uarTotal = Lens.field @"total"
{-# INLINEABLE uarTotal #-}
{-# DEPRECATED total "Use generic-lens or generic-optics with 'total' instead"  #-}

instance Core.FromJSON UsageAccountResult where
        parseJSON
          = Core.withObject "UsageAccountResult" Core.$
              \ x ->
                UsageAccountResult' Core.<$>
                  (x Core..:? "accountId") Core.<*> x Core..:? "total"
