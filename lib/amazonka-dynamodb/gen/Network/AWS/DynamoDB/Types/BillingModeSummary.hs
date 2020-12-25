{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.BillingModeSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.BillingModeSummary
  ( BillingModeSummary (..),

    -- * Smart constructor
    mkBillingModeSummary,

    -- * Lenses
    bmsBillingMode,
    bmsLastUpdateToPayPerRequestDateTime,
  )
where

import qualified Network.AWS.DynamoDB.Types.BillingMode as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains the details for the read/write capacity mode.
--
-- /See:/ 'mkBillingModeSummary' smart constructor.
data BillingModeSummary = BillingModeSummary'
  { -- | Controls how you are charged for read and write throughput and how you manage capacity. This setting can be changed later.
    --
    --
    --     * @PROVISIONED@ - Sets the read/write capacity mode to @PROVISIONED@ . We recommend using @PROVISIONED@ for predictable workloads.
    --
    --
    --     * @PAY_PER_REQUEST@ - Sets the read/write capacity mode to @PAY_PER_REQUEST@ . We recommend using @PAY_PER_REQUEST@ for unpredictable workloads.
    billingMode :: Core.Maybe Types.BillingMode,
    -- | Represents the time when @PAY_PER_REQUEST@ was last set as the read/write capacity mode.
    lastUpdateToPayPerRequestDateTime :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'BillingModeSummary' value with any optional fields omitted.
mkBillingModeSummary ::
  BillingModeSummary
mkBillingModeSummary =
  BillingModeSummary'
    { billingMode = Core.Nothing,
      lastUpdateToPayPerRequestDateTime = Core.Nothing
    }

-- | Controls how you are charged for read and write throughput and how you manage capacity. This setting can be changed later.
--
--
--     * @PROVISIONED@ - Sets the read/write capacity mode to @PROVISIONED@ . We recommend using @PROVISIONED@ for predictable workloads.
--
--
--     * @PAY_PER_REQUEST@ - Sets the read/write capacity mode to @PAY_PER_REQUEST@ . We recommend using @PAY_PER_REQUEST@ for unpredictable workloads.
--
--
--
-- /Note:/ Consider using 'billingMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmsBillingMode :: Lens.Lens' BillingModeSummary (Core.Maybe Types.BillingMode)
bmsBillingMode = Lens.field @"billingMode"
{-# DEPRECATED bmsBillingMode "Use generic-lens or generic-optics with 'billingMode' instead." #-}

-- | Represents the time when @PAY_PER_REQUEST@ was last set as the read/write capacity mode.
--
-- /Note:/ Consider using 'lastUpdateToPayPerRequestDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmsLastUpdateToPayPerRequestDateTime :: Lens.Lens' BillingModeSummary (Core.Maybe Core.NominalDiffTime)
bmsLastUpdateToPayPerRequestDateTime = Lens.field @"lastUpdateToPayPerRequestDateTime"
{-# DEPRECATED bmsLastUpdateToPayPerRequestDateTime "Use generic-lens or generic-optics with 'lastUpdateToPayPerRequestDateTime' instead." #-}

instance Core.FromJSON BillingModeSummary where
  parseJSON =
    Core.withObject "BillingModeSummary" Core.$
      \x ->
        BillingModeSummary'
          Core.<$> (x Core..:? "BillingMode")
          Core.<*> (x Core..:? "LastUpdateToPayPerRequestDateTime")
