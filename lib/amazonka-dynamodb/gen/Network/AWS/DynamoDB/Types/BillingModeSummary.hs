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
    bmsLastUpdateToPayPerRequestDateTime,
    bmsBillingMode,
  )
where

import Network.AWS.DynamoDB.Types.BillingMode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the details for the read/write capacity mode.
--
-- /See:/ 'mkBillingModeSummary' smart constructor.
data BillingModeSummary = BillingModeSummary'
  { lastUpdateToPayPerRequestDateTime ::
      Lude.Maybe Lude.Timestamp,
    billingMode :: Lude.Maybe BillingMode
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BillingModeSummary' with the minimum fields required to make a request.
--
-- * 'billingMode' - Controls how you are charged for read and write throughput and how you manage capacity. This setting can be changed later.
--
--
--     * @PROVISIONED@ - Sets the read/write capacity mode to @PROVISIONED@ . We recommend using @PROVISIONED@ for predictable workloads.
--
--
--     * @PAY_PER_REQUEST@ - Sets the read/write capacity mode to @PAY_PER_REQUEST@ . We recommend using @PAY_PER_REQUEST@ for unpredictable workloads.
--
--
-- * 'lastUpdateToPayPerRequestDateTime' - Represents the time when @PAY_PER_REQUEST@ was last set as the read/write capacity mode.
mkBillingModeSummary ::
  BillingModeSummary
mkBillingModeSummary =
  BillingModeSummary'
    { lastUpdateToPayPerRequestDateTime =
        Lude.Nothing,
      billingMode = Lude.Nothing
    }

-- | Represents the time when @PAY_PER_REQUEST@ was last set as the read/write capacity mode.
--
-- /Note:/ Consider using 'lastUpdateToPayPerRequestDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmsLastUpdateToPayPerRequestDateTime :: Lens.Lens' BillingModeSummary (Lude.Maybe Lude.Timestamp)
bmsLastUpdateToPayPerRequestDateTime = Lens.lens (lastUpdateToPayPerRequestDateTime :: BillingModeSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdateToPayPerRequestDateTime = a} :: BillingModeSummary)
{-# DEPRECATED bmsLastUpdateToPayPerRequestDateTime "Use generic-lens or generic-optics with 'lastUpdateToPayPerRequestDateTime' instead." #-}

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
bmsBillingMode :: Lens.Lens' BillingModeSummary (Lude.Maybe BillingMode)
bmsBillingMode = Lens.lens (billingMode :: BillingModeSummary -> Lude.Maybe BillingMode) (\s a -> s {billingMode = a} :: BillingModeSummary)
{-# DEPRECATED bmsBillingMode "Use generic-lens or generic-optics with 'billingMode' instead." #-}

instance Lude.FromJSON BillingModeSummary where
  parseJSON =
    Lude.withObject
      "BillingModeSummary"
      ( \x ->
          BillingModeSummary'
            Lude.<$> (x Lude..:? "LastUpdateToPayPerRequestDateTime")
            Lude.<*> (x Lude..:? "BillingMode")
      )
