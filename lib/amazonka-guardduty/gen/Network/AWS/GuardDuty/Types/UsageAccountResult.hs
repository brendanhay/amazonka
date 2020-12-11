-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.UsageAccountResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.UsageAccountResult
  ( UsageAccountResult (..),

    -- * Smart constructor
    mkUsageAccountResult,

    -- * Lenses
    uarAccountId,
    uarTotal,
  )
where

import Network.AWS.GuardDuty.Types.Total
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information on the total of usage based on account IDs.
--
-- /See:/ 'mkUsageAccountResult' smart constructor.
data UsageAccountResult = UsageAccountResult'
  { accountId ::
      Lude.Maybe Lude.Text,
    total :: Lude.Maybe Total
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UsageAccountResult' with the minimum fields required to make a request.
--
-- * 'accountId' - The Account ID that generated usage.
-- * 'total' - Represents the total of usage for the Account ID.
mkUsageAccountResult ::
  UsageAccountResult
mkUsageAccountResult =
  UsageAccountResult'
    { accountId = Lude.Nothing,
      total = Lude.Nothing
    }

-- | The Account ID that generated usage.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarAccountId :: Lens.Lens' UsageAccountResult (Lude.Maybe Lude.Text)
uarAccountId = Lens.lens (accountId :: UsageAccountResult -> Lude.Maybe Lude.Text) (\s a -> s {accountId = a} :: UsageAccountResult)
{-# DEPRECATED uarAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | Represents the total of usage for the Account ID.
--
-- /Note:/ Consider using 'total' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarTotal :: Lens.Lens' UsageAccountResult (Lude.Maybe Total)
uarTotal = Lens.lens (total :: UsageAccountResult -> Lude.Maybe Total) (\s a -> s {total = a} :: UsageAccountResult)
{-# DEPRECATED uarTotal "Use generic-lens or generic-optics with 'total' instead." #-}

instance Lude.FromJSON UsageAccountResult where
  parseJSON =
    Lude.withObject
      "UsageAccountResult"
      ( \x ->
          UsageAccountResult'
            Lude.<$> (x Lude..:? "accountId") Lude.<*> (x Lude..:? "total")
      )
