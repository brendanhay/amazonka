{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.Concurrency
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.Concurrency
  ( Concurrency (..),

    -- * Smart constructor
    mkConcurrency,

    -- * Lenses
    cReservedConcurrentExecutions,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | /See:/ 'mkConcurrency' smart constructor.
newtype Concurrency = Concurrency'
  { reservedConcurrentExecutions ::
      Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Concurrency' with the minimum fields required to make a request.
--
-- * 'reservedConcurrentExecutions' - The number of concurrent executions that are reserved for this function. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/concurrent-executions.html Managing Concurrency> .
mkConcurrency ::
  Concurrency
mkConcurrency =
  Concurrency' {reservedConcurrentExecutions = Lude.Nothing}

-- | The number of concurrent executions that are reserved for this function. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/concurrent-executions.html Managing Concurrency> .
--
-- /Note:/ Consider using 'reservedConcurrentExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cReservedConcurrentExecutions :: Lens.Lens' Concurrency (Lude.Maybe Lude.Natural)
cReservedConcurrentExecutions = Lens.lens (reservedConcurrentExecutions :: Concurrency -> Lude.Maybe Lude.Natural) (\s a -> s {reservedConcurrentExecutions = a} :: Concurrency)
{-# DEPRECATED cReservedConcurrentExecutions "Use generic-lens or generic-optics with 'reservedConcurrentExecutions' instead." #-}

instance Lude.FromJSON Concurrency where
  parseJSON =
    Lude.withObject
      "Concurrency"
      ( \x ->
          Concurrency' Lude.<$> (x Lude..:? "ReservedConcurrentExecutions")
      )
