{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.Counters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.Counters
  ( Counters (..),

    -- * Smart constructor
    mkCounters,

    -- * Lenses
    cPassed,
    cSkipped,
    cWarned,
    cStopped,
    cTotal,
    cFailed,
    cErrored,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents entity counters.
--
-- /See:/ 'mkCounters' smart constructor.
data Counters = Counters'
  { passed :: Lude.Maybe Lude.Int,
    skipped :: Lude.Maybe Lude.Int,
    warned :: Lude.Maybe Lude.Int,
    stopped :: Lude.Maybe Lude.Int,
    total :: Lude.Maybe Lude.Int,
    failed :: Lude.Maybe Lude.Int,
    errored :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Counters' with the minimum fields required to make a request.
--
-- * 'errored' - The number of errored entities.
-- * 'failed' - The number of failed entities.
-- * 'passed' - The number of passed entities.
-- * 'skipped' - The number of skipped entities.
-- * 'stopped' - The number of stopped entities.
-- * 'total' - The total number of entities.
-- * 'warned' - The number of warned entities.
mkCounters ::
  Counters
mkCounters =
  Counters'
    { passed = Lude.Nothing,
      skipped = Lude.Nothing,
      warned = Lude.Nothing,
      stopped = Lude.Nothing,
      total = Lude.Nothing,
      failed = Lude.Nothing,
      errored = Lude.Nothing
    }

-- | The number of passed entities.
--
-- /Note:/ Consider using 'passed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cPassed :: Lens.Lens' Counters (Lude.Maybe Lude.Int)
cPassed = Lens.lens (passed :: Counters -> Lude.Maybe Lude.Int) (\s a -> s {passed = a} :: Counters)
{-# DEPRECATED cPassed "Use generic-lens or generic-optics with 'passed' instead." #-}

-- | The number of skipped entities.
--
-- /Note:/ Consider using 'skipped' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSkipped :: Lens.Lens' Counters (Lude.Maybe Lude.Int)
cSkipped = Lens.lens (skipped :: Counters -> Lude.Maybe Lude.Int) (\s a -> s {skipped = a} :: Counters)
{-# DEPRECATED cSkipped "Use generic-lens or generic-optics with 'skipped' instead." #-}

-- | The number of warned entities.
--
-- /Note:/ Consider using 'warned' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cWarned :: Lens.Lens' Counters (Lude.Maybe Lude.Int)
cWarned = Lens.lens (warned :: Counters -> Lude.Maybe Lude.Int) (\s a -> s {warned = a} :: Counters)
{-# DEPRECATED cWarned "Use generic-lens or generic-optics with 'warned' instead." #-}

-- | The number of stopped entities.
--
-- /Note:/ Consider using 'stopped' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cStopped :: Lens.Lens' Counters (Lude.Maybe Lude.Int)
cStopped = Lens.lens (stopped :: Counters -> Lude.Maybe Lude.Int) (\s a -> s {stopped = a} :: Counters)
{-# DEPRECATED cStopped "Use generic-lens or generic-optics with 'stopped' instead." #-}

-- | The total number of entities.
--
-- /Note:/ Consider using 'total' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTotal :: Lens.Lens' Counters (Lude.Maybe Lude.Int)
cTotal = Lens.lens (total :: Counters -> Lude.Maybe Lude.Int) (\s a -> s {total = a} :: Counters)
{-# DEPRECATED cTotal "Use generic-lens or generic-optics with 'total' instead." #-}

-- | The number of failed entities.
--
-- /Note:/ Consider using 'failed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cFailed :: Lens.Lens' Counters (Lude.Maybe Lude.Int)
cFailed = Lens.lens (failed :: Counters -> Lude.Maybe Lude.Int) (\s a -> s {failed = a} :: Counters)
{-# DEPRECATED cFailed "Use generic-lens or generic-optics with 'failed' instead." #-}

-- | The number of errored entities.
--
-- /Note:/ Consider using 'errored' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cErrored :: Lens.Lens' Counters (Lude.Maybe Lude.Int)
cErrored = Lens.lens (errored :: Counters -> Lude.Maybe Lude.Int) (\s a -> s {errored = a} :: Counters)
{-# DEPRECATED cErrored "Use generic-lens or generic-optics with 'errored' instead." #-}

instance Lude.FromJSON Counters where
  parseJSON =
    Lude.withObject
      "Counters"
      ( \x ->
          Counters'
            Lude.<$> (x Lude..:? "passed")
            Lude.<*> (x Lude..:? "skipped")
            Lude.<*> (x Lude..:? "warned")
            Lude.<*> (x Lude..:? "stopped")
            Lude.<*> (x Lude..:? "total")
            Lude.<*> (x Lude..:? "failed")
            Lude.<*> (x Lude..:? "errored")
      )
