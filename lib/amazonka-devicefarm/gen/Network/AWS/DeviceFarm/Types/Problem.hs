{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.Problem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.Problem
  ( Problem (..),

    -- * Smart constructor
    mkProblem,

    -- * Lenses
    pDevice,
    pTest,
    pResult,
    pRun,
    pJob,
    pMessage,
    pSuite,
  )
where

import Network.AWS.DeviceFarm.Types.Device
import Network.AWS.DeviceFarm.Types.ExecutionResult
import Network.AWS.DeviceFarm.Types.ProblemDetail
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a specific warning or failure.
--
-- /See:/ 'mkProblem' smart constructor.
data Problem = Problem'
  { device :: Lude.Maybe Device,
    test :: Lude.Maybe ProblemDetail,
    result :: Lude.Maybe ExecutionResult,
    run :: Lude.Maybe ProblemDetail,
    job :: Lude.Maybe ProblemDetail,
    message :: Lude.Maybe Lude.Text,
    suite :: Lude.Maybe ProblemDetail
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Problem' with the minimum fields required to make a request.
--
-- * 'device' - Information about the associated device.
-- * 'job' - Information about the associated job.
-- * 'message' - A message about the problem's result.
-- * 'result' - The problem's result.
--
-- Allowed values include:
--
--     * PENDING
--
--
--     * PASSED
--
--
--     * WARNED
--
--
--     * FAILED
--
--
--     * SKIPPED
--
--
--     * ERRORED
--
--
--     * STOPPED
--
--
-- * 'run' - Information about the associated run.
-- * 'suite' - Information about the associated suite.
-- * 'test' - Information about the associated test.
mkProblem ::
  Problem
mkProblem =
  Problem'
    { device = Lude.Nothing,
      test = Lude.Nothing,
      result = Lude.Nothing,
      run = Lude.Nothing,
      job = Lude.Nothing,
      message = Lude.Nothing,
      suite = Lude.Nothing
    }

-- | Information about the associated device.
--
-- /Note:/ Consider using 'device' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pDevice :: Lens.Lens' Problem (Lude.Maybe Device)
pDevice = Lens.lens (device :: Problem -> Lude.Maybe Device) (\s a -> s {device = a} :: Problem)
{-# DEPRECATED pDevice "Use generic-lens or generic-optics with 'device' instead." #-}

-- | Information about the associated test.
--
-- /Note:/ Consider using 'test' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pTest :: Lens.Lens' Problem (Lude.Maybe ProblemDetail)
pTest = Lens.lens (test :: Problem -> Lude.Maybe ProblemDetail) (\s a -> s {test = a} :: Problem)
{-# DEPRECATED pTest "Use generic-lens or generic-optics with 'test' instead." #-}

-- | The problem's result.
--
-- Allowed values include:
--
--     * PENDING
--
--
--     * PASSED
--
--
--     * WARNED
--
--
--     * FAILED
--
--
--     * SKIPPED
--
--
--     * ERRORED
--
--
--     * STOPPED
--
--
--
-- /Note:/ Consider using 'result' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pResult :: Lens.Lens' Problem (Lude.Maybe ExecutionResult)
pResult = Lens.lens (result :: Problem -> Lude.Maybe ExecutionResult) (\s a -> s {result = a} :: Problem)
{-# DEPRECATED pResult "Use generic-lens or generic-optics with 'result' instead." #-}

-- | Information about the associated run.
--
-- /Note:/ Consider using 'run' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pRun :: Lens.Lens' Problem (Lude.Maybe ProblemDetail)
pRun = Lens.lens (run :: Problem -> Lude.Maybe ProblemDetail) (\s a -> s {run = a} :: Problem)
{-# DEPRECATED pRun "Use generic-lens or generic-optics with 'run' instead." #-}

-- | Information about the associated job.
--
-- /Note:/ Consider using 'job' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pJob :: Lens.Lens' Problem (Lude.Maybe ProblemDetail)
pJob = Lens.lens (job :: Problem -> Lude.Maybe ProblemDetail) (\s a -> s {job = a} :: Problem)
{-# DEPRECATED pJob "Use generic-lens or generic-optics with 'job' instead." #-}

-- | A message about the problem's result.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pMessage :: Lens.Lens' Problem (Lude.Maybe Lude.Text)
pMessage = Lens.lens (message :: Problem -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: Problem)
{-# DEPRECATED pMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | Information about the associated suite.
--
-- /Note:/ Consider using 'suite' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pSuite :: Lens.Lens' Problem (Lude.Maybe ProblemDetail)
pSuite = Lens.lens (suite :: Problem -> Lude.Maybe ProblemDetail) (\s a -> s {suite = a} :: Problem)
{-# DEPRECATED pSuite "Use generic-lens or generic-optics with 'suite' instead." #-}

instance Lude.FromJSON Problem where
  parseJSON =
    Lude.withObject
      "Problem"
      ( \x ->
          Problem'
            Lude.<$> (x Lude..:? "device")
            Lude.<*> (x Lude..:? "test")
            Lude.<*> (x Lude..:? "result")
            Lude.<*> (x Lude..:? "run")
            Lude.<*> (x Lude..:? "job")
            Lude.<*> (x Lude..:? "message")
            Lude.<*> (x Lude..:? "suite")
      )
