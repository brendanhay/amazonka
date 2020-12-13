{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.ExecutionProperty
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ExecutionProperty
  ( ExecutionProperty (..),

    -- * Smart constructor
    mkExecutionProperty,

    -- * Lenses
    epMaxConcurrentRuns,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An execution property of a job.
--
-- /See:/ 'mkExecutionProperty' smart constructor.
newtype ExecutionProperty = ExecutionProperty'
  { -- | The maximum number of concurrent runs allowed for the job. The default is 1. An error is returned when this threshold is reached. The maximum value you can specify is controlled by a service limit.
    maxConcurrentRuns :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExecutionProperty' with the minimum fields required to make a request.
--
-- * 'maxConcurrentRuns' - The maximum number of concurrent runs allowed for the job. The default is 1. An error is returned when this threshold is reached. The maximum value you can specify is controlled by a service limit.
mkExecutionProperty ::
  ExecutionProperty
mkExecutionProperty =
  ExecutionProperty' {maxConcurrentRuns = Lude.Nothing}

-- | The maximum number of concurrent runs allowed for the job. The default is 1. An error is returned when this threshold is reached. The maximum value you can specify is controlled by a service limit.
--
-- /Note:/ Consider using 'maxConcurrentRuns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epMaxConcurrentRuns :: Lens.Lens' ExecutionProperty (Lude.Maybe Lude.Int)
epMaxConcurrentRuns = Lens.lens (maxConcurrentRuns :: ExecutionProperty -> Lude.Maybe Lude.Int) (\s a -> s {maxConcurrentRuns = a} :: ExecutionProperty)
{-# DEPRECATED epMaxConcurrentRuns "Use generic-lens or generic-optics with 'maxConcurrentRuns' instead." #-}

instance Lude.FromJSON ExecutionProperty where
  parseJSON =
    Lude.withObject
      "ExecutionProperty"
      ( \x ->
          ExecutionProperty' Lude.<$> (x Lude..:? "MaxConcurrentRuns")
      )

instance Lude.ToJSON ExecutionProperty where
  toJSON ExecutionProperty' {..} =
    Lude.object
      ( Lude.catMaybes
          [("MaxConcurrentRuns" Lude..=) Lude.<$> maxConcurrentRuns]
      )
