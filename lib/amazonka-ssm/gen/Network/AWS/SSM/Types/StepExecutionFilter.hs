-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.StepExecutionFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.StepExecutionFilter
  ( StepExecutionFilter (..),

    -- * Smart constructor
    mkStepExecutionFilter,

    -- * Lenses
    sefKey,
    sefValues,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.StepExecutionFilterKey

-- | A filter to limit the amount of step execution information returned by the call.
--
-- /See:/ 'mkStepExecutionFilter' smart constructor.
data StepExecutionFilter = StepExecutionFilter'
  { key ::
      StepExecutionFilterKey,
    values :: Lude.NonEmpty Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StepExecutionFilter' with the minimum fields required to make a request.
--
-- * 'key' - One or more keys to limit the results. Valid filter keys include the following: StepName, Action, StepExecutionId, StepExecutionStatus, StartTimeBefore, StartTimeAfter.
-- * 'values' - The values of the filter key.
mkStepExecutionFilter ::
  -- | 'key'
  StepExecutionFilterKey ->
  -- | 'values'
  Lude.NonEmpty Lude.Text ->
  StepExecutionFilter
mkStepExecutionFilter pKey_ pValues_ =
  StepExecutionFilter' {key = pKey_, values = pValues_}

-- | One or more keys to limit the results. Valid filter keys include the following: StepName, Action, StepExecutionId, StepExecutionStatus, StartTimeBefore, StartTimeAfter.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sefKey :: Lens.Lens' StepExecutionFilter StepExecutionFilterKey
sefKey = Lens.lens (key :: StepExecutionFilter -> StepExecutionFilterKey) (\s a -> s {key = a} :: StepExecutionFilter)
{-# DEPRECATED sefKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The values of the filter key.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sefValues :: Lens.Lens' StepExecutionFilter (Lude.NonEmpty Lude.Text)
sefValues = Lens.lens (values :: StepExecutionFilter -> Lude.NonEmpty Lude.Text) (\s a -> s {values = a} :: StepExecutionFilter)
{-# DEPRECATED sefValues "Use generic-lens or generic-optics with 'values' instead." #-}

instance Lude.ToJSON StepExecutionFilter where
  toJSON StepExecutionFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Key" Lude..= key),
            Lude.Just ("Values" Lude..= values)
          ]
      )
