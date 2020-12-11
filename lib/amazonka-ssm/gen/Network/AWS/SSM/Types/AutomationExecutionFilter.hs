-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AutomationExecutionFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AutomationExecutionFilter
  ( AutomationExecutionFilter (..),

    -- * Smart constructor
    mkAutomationExecutionFilter,

    -- * Lenses
    autKey,
    autValues,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.AutomationExecutionFilterKey

-- | A filter used to match specific automation executions. This is used to limit the scope of Automation execution information returned.
--
-- /See:/ 'mkAutomationExecutionFilter' smart constructor.
data AutomationExecutionFilter = AutomationExecutionFilter'
  { key ::
      AutomationExecutionFilterKey,
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

-- | Creates a value of 'AutomationExecutionFilter' with the minimum fields required to make a request.
--
-- * 'key' - One or more keys to limit the results. Valid filter keys include the following: DocumentNamePrefix, ExecutionStatus, ExecutionId, ParentExecutionId, CurrentAction, StartTimeBefore, StartTimeAfter, TargetResourceGroup.
-- * 'values' - The values used to limit the execution information associated with the filter's key.
mkAutomationExecutionFilter ::
  -- | 'key'
  AutomationExecutionFilterKey ->
  -- | 'values'
  Lude.NonEmpty Lude.Text ->
  AutomationExecutionFilter
mkAutomationExecutionFilter pKey_ pValues_ =
  AutomationExecutionFilter' {key = pKey_, values = pValues_}

-- | One or more keys to limit the results. Valid filter keys include the following: DocumentNamePrefix, ExecutionStatus, ExecutionId, ParentExecutionId, CurrentAction, StartTimeBefore, StartTimeAfter, TargetResourceGroup.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
autKey :: Lens.Lens' AutomationExecutionFilter AutomationExecutionFilterKey
autKey = Lens.lens (key :: AutomationExecutionFilter -> AutomationExecutionFilterKey) (\s a -> s {key = a} :: AutomationExecutionFilter)
{-# DEPRECATED autKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The values used to limit the execution information associated with the filter's key.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
autValues :: Lens.Lens' AutomationExecutionFilter (Lude.NonEmpty Lude.Text)
autValues = Lens.lens (values :: AutomationExecutionFilter -> Lude.NonEmpty Lude.Text) (\s a -> s {values = a} :: AutomationExecutionFilter)
{-# DEPRECATED autValues "Use generic-lens or generic-optics with 'values' instead." #-}

instance Lude.ToJSON AutomationExecutionFilter where
  toJSON AutomationExecutionFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Key" Lude..= key),
            Lude.Just ("Values" Lude..= values)
          ]
      )
