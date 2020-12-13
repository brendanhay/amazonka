{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    aeffValues,
    aeffKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.AutomationExecutionFilterKey

-- | A filter used to match specific automation executions. This is used to limit the scope of Automation execution information returned.
--
-- /See:/ 'mkAutomationExecutionFilter' smart constructor.
data AutomationExecutionFilter = AutomationExecutionFilter'
  { -- | The values used to limit the execution information associated with the filter's key.
    values :: Lude.NonEmpty Lude.Text,
    -- | One or more keys to limit the results. Valid filter keys include the following: DocumentNamePrefix, ExecutionStatus, ExecutionId, ParentExecutionId, CurrentAction, StartTimeBefore, StartTimeAfter, TargetResourceGroup.
    key :: AutomationExecutionFilterKey
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AutomationExecutionFilter' with the minimum fields required to make a request.
--
-- * 'values' - The values used to limit the execution information associated with the filter's key.
-- * 'key' - One or more keys to limit the results. Valid filter keys include the following: DocumentNamePrefix, ExecutionStatus, ExecutionId, ParentExecutionId, CurrentAction, StartTimeBefore, StartTimeAfter, TargetResourceGroup.
mkAutomationExecutionFilter ::
  -- | 'values'
  Lude.NonEmpty Lude.Text ->
  -- | 'key'
  AutomationExecutionFilterKey ->
  AutomationExecutionFilter
mkAutomationExecutionFilter pValues_ pKey_ =
  AutomationExecutionFilter' {values = pValues_, key = pKey_}

-- | The values used to limit the execution information associated with the filter's key.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeffValues :: Lens.Lens' AutomationExecutionFilter (Lude.NonEmpty Lude.Text)
aeffValues = Lens.lens (values :: AutomationExecutionFilter -> Lude.NonEmpty Lude.Text) (\s a -> s {values = a} :: AutomationExecutionFilter)
{-# DEPRECATED aeffValues "Use generic-lens or generic-optics with 'values' instead." #-}

-- | One or more keys to limit the results. Valid filter keys include the following: DocumentNamePrefix, ExecutionStatus, ExecutionId, ParentExecutionId, CurrentAction, StartTimeBefore, StartTimeAfter, TargetResourceGroup.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeffKey :: Lens.Lens' AutomationExecutionFilter AutomationExecutionFilterKey
aeffKey = Lens.lens (key :: AutomationExecutionFilter -> AutomationExecutionFilterKey) (\s a -> s {key = a} :: AutomationExecutionFilter)
{-# DEPRECATED aeffKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.ToJSON AutomationExecutionFilter where
  toJSON AutomationExecutionFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Values" Lude..= values),
            Lude.Just ("Key" Lude..= key)
          ]
      )
