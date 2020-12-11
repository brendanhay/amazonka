-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ExecutionTrigger
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ExecutionTrigger
  ( ExecutionTrigger (..),

    -- * Smart constructor
    mkExecutionTrigger,

    -- * Lenses
    etTriggerType,
    etTriggerDetail,
  )
where

import Network.AWS.CodePipeline.Types.TriggerType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The interaction or event that started a pipeline execution.
--
-- /See:/ 'mkExecutionTrigger' smart constructor.
data ExecutionTrigger = ExecutionTrigger'
  { triggerType ::
      Lude.Maybe TriggerType,
    triggerDetail :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExecutionTrigger' with the minimum fields required to make a request.
--
-- * 'triggerDetail' - Detail related to the event that started a pipeline execution, such as the webhook ARN of the webhook that triggered the pipeline execution or the user ARN for a user-initiated @start-pipeline-execution@ CLI command.
-- * 'triggerType' - The type of change-detection method, command, or user interaction that started a pipeline execution.
mkExecutionTrigger ::
  ExecutionTrigger
mkExecutionTrigger =
  ExecutionTrigger'
    { triggerType = Lude.Nothing,
      triggerDetail = Lude.Nothing
    }

-- | The type of change-detection method, command, or user interaction that started a pipeline execution.
--
-- /Note:/ Consider using 'triggerType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etTriggerType :: Lens.Lens' ExecutionTrigger (Lude.Maybe TriggerType)
etTriggerType = Lens.lens (triggerType :: ExecutionTrigger -> Lude.Maybe TriggerType) (\s a -> s {triggerType = a} :: ExecutionTrigger)
{-# DEPRECATED etTriggerType "Use generic-lens or generic-optics with 'triggerType' instead." #-}

-- | Detail related to the event that started a pipeline execution, such as the webhook ARN of the webhook that triggered the pipeline execution or the user ARN for a user-initiated @start-pipeline-execution@ CLI command.
--
-- /Note:/ Consider using 'triggerDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etTriggerDetail :: Lens.Lens' ExecutionTrigger (Lude.Maybe Lude.Text)
etTriggerDetail = Lens.lens (triggerDetail :: ExecutionTrigger -> Lude.Maybe Lude.Text) (\s a -> s {triggerDetail = a} :: ExecutionTrigger)
{-# DEPRECATED etTriggerDetail "Use generic-lens or generic-optics with 'triggerDetail' instead." #-}

instance Lude.FromJSON ExecutionTrigger where
  parseJSON =
    Lude.withObject
      "ExecutionTrigger"
      ( \x ->
          ExecutionTrigger'
            Lude.<$> (x Lude..:? "triggerType") Lude.<*> (x Lude..:? "triggerDetail")
      )
