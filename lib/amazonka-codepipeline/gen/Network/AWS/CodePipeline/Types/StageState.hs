{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.StageState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.StageState
  ( StageState (..),

    -- * Smart constructor
    mkStageState,

    -- * Lenses
    ssInboundExecution,
    ssInboundTransitionState,
    ssActionStates,
    ssStageName,
    ssLatestExecution,
  )
where

import Network.AWS.CodePipeline.Types.ActionState
import Network.AWS.CodePipeline.Types.StageExecution
import Network.AWS.CodePipeline.Types.TransitionState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents information about the state of the stage.
--
-- /See:/ 'mkStageState' smart constructor.
data StageState = StageState'
  { inboundExecution :: Lude.Maybe StageExecution,
    -- | The state of the inbound transition, which is either enabled or disabled.
    inboundTransitionState :: Lude.Maybe TransitionState,
    -- | The state of the stage.
    actionStates :: Lude.Maybe [ActionState],
    -- | The name of the stage.
    stageName :: Lude.Maybe Lude.Text,
    -- | Information about the latest execution in the stage, including its ID and status.
    latestExecution :: Lude.Maybe StageExecution
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StageState' with the minimum fields required to make a request.
--
-- * 'inboundExecution' -
-- * 'inboundTransitionState' - The state of the inbound transition, which is either enabled or disabled.
-- * 'actionStates' - The state of the stage.
-- * 'stageName' - The name of the stage.
-- * 'latestExecution' - Information about the latest execution in the stage, including its ID and status.
mkStageState ::
  StageState
mkStageState =
  StageState'
    { inboundExecution = Lude.Nothing,
      inboundTransitionState = Lude.Nothing,
      actionStates = Lude.Nothing,
      stageName = Lude.Nothing,
      latestExecution = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'inboundExecution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssInboundExecution :: Lens.Lens' StageState (Lude.Maybe StageExecution)
ssInboundExecution = Lens.lens (inboundExecution :: StageState -> Lude.Maybe StageExecution) (\s a -> s {inboundExecution = a} :: StageState)
{-# DEPRECATED ssInboundExecution "Use generic-lens or generic-optics with 'inboundExecution' instead." #-}

-- | The state of the inbound transition, which is either enabled or disabled.
--
-- /Note:/ Consider using 'inboundTransitionState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssInboundTransitionState :: Lens.Lens' StageState (Lude.Maybe TransitionState)
ssInboundTransitionState = Lens.lens (inboundTransitionState :: StageState -> Lude.Maybe TransitionState) (\s a -> s {inboundTransitionState = a} :: StageState)
{-# DEPRECATED ssInboundTransitionState "Use generic-lens or generic-optics with 'inboundTransitionState' instead." #-}

-- | The state of the stage.
--
-- /Note:/ Consider using 'actionStates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssActionStates :: Lens.Lens' StageState (Lude.Maybe [ActionState])
ssActionStates = Lens.lens (actionStates :: StageState -> Lude.Maybe [ActionState]) (\s a -> s {actionStates = a} :: StageState)
{-# DEPRECATED ssActionStates "Use generic-lens or generic-optics with 'actionStates' instead." #-}

-- | The name of the stage.
--
-- /Note:/ Consider using 'stageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssStageName :: Lens.Lens' StageState (Lude.Maybe Lude.Text)
ssStageName = Lens.lens (stageName :: StageState -> Lude.Maybe Lude.Text) (\s a -> s {stageName = a} :: StageState)
{-# DEPRECATED ssStageName "Use generic-lens or generic-optics with 'stageName' instead." #-}

-- | Information about the latest execution in the stage, including its ID and status.
--
-- /Note:/ Consider using 'latestExecution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssLatestExecution :: Lens.Lens' StageState (Lude.Maybe StageExecution)
ssLatestExecution = Lens.lens (latestExecution :: StageState -> Lude.Maybe StageExecution) (\s a -> s {latestExecution = a} :: StageState)
{-# DEPRECATED ssLatestExecution "Use generic-lens or generic-optics with 'latestExecution' instead." #-}

instance Lude.FromJSON StageState where
  parseJSON =
    Lude.withObject
      "StageState"
      ( \x ->
          StageState'
            Lude.<$> (x Lude..:? "inboundExecution")
            Lude.<*> (x Lude..:? "inboundTransitionState")
            Lude.<*> (x Lude..:? "actionStates" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "stageName")
            Lude.<*> (x Lude..:? "latestExecution")
      )
