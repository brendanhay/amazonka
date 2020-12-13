{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionExecutionResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionExecutionResult
  ( ActionExecutionResult (..),

    -- * Smart constructor
    mkActionExecutionResult,

    -- * Lenses
    aerExternalExecutionURL,
    aerExternalExecutionId,
    aerExternalExecutionSummary,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Execution result information, such as the external execution ID.
--
-- /See:/ 'mkActionExecutionResult' smart constructor.
data ActionExecutionResult = ActionExecutionResult'
  { -- | The deepest external link to the external resource (for example, a repository URL or deployment endpoint) that is used when running the action.
    externalExecutionURL :: Lude.Maybe Lude.Text,
    -- | The action provider's external ID for the action execution.
    externalExecutionId :: Lude.Maybe Lude.Text,
    -- | The action provider's summary for the action execution.
    externalExecutionSummary :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ActionExecutionResult' with the minimum fields required to make a request.
--
-- * 'externalExecutionURL' - The deepest external link to the external resource (for example, a repository URL or deployment endpoint) that is used when running the action.
-- * 'externalExecutionId' - The action provider's external ID for the action execution.
-- * 'externalExecutionSummary' - The action provider's summary for the action execution.
mkActionExecutionResult ::
  ActionExecutionResult
mkActionExecutionResult =
  ActionExecutionResult'
    { externalExecutionURL = Lude.Nothing,
      externalExecutionId = Lude.Nothing,
      externalExecutionSummary = Lude.Nothing
    }

-- | The deepest external link to the external resource (for example, a repository URL or deployment endpoint) that is used when running the action.
--
-- /Note:/ Consider using 'externalExecutionURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aerExternalExecutionURL :: Lens.Lens' ActionExecutionResult (Lude.Maybe Lude.Text)
aerExternalExecutionURL = Lens.lens (externalExecutionURL :: ActionExecutionResult -> Lude.Maybe Lude.Text) (\s a -> s {externalExecutionURL = a} :: ActionExecutionResult)
{-# DEPRECATED aerExternalExecutionURL "Use generic-lens or generic-optics with 'externalExecutionURL' instead." #-}

-- | The action provider's external ID for the action execution.
--
-- /Note:/ Consider using 'externalExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aerExternalExecutionId :: Lens.Lens' ActionExecutionResult (Lude.Maybe Lude.Text)
aerExternalExecutionId = Lens.lens (externalExecutionId :: ActionExecutionResult -> Lude.Maybe Lude.Text) (\s a -> s {externalExecutionId = a} :: ActionExecutionResult)
{-# DEPRECATED aerExternalExecutionId "Use generic-lens or generic-optics with 'externalExecutionId' instead." #-}

-- | The action provider's summary for the action execution.
--
-- /Note:/ Consider using 'externalExecutionSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aerExternalExecutionSummary :: Lens.Lens' ActionExecutionResult (Lude.Maybe Lude.Text)
aerExternalExecutionSummary = Lens.lens (externalExecutionSummary :: ActionExecutionResult -> Lude.Maybe Lude.Text) (\s a -> s {externalExecutionSummary = a} :: ActionExecutionResult)
{-# DEPRECATED aerExternalExecutionSummary "Use generic-lens or generic-optics with 'externalExecutionSummary' instead." #-}

instance Lude.FromJSON ActionExecutionResult where
  parseJSON =
    Lude.withObject
      "ActionExecutionResult"
      ( \x ->
          ActionExecutionResult'
            Lude.<$> (x Lude..:? "externalExecutionUrl")
            Lude.<*> (x Lude..:? "externalExecutionId")
            Lude.<*> (x Lude..:? "externalExecutionSummary")
      )
