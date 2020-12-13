{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionState
  ( ActionState (..),

    -- * Smart constructor
    mkActionState,

    -- * Lenses
    asRevisionURL,
    asEntityURL,
    asActionName,
    asCurrentRevision,
    asLatestExecution,
  )
where

import Network.AWS.CodePipeline.Types.ActionExecution
import Network.AWS.CodePipeline.Types.ActionRevision
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents information about the state of an action.
--
-- /See:/ 'mkActionState' smart constructor.
data ActionState = ActionState'
  { -- | A URL link for more information about the revision, such as a commit details page.
    revisionURL :: Lude.Maybe Lude.Text,
    -- | A URL link for more information about the state of the action, such as a deployment group details page.
    entityURL :: Lude.Maybe Lude.Text,
    -- | The name of the action.
    actionName :: Lude.Maybe Lude.Text,
    -- | Represents information about the version (or revision) of an action.
    currentRevision :: Lude.Maybe ActionRevision,
    -- | Represents information about the run of an action.
    latestExecution :: Lude.Maybe ActionExecution
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ActionState' with the minimum fields required to make a request.
--
-- * 'revisionURL' - A URL link for more information about the revision, such as a commit details page.
-- * 'entityURL' - A URL link for more information about the state of the action, such as a deployment group details page.
-- * 'actionName' - The name of the action.
-- * 'currentRevision' - Represents information about the version (or revision) of an action.
-- * 'latestExecution' - Represents information about the run of an action.
mkActionState ::
  ActionState
mkActionState =
  ActionState'
    { revisionURL = Lude.Nothing,
      entityURL = Lude.Nothing,
      actionName = Lude.Nothing,
      currentRevision = Lude.Nothing,
      latestExecution = Lude.Nothing
    }

-- | A URL link for more information about the revision, such as a commit details page.
--
-- /Note:/ Consider using 'revisionURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asRevisionURL :: Lens.Lens' ActionState (Lude.Maybe Lude.Text)
asRevisionURL = Lens.lens (revisionURL :: ActionState -> Lude.Maybe Lude.Text) (\s a -> s {revisionURL = a} :: ActionState)
{-# DEPRECATED asRevisionURL "Use generic-lens or generic-optics with 'revisionURL' instead." #-}

-- | A URL link for more information about the state of the action, such as a deployment group details page.
--
-- /Note:/ Consider using 'entityURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asEntityURL :: Lens.Lens' ActionState (Lude.Maybe Lude.Text)
asEntityURL = Lens.lens (entityURL :: ActionState -> Lude.Maybe Lude.Text) (\s a -> s {entityURL = a} :: ActionState)
{-# DEPRECATED asEntityURL "Use generic-lens or generic-optics with 'entityURL' instead." #-}

-- | The name of the action.
--
-- /Note:/ Consider using 'actionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asActionName :: Lens.Lens' ActionState (Lude.Maybe Lude.Text)
asActionName = Lens.lens (actionName :: ActionState -> Lude.Maybe Lude.Text) (\s a -> s {actionName = a} :: ActionState)
{-# DEPRECATED asActionName "Use generic-lens or generic-optics with 'actionName' instead." #-}

-- | Represents information about the version (or revision) of an action.
--
-- /Note:/ Consider using 'currentRevision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asCurrentRevision :: Lens.Lens' ActionState (Lude.Maybe ActionRevision)
asCurrentRevision = Lens.lens (currentRevision :: ActionState -> Lude.Maybe ActionRevision) (\s a -> s {currentRevision = a} :: ActionState)
{-# DEPRECATED asCurrentRevision "Use generic-lens or generic-optics with 'currentRevision' instead." #-}

-- | Represents information about the run of an action.
--
-- /Note:/ Consider using 'latestExecution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asLatestExecution :: Lens.Lens' ActionState (Lude.Maybe ActionExecution)
asLatestExecution = Lens.lens (latestExecution :: ActionState -> Lude.Maybe ActionExecution) (\s a -> s {latestExecution = a} :: ActionState)
{-# DEPRECATED asLatestExecution "Use generic-lens or generic-optics with 'latestExecution' instead." #-}

instance Lude.FromJSON ActionState where
  parseJSON =
    Lude.withObject
      "ActionState"
      ( \x ->
          ActionState'
            Lude.<$> (x Lude..:? "revisionUrl")
            Lude.<*> (x Lude..:? "entityUrl")
            Lude.<*> (x Lude..:? "actionName")
            Lude.<*> (x Lude..:? "currentRevision")
            Lude.<*> (x Lude..:? "latestExecution")
      )
