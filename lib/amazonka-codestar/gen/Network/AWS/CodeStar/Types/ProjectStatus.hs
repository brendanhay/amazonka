-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.Types.ProjectStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeStar.Types.ProjectStatus
  ( ProjectStatus (..),

    -- * Smart constructor
    mkProjectStatus,

    -- * Lenses
    psReason,
    psState,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An indication of whether a project creation or deletion is failed or successful.
--
-- /See:/ 'mkProjectStatus' smart constructor.
data ProjectStatus = ProjectStatus'
  { reason :: Lude.Maybe Lude.Text,
    state :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProjectStatus' with the minimum fields required to make a request.
--
-- * 'reason' - In the case of a project creation or deletion failure, a reason for the failure.
-- * 'state' - The phase of completion for a project creation or deletion.
mkProjectStatus ::
  -- | 'state'
  Lude.Text ->
  ProjectStatus
mkProjectStatus pState_ =
  ProjectStatus' {reason = Lude.Nothing, state = pState_}

-- | In the case of a project creation or deletion failure, a reason for the failure.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psReason :: Lens.Lens' ProjectStatus (Lude.Maybe Lude.Text)
psReason = Lens.lens (reason :: ProjectStatus -> Lude.Maybe Lude.Text) (\s a -> s {reason = a} :: ProjectStatus)
{-# DEPRECATED psReason "Use generic-lens or generic-optics with 'reason' instead." #-}

-- | The phase of completion for a project creation or deletion.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psState :: Lens.Lens' ProjectStatus Lude.Text
psState = Lens.lens (state :: ProjectStatus -> Lude.Text) (\s a -> s {state = a} :: ProjectStatus)
{-# DEPRECATED psState "Use generic-lens or generic-optics with 'state' instead." #-}

instance Lude.FromJSON ProjectStatus where
  parseJSON =
    Lude.withObject
      "ProjectStatus"
      ( \x ->
          ProjectStatus'
            Lude.<$> (x Lude..:? "reason") Lude.<*> (x Lude..: "state")
      )
