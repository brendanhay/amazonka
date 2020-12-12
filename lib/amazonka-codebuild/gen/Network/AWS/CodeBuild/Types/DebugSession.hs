{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.DebugSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.DebugSession
  ( DebugSession (..),

    -- * Smart constructor
    mkDebugSession,

    -- * Lenses
    dsSessionEnabled,
    dsSessionTarget,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the debug session for a build. For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/session-manager.html Viewing a running build in Session Manager> .
--
-- /See:/ 'mkDebugSession' smart constructor.
data DebugSession = DebugSession'
  { sessionEnabled ::
      Lude.Maybe Lude.Bool,
    sessionTarget :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DebugSession' with the minimum fields required to make a request.
--
-- * 'sessionEnabled' - Specifies if session debugging is enabled for this build.
-- * 'sessionTarget' - Contains the identifier of the Session Manager session used for the build. To work with the paused build, you open this session to examine, control, and resume the build.
mkDebugSession ::
  DebugSession
mkDebugSession =
  DebugSession'
    { sessionEnabled = Lude.Nothing,
      sessionTarget = Lude.Nothing
    }

-- | Specifies if session debugging is enabled for this build.
--
-- /Note:/ Consider using 'sessionEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsSessionEnabled :: Lens.Lens' DebugSession (Lude.Maybe Lude.Bool)
dsSessionEnabled = Lens.lens (sessionEnabled :: DebugSession -> Lude.Maybe Lude.Bool) (\s a -> s {sessionEnabled = a} :: DebugSession)
{-# DEPRECATED dsSessionEnabled "Use generic-lens or generic-optics with 'sessionEnabled' instead." #-}

-- | Contains the identifier of the Session Manager session used for the build. To work with the paused build, you open this session to examine, control, and resume the build.
--
-- /Note:/ Consider using 'sessionTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsSessionTarget :: Lens.Lens' DebugSession (Lude.Maybe Lude.Text)
dsSessionTarget = Lens.lens (sessionTarget :: DebugSession -> Lude.Maybe Lude.Text) (\s a -> s {sessionTarget = a} :: DebugSession)
{-# DEPRECATED dsSessionTarget "Use generic-lens or generic-optics with 'sessionTarget' instead." #-}

instance Lude.FromJSON DebugSession where
  parseJSON =
    Lude.withObject
      "DebugSession"
      ( \x ->
          DebugSession'
            Lude.<$> (x Lude..:? "sessionEnabled")
            Lude.<*> (x Lude..:? "sessionTarget")
      )
