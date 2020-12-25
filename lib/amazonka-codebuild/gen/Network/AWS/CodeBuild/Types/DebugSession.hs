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

import qualified Network.AWS.CodeBuild.Types.NonEmptyString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the debug session for a build. For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/session-manager.html Viewing a running build in Session Manager> .
--
-- /See:/ 'mkDebugSession' smart constructor.
data DebugSession = DebugSession'
  { -- | Specifies if session debugging is enabled for this build.
    sessionEnabled :: Core.Maybe Core.Bool,
    -- | Contains the identifier of the Session Manager session used for the build. To work with the paused build, you open this session to examine, control, and resume the build.
    sessionTarget :: Core.Maybe Types.NonEmptyString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DebugSession' value with any optional fields omitted.
mkDebugSession ::
  DebugSession
mkDebugSession =
  DebugSession'
    { sessionEnabled = Core.Nothing,
      sessionTarget = Core.Nothing
    }

-- | Specifies if session debugging is enabled for this build.
--
-- /Note:/ Consider using 'sessionEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsSessionEnabled :: Lens.Lens' DebugSession (Core.Maybe Core.Bool)
dsSessionEnabled = Lens.field @"sessionEnabled"
{-# DEPRECATED dsSessionEnabled "Use generic-lens or generic-optics with 'sessionEnabled' instead." #-}

-- | Contains the identifier of the Session Manager session used for the build. To work with the paused build, you open this session to examine, control, and resume the build.
--
-- /Note:/ Consider using 'sessionTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsSessionTarget :: Lens.Lens' DebugSession (Core.Maybe Types.NonEmptyString)
dsSessionTarget = Lens.field @"sessionTarget"
{-# DEPRECATED dsSessionTarget "Use generic-lens or generic-optics with 'sessionTarget' instead." #-}

instance Core.FromJSON DebugSession where
  parseJSON =
    Core.withObject "DebugSession" Core.$
      \x ->
        DebugSession'
          Core.<$> (x Core..:? "sessionEnabled") Core.<*> (x Core..:? "sessionTarget")
