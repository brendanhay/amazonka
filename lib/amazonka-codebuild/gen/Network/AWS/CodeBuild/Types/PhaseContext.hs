{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.PhaseContext
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.PhaseContext
  ( PhaseContext (..),

    -- * Smart constructor
    mkPhaseContext,

    -- * Lenses
    pcMessage,
    pcStatusCode,
  )
where

import qualified Network.AWS.CodeBuild.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Additional information about a build phase that has an error. You can use this information for troubleshooting.
--
-- /See:/ 'mkPhaseContext' smart constructor.
data PhaseContext = PhaseContext'
  { -- | An explanation of the build phase's context. This might include a command ID and an exit code.
    message :: Core.Maybe Types.String,
    -- | The status code for the context of the build phase.
    statusCode :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PhaseContext' value with any optional fields omitted.
mkPhaseContext ::
  PhaseContext
mkPhaseContext =
  PhaseContext' {message = Core.Nothing, statusCode = Core.Nothing}

-- | An explanation of the build phase's context. This might include a command ID and an exit code.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcMessage :: Lens.Lens' PhaseContext (Core.Maybe Types.String)
pcMessage = Lens.field @"message"
{-# DEPRECATED pcMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The status code for the context of the build phase.
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcStatusCode :: Lens.Lens' PhaseContext (Core.Maybe Types.String)
pcStatusCode = Lens.field @"statusCode"
{-# DEPRECATED pcStatusCode "Use generic-lens or generic-optics with 'statusCode' instead." #-}

instance Core.FromJSON PhaseContext where
  parseJSON =
    Core.withObject "PhaseContext" Core.$
      \x ->
        PhaseContext'
          Core.<$> (x Core..:? "message") Core.<*> (x Core..:? "statusCode")
