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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Additional information about a build phase that has an error. You can use this information for troubleshooting.
--
-- /See:/ 'mkPhaseContext' smart constructor.
data PhaseContext = PhaseContext'
  { -- | An explanation of the build phase's context. This might include a command ID and an exit code.
    message :: Lude.Maybe Lude.Text,
    -- | The status code for the context of the build phase.
    statusCode :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PhaseContext' with the minimum fields required to make a request.
--
-- * 'message' - An explanation of the build phase's context. This might include a command ID and an exit code.
-- * 'statusCode' - The status code for the context of the build phase.
mkPhaseContext ::
  PhaseContext
mkPhaseContext =
  PhaseContext' {message = Lude.Nothing, statusCode = Lude.Nothing}

-- | An explanation of the build phase's context. This might include a command ID and an exit code.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcMessage :: Lens.Lens' PhaseContext (Lude.Maybe Lude.Text)
pcMessage = Lens.lens (message :: PhaseContext -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: PhaseContext)
{-# DEPRECATED pcMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The status code for the context of the build phase.
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcStatusCode :: Lens.Lens' PhaseContext (Lude.Maybe Lude.Text)
pcStatusCode = Lens.lens (statusCode :: PhaseContext -> Lude.Maybe Lude.Text) (\s a -> s {statusCode = a} :: PhaseContext)
{-# DEPRECATED pcStatusCode "Use generic-lens or generic-optics with 'statusCode' instead." #-}

instance Lude.FromJSON PhaseContext where
  parseJSON =
    Lude.withObject
      "PhaseContext"
      ( \x ->
          PhaseContext'
            Lude.<$> (x Lude..:? "message") Lude.<*> (x Lude..:? "statusCode")
      )
