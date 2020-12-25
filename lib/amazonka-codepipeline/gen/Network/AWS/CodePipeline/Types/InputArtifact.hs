{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.InputArtifact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.InputArtifact
  ( InputArtifact (..),

    -- * Smart constructor
    mkInputArtifact,

    -- * Lenses
    iaName,
  )
where

import qualified Network.AWS.CodePipeline.Types.Name as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents information about an artifact to be worked on, such as a test or build artifact.
--
-- /See:/ 'mkInputArtifact' smart constructor.
newtype InputArtifact = InputArtifact'
  { -- | The name of the artifact to be worked on (for example, "My App").
    --
    -- The input artifact of an action must exactly match the output artifact declared in a preceding action, but the input artifact does not have to be the next action in strict sequence from the action that provided the output artifact. Actions in parallel can declare different output artifacts, which are in turn consumed by different following actions.
    name :: Types.Name
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'InputArtifact' value with any optional fields omitted.
mkInputArtifact ::
  -- | 'name'
  Types.Name ->
  InputArtifact
mkInputArtifact name = InputArtifact' {name}

-- | The name of the artifact to be worked on (for example, "My App").
--
-- The input artifact of an action must exactly match the output artifact declared in a preceding action, but the input artifact does not have to be the next action in strict sequence from the action that provided the output artifact. Actions in parallel can declare different output artifacts, which are in turn consumed by different following actions.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaName :: Lens.Lens' InputArtifact Types.Name
iaName = Lens.field @"name"
{-# DEPRECATED iaName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON InputArtifact where
  toJSON InputArtifact {..} =
    Core.object (Core.catMaybes [Core.Just ("name" Core..= name)])

instance Core.FromJSON InputArtifact where
  parseJSON =
    Core.withObject "InputArtifact" Core.$
      \x -> InputArtifact' Core.<$> (x Core..: "name")
