{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.OutputArtifact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.OutputArtifact
  ( OutputArtifact (..),

    -- * Smart constructor
    mkOutputArtifact,

    -- * Lenses
    oaName,
  )
where

import qualified Network.AWS.CodePipeline.Types.ArtifactName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents information about the output of an action.
--
-- /See:/ 'mkOutputArtifact' smart constructor.
newtype OutputArtifact = OutputArtifact'
  { -- | The name of the output of an artifact, such as "My App".
    --
    -- The input artifact of an action must exactly match the output artifact declared in a preceding action, but the input artifact does not have to be the next action in strict sequence from the action that provided the output artifact. Actions in parallel can declare different output artifacts, which are in turn consumed by different following actions.
    -- Output artifact names must be unique within a pipeline.
    name :: Types.ArtifactName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'OutputArtifact' value with any optional fields omitted.
mkOutputArtifact ::
  -- | 'name'
  Types.ArtifactName ->
  OutputArtifact
mkOutputArtifact name = OutputArtifact' {name}

-- | The name of the output of an artifact, such as "My App".
--
-- The input artifact of an action must exactly match the output artifact declared in a preceding action, but the input artifact does not have to be the next action in strict sequence from the action that provided the output artifact. Actions in parallel can declare different output artifacts, which are in turn consumed by different following actions.
-- Output artifact names must be unique within a pipeline.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oaName :: Lens.Lens' OutputArtifact Types.ArtifactName
oaName = Lens.field @"name"
{-# DEPRECATED oaName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON OutputArtifact where
  toJSON OutputArtifact {..} =
    Core.object (Core.catMaybes [Core.Just ("name" Core..= name)])

instance Core.FromJSON OutputArtifact where
  parseJSON =
    Core.withObject "OutputArtifact" Core.$
      \x -> OutputArtifact' Core.<$> (x Core..: "name")
