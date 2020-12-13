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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents information about an artifact to be worked on, such as a test or build artifact.
--
-- /See:/ 'mkInputArtifact' smart constructor.
newtype InputArtifact = InputArtifact'
  { -- | The name of the artifact to be worked on (for example, "My App").
    --
    -- The input artifact of an action must exactly match the output artifact declared in a preceding action, but the input artifact does not have to be the next action in strict sequence from the action that provided the output artifact. Actions in parallel can declare different output artifacts, which are in turn consumed by different following actions.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InputArtifact' with the minimum fields required to make a request.
--
-- * 'name' - The name of the artifact to be worked on (for example, "My App").
--
-- The input artifact of an action must exactly match the output artifact declared in a preceding action, but the input artifact does not have to be the next action in strict sequence from the action that provided the output artifact. Actions in parallel can declare different output artifacts, which are in turn consumed by different following actions.
mkInputArtifact ::
  -- | 'name'
  Lude.Text ->
  InputArtifact
mkInputArtifact pName_ = InputArtifact' {name = pName_}

-- | The name of the artifact to be worked on (for example, "My App").
--
-- The input artifact of an action must exactly match the output artifact declared in a preceding action, but the input artifact does not have to be the next action in strict sequence from the action that provided the output artifact. Actions in parallel can declare different output artifacts, which are in turn consumed by different following actions.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaName :: Lens.Lens' InputArtifact Lude.Text
iaName = Lens.lens (name :: InputArtifact -> Lude.Text) (\s a -> s {name = a} :: InputArtifact)
{-# DEPRECATED iaName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON InputArtifact where
  parseJSON =
    Lude.withObject
      "InputArtifact"
      (\x -> InputArtifact' Lude.<$> (x Lude..: "name"))

instance Lude.ToJSON InputArtifact where
  toJSON InputArtifact' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("name" Lude..= name)])
