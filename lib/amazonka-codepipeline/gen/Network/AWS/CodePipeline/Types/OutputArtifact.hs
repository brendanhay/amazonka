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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents information about the output of an action.
--
-- /See:/ 'mkOutputArtifact' smart constructor.
newtype OutputArtifact = OutputArtifact'
  { -- | The name of the output of an artifact, such as "My App".
    --
    -- The input artifact of an action must exactly match the output artifact declared in a preceding action, but the input artifact does not have to be the next action in strict sequence from the action that provided the output artifact. Actions in parallel can declare different output artifacts, which are in turn consumed by different following actions.
    -- Output artifact names must be unique within a pipeline.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OutputArtifact' with the minimum fields required to make a request.
--
-- * 'name' - The name of the output of an artifact, such as "My App".
--
-- The input artifact of an action must exactly match the output artifact declared in a preceding action, but the input artifact does not have to be the next action in strict sequence from the action that provided the output artifact. Actions in parallel can declare different output artifacts, which are in turn consumed by different following actions.
-- Output artifact names must be unique within a pipeline.
mkOutputArtifact ::
  -- | 'name'
  Lude.Text ->
  OutputArtifact
mkOutputArtifact pName_ = OutputArtifact' {name = pName_}

-- | The name of the output of an artifact, such as "My App".
--
-- The input artifact of an action must exactly match the output artifact declared in a preceding action, but the input artifact does not have to be the next action in strict sequence from the action that provided the output artifact. Actions in parallel can declare different output artifacts, which are in turn consumed by different following actions.
-- Output artifact names must be unique within a pipeline.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oaName :: Lens.Lens' OutputArtifact Lude.Text
oaName = Lens.lens (name :: OutputArtifact -> Lude.Text) (\s a -> s {name = a} :: OutputArtifact)
{-# DEPRECATED oaName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON OutputArtifact where
  parseJSON =
    Lude.withObject
      "OutputArtifact"
      (\x -> OutputArtifact' Lude.<$> (x Lude..: "name"))

instance Lude.ToJSON OutputArtifact where
  toJSON OutputArtifact' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("name" Lude..= name)])
